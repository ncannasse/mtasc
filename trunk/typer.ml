open Expr

type type_decl =
	| Void
	| Dyn
	| Class of class_context
	| Static of class_context
	| Function of type_decl list * type_decl

and class_field = {
	f_name : string;
	f_type : type_decl;
	f_static : static_flag;
	f_public : public_flag;
	f_pos : pos;
}

and class_context = {
	path : type_path;
	name : string;
	file : string;
	interface : bool;
	dynamic : bool;
	fields : (string,class_field) Hashtbl.t;
	statics : (string,class_field) Hashtbl.t;
	mutable super : class_context;
	mutable implements : class_context list;
	mutable imports : (string,type_path) Hashtbl.t;
}

type local = {
	lt : type_decl;
	lf : int;
}

type context = {
	class_path : string list;
	files : (string,signature list) Hashtbl.t;
	classes : (type_path,class_context) Hashtbl.t;
	in_static : bool;
	in_lambda : bool;
	locals : (string,local) Hashtbl.t;
	mutable frame : int;
	mutable inumber : type_decl;
	mutable ibool : type_decl;
	mutable istring : type_decl;
	mutable returns : type_decl;
	mutable current : class_context;
	finalizers : (unit -> unit) list ref;
}


type error_msg =
	| Class_not_found of type_path
	| Class_name_mistake of type_path
	| Cannot_unify of type_decl * type_decl
	| Custom of string

exception Error of error_msg * pos
exception File_not_found of string

let verbose = ref false

let error msg p = raise (Error (msg,p))

let rec s_type_decl = function
	| Void -> "Void"
	| Dyn -> "Any"
	| Class c -> s_type_path c.path
	| Static c -> "#" ^ s_type_path c.path
	| Function ([],r) -> "Void -> " ^ s_type_decl r
	| Function (args,r) -> String.concat " -> " (List.map s_type_decl args) ^ " -> " ^ s_type_decl r

let error_msg = function
	| Class_not_found p -> "class not found : " ^ s_type_path p
	| Class_name_mistake p -> "class name mistake : should be " ^ s_type_path p
	| Cannot_unify (ta,tb) -> s_type_decl ta ^ " should be " ^ s_type_decl tb
	| Custom msg -> msg

let verbose_msg m =
	if !verbose then begin
		print_endline m;
		flush stdout;
	end

let load_class_ref = ref ((fun _ -> assert false) : context -> type_path -> pos -> class_context)
let type_function_ref = ref (fun ?lambda _ -> assert false)

let t_object ctx = !load_class_ref ctx ([],"Object") null_pos
let t_array ctx = !load_class_ref ctx ([],"Array") null_pos

let is_number ctx = function
	| Class c when c == (match ctx.inumber with Class c2 -> c2 | _ -> assert false) -> true
	| _ -> false

let is_string ctx = function
	| Class c when c == (match ctx.istring with Class c2 -> c2 | _ -> assert false) -> true
	| _ -> false

let resolve_path clctx = function
	| (_ :: _) , _ as p -> p
	| [] , n ->
		try
			Hashtbl.find clctx.imports n
		with
			Not_found -> [] , n

let t_opt ctx clctx p = function
	| None -> Dyn
	| Some t -> 
		match resolve_path clctx t with
		| [] , "Void" -> Void
		| cp ->
			Class (!load_class_ref ctx cp p)

let ret_opt ctx clctx p f =
	let rec has_return (e,p) =
		match e with
		| EVars _ 
		| EFunction _ 
		| EBreak
		| EContinue
		| EVal _
		| EReturn None
			-> false
		| EBlock el ->
			List.exists has_return el
		| EFor (el,_,_,e) ->
			List.exists has_return (e::el)
		| EForIn (e1,_,e2) ->
			has_return e1 || has_return e2
		| EIf (_,e,eo) ->
			has_return e || (match eo with None -> false | Some e -> has_return e)
		| EWhile (_,e,_) ->
			has_return e
		| ESwitch (_,cases,def) ->
			List.exists (fun (_,e) -> has_return e) cases || (match def with None -> false | Some e -> has_return e)
		| EReturn (Some _ ) ->
			true
	in
	match f.fexpr with
	| Some e when not (has_return e) -> 
		(match f.ftype with
		| None | Some ([],"Void") -> Void
		| Some cp -> error (Custom ("Missing return of type " ^ s_type_path cp)) p)
	| _ -> t_opt ctx clctx p f.ftype

let add_class_field ctx clctx fname stat pub ft p =
	let h = (match stat with IsStatic -> clctx.statics | IsMember -> clctx.fields) in
	if Hashtbl.mem h fname then error (Custom ("Field redefiniton : " ^ fname)) p;
	Hashtbl.add h fname {
		f_name = fname;
		f_type = ft;
		f_static = stat;
		f_public = pub;
		f_pos = p;
	}

let is_dynamic = function
	| Dyn | Function _ -> true
	| Void | Static _ -> false
	| Class c -> c.dynamic

let add_finalizer ctx f =
	ctx.finalizers := f :: !(ctx.finalizers)

let no_void t p =
	if t = Void then error (Custom "Void where Object expected") p

let define_local ctx name t p =
	if Hashtbl.mem ctx.locals name then error (Custom ("Local variable redefinition : " ^ name)) p;
	Hashtbl.add ctx.locals name { lt = t; lf = ctx.frame }

let new_frame ctx =
	let f = ctx.frame in
	ctx.frame <- ctx.frame + 1;
	f

let clean_frame ctx f =
	ctx.frame <- f;
	Hashtbl.iter (fun n l ->
		if l.lf > f then Hashtbl.remove ctx.locals n;
	) ctx.locals

(* check that ta >= tb *)
let rec unify ta tb p =
	match ta , tb with
	| Dyn , x | x , Dyn when x <> Void -> ()
	| Void , Void -> ()
	| Function (args1,r1) , Function (args2,r2) ->
		let rec loop a1 a2 = 
			match a1 , a2 with
			| x :: l1, y :: l2 -> unify x y p; loop l1 l2
			| _ , _ -> ()
		in
		loop args1 args2;
		unify r2 r1 p
	| Class cl1, Class cl2 ->
		let rec loop cl1 =
			if cl1 == cl2 || List.exists ((==) cl2) cl1.implements then
				()
			else if cl1.super == cl1 then
				error (Cannot_unify (ta,tb)) p
			else
				loop cl1.super
		in
		loop cl1
	| Static _, Class c when c.super == c -> ()
	| _ , _ ->
		error (Cannot_unify (ta,tb)) p

let rec resolve t fname =
	match t with
	| Void
	| Dyn
	| Function _ -> None
	| Static c -> 
		(try Some (Hashtbl.find c.statics fname) with Not_found -> if c.super == c then None else resolve (Static c.super) fname)
	| Class c -> 
		try 
			Some (Hashtbl.find c.fields fname)
		with 
			Not_found -> 
				if c.super == c then
					None 
				else 
					resolve (Class c.super) fname

and type_ident ctx name e p =
	(* local variable lookup *)
	try
		let l = Hashtbl.find ctx.locals name in
		l.lt
	with
		Not_found ->
	(* member variable lookup *)
	try
		let f = (match resolve (Class ctx.current) name with None -> raise Not_found | Some f -> f) in
		if ctx.in_static then error (Custom ("Cannot access member variable " ^ name ^" in static function")) p;
		set_eval e (EField ((EConst (Ident "this"),p),name));
		f.f_type
	with
		Not_found ->
			(* static variable lookup *)
			match resolve (Static ctx.current) name with
			| Some f -> 
				set_eval e (EField ((EStatic ctx.current.path,p),name));
				f.f_type
			| None -> 
				match resolve (Static (!load_class_ref ctx ([],"TopLevel") null_pos)) name with
				| Some f -> 
					set_eval e (EField ((EConst (Ident "_global"),p),name));			
					f.f_type
				| None -> 
					if not ctx.current.dynamic then error (Custom ("Unknown identifier " ^ name)) p;
					set_eval e (EField ((EConst (Ident "this"),p),name));
					Dyn

let type_constant ctx c e p =
	match c with
	| Int _ | Float _ -> ctx.inumber
	| String _ -> ctx.istring
	| Ident "_root" -> Class (!load_class_ref ctx ([],"MovieClip") p)
	| Ident "true" | Ident "false" -> ctx.ibool
	| Ident "null" | Ident "undefined" | Ident "_global" -> Dyn
	| Ident "this" ->
		if ctx.in_lambda then
			Dyn
		else begin
			if ctx.in_static then error (Custom "Cannot access this in static function") p;
			Class ctx.current
		end
	| Ident "super" ->
		if ctx.in_lambda then
			Dyn
		else begin
			if ctx.in_static then error (Custom "Cannot access super in static function") p;
			Class ctx.current.super
		end
	| Ident name ->
		type_ident ctx name e p
	| Name _ ->
		assert false

let rec type_binop ctx op v1 v2 p =
	let t1 = type_val ctx v1 in
	let t2 = type_val ctx v2 in
	no_void t1 (pos v1);
	no_void t2 (pos v2);
	match op with
	| OpAdd ->
		if t1 == Dyn || t2 == Dyn then
			Dyn
		else if is_number ctx t1 && is_number ctx t2 then
			ctx.inumber
		else
			ctx.istring
	| OpAnd
	| OpOr
	| OpXor
	| OpShl
	| OpShr
	| OpUShr
	| OpMod
	| OpMult | OpDiv | OpSub ->
		unify t1 ctx.inumber p;
		unify t2 ctx.inumber p;
		ctx.inumber
	| OpAssign ->
		unify t2 t1 p;
		t1
	| OpEq
	| OpNotEq
	| OpGt
	| OpGte
	| OpLt
	| OpBoolAnd
	| OpBoolOr
	| OpLte ->
		ctx.ibool
	| OpAssignOp op ->
		let t = type_binop ctx op v1 v2 p in
		unify t t1 p;
		t1

and type_val ctx ((v,p) as e) =
	match v with
	| EConst (Name c) -> type_val ctx (EStatic ([],c),p)
	| EConst c -> type_constant ctx c e p
	| EArray (v1,v2) -> 
		let t = type_val ctx v1 in
		let t2 = type_val ctx v2 in
		(match t2 with
		| Dyn -> ()
		| _ ->
			if not (is_number ctx t2) && not(is_string ctx t2) then error (Cannot_unify (t2,ctx.inumber)) (pos v2));
		if not (is_dynamic t) then error (Cannot_unify (t,Class (t_array ctx))) (pos v1);
		Dyn
	| EBinop (op,v1,v2) ->
		type_binop ctx op v1 v2 p
	| EField (v,s) -> 
		let t = type_val ctx v in
		(match resolve t s with
		| None -> 
			if not (is_dynamic t) then error (Custom (s_type_decl (match t with Static c -> Class c | _ -> t) ^ " have no " ^ (match t with Static _ -> "static " | _ -> "") ^ "field " ^ s)) p;
			Dyn
		| Some f ->
			if f.f_public = IsPrivate then (match t with Class c | Static c when c != ctx.current -> error (Custom ("Cannot access private field " ^ s)) p | _ -> ());
			f.f_type)
	| EStatic cpath ->
		let cpath = resolve_path ctx.current cpath in
		Static (!load_class_ref ctx cpath p)
	| EParenthesis v ->
		type_val ctx v
	| EObjDecl vl ->
		List.iter (fun (_,v) -> no_void (type_val ctx v) (pos v)) vl;
		Class (t_object ctx)
	| EArrayDecl vl ->
		List.iter (fun v -> no_void (type_val ctx v) (pos v)) vl;
		Class (t_array ctx)
	| ECall (v,args) ->
		let t = type_val ctx v in
		(match t with
		| Function (fargs,ret) ->
			let rec loop l1 l2 =
				match l1 , l2 with
				| [] , _
				| _ , [] -> ()
				| v :: l1 , a :: l2 ->
					unify (type_val ctx v) a (pos v);
					loop l1 l2
			in
			loop args fargs;
			ret
		| Dyn ->
			List.iter (fun v -> no_void (type_val ctx v) (pos v)) args;
			Dyn
		| _ -> 
			error (Custom ("Cannot call non-function object " ^ s_type_decl t)) (pos v));
	| EQuestion (v,v1,v2) ->
		no_void (type_val ctx v) (pos v);
		let t1 = type_val ctx v1 in
		let t2 = type_val ctx v2 in
		unify t2 t1 (pos v2);
		t1
	| EUnop (Not,_,v) ->
		no_void (type_val ctx v) (pos v);
		ctx.ibool
	| EUnop (_,_,v) ->
		unify (type_val ctx v) ctx.inumber (pos v);
		ctx.inumber
	| ENew (cpath,vl) ->
		let cl = !load_class_ref ctx (resolve_path ctx.current cpath) p in
		let args = List.map (type_val ctx) vl in
		(match resolve (Class cl) (snd cpath) with
		| None -> ()
		| Some t ->
			if t.f_public = IsPrivate && cl != ctx.current then error (Custom "Cannot call private constructor") p;
			unify (Function (args,Void)) t.f_type p);
		Class cl
 	| ELambda f ->
		!type_function_ref ~lambda:true ctx (t_object ctx) f p

let rec type_expr ctx (e,p) =
	match e with
	| EVars (_,_,vl) ->
		let vt = List.map (fun (name,t,v) -> 
			let t = t_opt ctx ctx.current p t in
			(match v with None -> () | Some v -> unify (type_val ctx v) t (pos v));
			name , t
		) vl in
		List.iter (fun (name,t) -> define_local ctx name t p) vt
	| EFunction f ->
		assert false
	| EBlock el ->
		let f = new_frame ctx in
		List.iter (type_expr ctx) el;
		clean_frame ctx f
	| EFor (inits,conds,incrs,e) ->
		let f = new_frame ctx in
		List.iter (type_expr ctx) inits;
		List.iter (fun v ->
			no_void (type_val ctx v) (pos v)
		) conds;
		List.iter (fun v ->
			ignore(type_val ctx v)
		) incrs;
		type_expr ctx e;
		clean_frame ctx f
	| EForIn (decl,v,e) ->
		let f = new_frame ctx in
		type_expr ctx decl;
		no_void (type_val ctx v) (pos v);
		type_expr ctx e;
		clean_frame ctx f
	| EIf (v,e,eo) ->
		no_void (type_val ctx v) (pos v);
		type_expr ctx e;
		(match eo with None -> () | Some e -> type_expr ctx e);
	| EWhile (v,e,_) ->
		no_void (type_val ctx v) (pos v);
		type_expr ctx e
	| ESwitch (v,cases,def) ->
		let t = type_val ctx v in
		List.iter (fun (v,e) ->
			unify (type_val ctx v) t (pos v);
			type_expr ctx e
		) cases;
		(match def with None -> () | Some e -> type_expr ctx e)
	| EReturn None ->
		if ctx.returns <> Void && ctx.returns <> Dyn then error (Custom "Return type cannot be Void") p;
	| EReturn (Some v) ->
		unify (type_val ctx v) ctx.returns (pos v)
	| EBreak
	| EContinue ->
		()
	| EVal v ->
		ignore(type_val ctx v)

let type_function ?(lambda=false) ctx clctx f p =
	match f.fexpr with
	| None -> assert false
	| Some e ->
		if not lambda && !verbose then begin
			print_endline ("Typing " ^ s_type_path clctx.path ^ "." ^ f.fname);
			flush stdout;
		end;
		let ctx = {
			ctx with
				current = clctx;
				locals = if lambda then ctx.locals else Hashtbl.create 0;
				in_static = (f.fstatic = IsStatic);
				in_lambda = lambda;
		} in
		let fr = new_frame ctx in
		ctx.returns <- ret_opt ctx clctx p f;
		let argst = List.map (fun (a,t) -> 
			let t = t_opt ctx clctx p t in
			define_local ctx a t p;
			t
		) f.fargs in
		type_expr ctx e;
		clean_frame ctx fr;
		Function (argst,ctx.returns)


let rec type_class_fields ctx clctx (e,p) =
	match e with
	| EBlock el -> List.iter (type_class_fields ctx clctx) el
	| EVars (stat,pub,vl) ->
		List.iter (fun (vname,vtype,vinit) ->
			let t = t_opt ctx clctx p vtype in
			add_class_field ctx clctx vname stat pub t p;
			match vinit with
			| None -> ()
			| Some v ->
				add_finalizer ctx (fun () -> 
					ctx.current <- clctx;
					unify (type_val ctx v) t p
				)
		) vl
	| EFunction f -> 
		let t = Function (List.map (fun (_,t) -> t_opt ctx clctx p t) f.fargs , ret_opt ctx clctx p f) in
		add_class_field ctx clctx f.fname f.fstatic f.fpublic t p;
		if f.fexpr <> None then add_finalizer ctx (fun () -> ignore(type_function ctx clctx f p));
	| _ ->
		assert false

let type_class ctx cpath herits e imports file interf =
	let old = ctx.current in
	let rec clctx = {
		path = cpath;
		name = snd cpath;
		file = file;
		interface = interf;
		dynamic = List.exists ((=) HDynamic) herits;
		fields = Hashtbl.create 0;
		statics = Hashtbl.create 0;
		super = clctx;
		implements = [];
		imports = imports;
	} in
	Hashtbl.add ctx.classes cpath clctx;
	ctx.current <- clctx;
	let rec loop = function
		| [] -> [],"Object"
		| HExtends cpath :: _ -> resolve_path clctx cpath
		| _ :: l -> loop l
	in
	let spath = loop herits in
	clctx.super <- !load_class_ref ctx spath (pos e);
	let rec loop = function
		| [] -> []
		| HImplements cpath :: l -> cpath :: loop l
		| _ :: l -> loop l
	in
	clctx.implements <- List.map (fun cpath -> 
		!load_class_ref ctx (resolve_path clctx cpath) (pos e)
	) (loop herits);
	type_class_fields ctx clctx e;
	ctx.current <- old;
	clctx

let type_file ctx req_path file el =
	let clctx = ref None in
	let imports = Hashtbl.create 0 in
	List.iter (fun (s,p) ->
		match s with
		| EClass (t,hl,e) -> 
			if t <> req_path then error (Class_name_mistake req_path) p;
			if !clctx <> None then assert false;
			clctx := Some (type_class ctx t hl e imports file false)
		| EInterface (t,hl,e) ->
			if t <> req_path then error (Class_name_mistake req_path) p;
			if !clctx <> None then assert false;
			clctx := Some (type_class ctx t hl e imports file true)
		| EImport p ->
			Hashtbl.add imports (snd p) p
	) el;
	!clctx

let load_file ctx file =	
	let rec loop = function
		| [] -> raise (File_not_found file)
		| path :: paths ->
			try
				let file = path ^ file in
				file , open_in file
			with
				_ -> loop paths
	in
	let file, ch = loop ctx.class_path in
	let expr, comments = (try
		Parser.parse (Lexing.from_channel ch) file
	with
		| exc ->
			close_in ch;
			raise exc
	) in
	close_in ch;
	Hashtbl.add ctx.files file expr;
	verbose_msg ("Parsed " ^ file);
	expr

let load_class ctx path p =
	try
		Hashtbl.find ctx.classes path
	with
		Not_found ->
			let file_name = (match fst path with
				 | [] -> snd path ^ ".as"
				 | _ -> String.concat "/" (fst path) ^ "/" ^ snd path ^ ".as")
			in
			try
				match type_file ctx path file_name (load_file ctx file_name) with
				| None -> error (Custom "Missing class definition") { pfile = file_name; pmin = 0; pmax = 0 }
				| Some c -> c
			with
				File_not_found _ -> error (Class_not_found path) p

let check_interfaces ctx () =
	Hashtbl.iter (fun _ clctx ->
		let cli = Class clctx in
		let rec loop_interf = function
			| i :: l -> loop_fields i; loop_interf l
			| [] -> ()
		and loop_fields i =
			loop_interf i.implements;
			Hashtbl.iter (fun _ f ->
				if f.f_static = IsMember then
					match resolve cli f.f_name with
					| None -> error (Custom ("Missing field " ^ f.f_name ^ " required by " ^ s_type_path i.path)) { pfile = clctx.file; pmin = 0; pmax = 0 }
					| Some f2 -> unify f2.f_type f.f_type f2.f_pos
			) i.fields
		in
		loop_interf clctx.implements;
	) ctx.classes

let create cpath = 
	let ctx = {
		current = Obj.magic();
		inumber = Obj.magic();
		ibool = Obj.magic();
		istring = Obj.magic();
		finalizers = ref [];
		class_path = cpath;
		files = Hashtbl.create 0;
		classes = Hashtbl.create 0;
		in_static = true;
		in_lambda = false;
		returns = Void;
		locals = Hashtbl.create 0;
		frame = 0;
	} in
	add_finalizer ctx (check_interfaces ctx);
	ctx.inumber <- Class (load_class ctx ([],"Number") null_pos);
	ctx.ibool <- Class (load_class ctx ([],"Boolean") null_pos);
	ctx.istring <- Class (load_class ctx ([],"String") null_pos);
	ctx

let rec finalize ctx =
	let fl = List.rev !(ctx.finalizers) in
	ctx.finalizers := [];
	match fl with
	| [] -> ()
	| _ ->
		List.iter (fun f -> f()) fl;
		finalize ctx

let exprs ctx = ctx.files

;;
load_class_ref := load_class;
type_function_ref := type_function