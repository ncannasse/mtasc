open Expr

type vars = (string,static_flag) Hashtbl.t

type generated =
	| NotYet
	| Generating
	| Done

type context = {
	path : type_path;
	vars : vars;
	imports : (string,type_path) Hashtbl.t;
	herits : herit list;
	exprs : signature list;
	expr : expr;
	classes : (type_path , context) Hashtbl.t;
	filename : string;
	is_interface : bool;
	mutable interfaces : context list;
	mutable superclass : context option;
	mutable constructor : func option;
	mutable generated : generated;
	mutable statics : (string * eval) list;
	mutable methods : func list;
}

let empty =  {
	path = ([],"<empty>");
	vars = Hashtbl.create 0;
	imports = Hashtbl.create 0;
	herits = [];
	expr = (EBlock [],null_pos);
	classes = Hashtbl.create 0;
	filename = "<empty>";
	is_interface = false;
	interfaces = [];
	superclass = None;
	constructor = None;
	generated = NotYet;
	statics = [];
	exprs = [];
	methods = [];
}
	

let path clctx =
	clctx.path

let getclass clctx p =
	Hashtbl.find clctx.classes p

let filename clctx =
	clctx.filename

let expr clctx =
	clctx.expr

let full_exprs clctx =
	clctx.exprs

let constructor clctx =
	clctx.constructor

let superclass clctx =
	clctx.superclass

let methods clctx =
	clctx.methods

let statics clctx =
	clctx.statics

let intrinsic clctx =
	List.exists (( = ) HIntrinsic) clctx.herits

let interface clctx =
	clctx.is_interface

let resolve clctx (path,name) =
	match path with
	| [] ->
		(try
			Hashtbl.find clctx.imports name
		with
			Not_found -> [] , name)
	| _ -> path , name

let rec resolve_supervar c name =
	match c.superclass with
	| None -> assert false
	| Some c ->
		try
			if Hashtbl.find c.vars name = IsStatic then
				resolve_supervar c name
			else
				c.path
		with
			Not_found -> resolve_supervar c name


let generate_exprs h fname el =
	let imports = Hashtbl.create 0 in
	let add_class interf path herits e =
		Hashtbl.add h path {
			filename = fname;
			path = path;
			vars = Hashtbl.create 0;
			imports = imports;
			herits = herits;
			is_interface = interf;
			classes = h;
			constructor = None;
			superclass = None;
			interfaces = [];
			statics = [];
			methods = [];
			generated = NotYet;
			expr = e;
			exprs = el;
		}
	in
	let rec loop (e,p) =
		match e with
		| EClass (path,herits,e) ->
			add_class false path herits e
		| EInterface (path,herits,e) ->
			add_class true path herits e
		| EImport path ->
			Hashtbl.add imports (snd path) path
	in
	List.iter loop el

let rec generate_class_vars h gen clctx (e,p) =
	match e with
	| EVars (static_flag,public_flag,vl) ->
		List.iter (fun (name,_,vinit) ->
			Hashtbl.add clctx.vars name static_flag;
			match static_flag , vinit with
			| IsStatic, Some v ->
				clctx.statics <- (name,v) :: clctx.statics;
				generate_class_static_refs h gen clctx v
			| _ , _ -> ()
		) vl
	| EFunction f ->		
		if f.fname = "new" then
			clctx.constructor <- Some f
		else begin
			Hashtbl.add clctx.vars f.fname f.fstatic;
			clctx.methods <- f :: clctx.methods
		end
	| EBlock el ->
		List.iter (generate_class_vars h gen clctx) el
	| _ ->
		assert false

and generate_class_static_refs h gen clctx v =
	let check p =
		let clctx2 = (try Hashtbl.find h (resolve clctx p) with Not_found -> assert false) in
		if clctx2 != clctx then generate_class h gen clctx2
	in
	let rec loop (v,p) =
		match v with
		| EField (v,_)
		| EParenthesis v
		| EUnop (_,_,v) ->
			loop v
		| EArray (v1,v2) 
		| EBinop (_,v1,v2) ->
			loop v1;
			loop v2
		| EObjDecl vl ->
			List.iter (fun (_,v) -> loop v) vl
		| EArrayDecl vl ->
			List.iter loop vl
		| ECall (v,vl) ->
			List.iter loop (v :: vl)
		| EQuestion (v,v1,v2) ->
			loop v;
			loop v1;
			loop v2
		| EStatic p ->
			check p;
		| ENew (p,vl) ->
			List.iter loop vl;
			check p;
		| EConst (Name p) ->
			check ([],p)
		| EConst _ 
		| ELambda _ ->
			()
	in
	loop v

and generate_class h gen clctx =
	match clctx.generated with
	| Done -> ()
	| Generating -> prerr_endline ("Warning : loop in generation for class " ^ s_type_path clctx.path)
	| NotYet ->
		let generate_herit = function
			| HIntrinsic | HDynamic -> ()
			| HExtends path ->
				(try
					let hctx = Hashtbl.find h (resolve clctx path) in
					clctx.superclass <- Some hctx;
					generate_class h gen hctx
				with
					Not_found -> assert false)
			| HImplements path ->
				try
					let hctx = Hashtbl.find h (resolve clctx path) in
					clctx.interfaces <- hctx :: clctx.interfaces;
					generate_class h gen hctx
				with
					Not_found -> assert false
		in
		clctx.generated <- Generating;
		List.iter generate_herit clctx.herits;
		generate_class_vars h gen clctx clctx.expr;
		clctx.methods <- List.rev clctx.methods;
		clctx.statics <- List.rev clctx.statics;
		gen clctx;
		clctx.generated <- Done

let generate gen exprs =
	let h = Hashtbl.create 0 in
	Hashtbl.iter (fun fname el -> generate_exprs h fname el) exprs;
	Hashtbl.iter (fun _ cl -> generate_class h gen cl) h