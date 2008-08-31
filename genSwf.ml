(*
 *  MTASC - MotionTwin ActionScript2 Compiler
 *  Copyright (c)2004 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Expr
open Swf
open ExtHashtbl
open ExtString
open ExtList

type kind = 
	| VarReg of int
	| VarStr
	| VarObj
	| VarGetSet of string

type local_ctx = {
	reg : int;
	sp : int;
}

type context =  {
	idents : (string,int) Hashtbl.t;
	ops : action DynArray.t;
	super_bindings : (type_path * string,bool) Hashtbl.t;
	locals : (string,local_ctx) Hashtbl.t;
	main : type_path option ref;
	mutable current : Class.context;
	mutable stack : int;
	mutable code_pos : int;
	mutable ident_count : int;
	mutable reg_count : int;
	mutable stack_size : int;
	mutable cur_block : expr;
	mutable breaks : (unit -> unit) list;
	mutable continue_pos : int;
	mutable opt_push : bool;
	mutable curmethod : string;
	mutable forins : int;
}

type push_style =
	| VStr of string
	| VInt of int
	| VInt32 of int32
	| VFloat of float
	| VReg of int
	| VThis
	| VNull
	| VSuper

let stack_delta = function
	| APush l -> List.length l
	| ASetReg _ -> 0
	| AAdd | ADivide | ASubtract | AMultiply | AMod | AStringAdd -> -1
	| AAnd | AOr | AXor | AShl | AShr | AAsr -> -1
	| ACompare | AGreater -> -1
	| AEval | ANot | AJump _ | AToInt | AToNumber | AToString | ATry _ | ASwap -> 0
	| ACondJump _ -> -1
	| AEqual | APhysEqual -> -1
	| ANew -> -1 (** only if 0 params **)
	| AObject | AInitArray -> 0 (** calculated outside **)
	| ASet -> -2
	| APop -> -1
	| AFunction _ | AFunction2 _ -> 1	
	| ADup -> 1
	| AWith _ -> -1
	| AObjGet -> -1
	| AObjSet -> -3
	| ALocalVar -> -1
	| ALocalAssign -> -2
	| AReturn -> -1
	| AFSCommand2 -> 0
	| AGetURL2 _ -> -2
	| ADeleteObj | AInstanceOf | ACast -> -1
	| AExtends | AImplements -> -2
	| AEnum2 | ATrace | AThrow -> -1
	| AGetTimer -> 1
	| AIncrement | ADecrement | AChr | AOrd | ARandom | ADelete | ATypeOf | ATargetPath -> 0
	| AObjCall | ACall | ANewMethod -> assert false
	| op -> failwith ("Unknown stack delta for " ^ (ActionScript.action_string (fun _ -> "") 0 op))

let enable_main = ref false
let version = ref None
let ftrace = ref None

let write ctx op =
	let write b op =
		DynArray.add ctx.ops op;
		ctx.code_pos <- ctx.code_pos + 1;
		ctx.stack_size <- ctx.stack_size + stack_delta op;
		ctx.opt_push <- b
	in
	match op with
	| APush l when ctx.opt_push ->
		(match DynArray.last ctx.ops with
		| (APush l2) as a ->
			ctx.code_pos <- ctx.code_pos - 1;
			ctx.stack_size <- ctx.stack_size - stack_delta a;
			DynArray.delete_last ctx.ops;
			write true (APush (l2 @ l))
		| _ ->
			assert false)
	| APush _ ->
		write true op
	| _ ->
		write false op

let call ctx kind n =
	let op , n = (match kind with
		| VarReg r ->
			write ctx (APush [PReg r;PUndefined]);
			AObjCall , n + 2
		| VarStr -> 
			ACall , n + 1
		| VarObj ->
			AObjCall , n + 2
		| VarGetSet s ->
			assert false
	) in
	DynArray.add ctx.ops op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let new_call ctx kind n  =
	let op , n = (match kind with
		| VarReg r ->
			write ctx (APush [PReg r;PUndefined]);
			ANewMethod , n + 2
		| VarStr -> 
			ANew , n + 1
		| VarObj ->
			ANewMethod , n + 2
		| VarGetSet _ ->
			assert false
	) in
	DynArray.add ctx.ops op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let push ctx items =
	write ctx (APush (List.map (fun i ->
		match i with
		| VStr str ->
			let n = (try
				Hashtbl.find ctx.idents str
			with Not_found ->
				let n = ctx.ident_count in
				ctx.ident_count <- n + 1;
				Hashtbl.add ctx.idents str n;
				n
			) in
			if n <= 0xFF then 
				PStack n
			else
				PStack2 n
		| VInt n ->
			PInt (Int32.of_int n)
		| VInt32 n ->
			PInt n
		| VFloat f ->
			PDouble f
		| VThis ->
			PReg 1
		| VNull ->
			PNull
		| VSuper ->
			PReg 2
		| VReg n ->
			PReg n
	) items))

let rec pop ctx n =
	if n > 0 then begin
		write ctx APop;
		pop ctx (n-1);
	end

let cjmp ctx =
	write ctx (ACondJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.ops - 1 in
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		DynArray.set ctx.ops op_pos (ACondJump delta);
		ctx.opt_push <- false
	)

let jmp ctx =
	write ctx (AJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.ops - 1 in
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		DynArray.set ctx.ops op_pos (AJump delta);
		ctx.opt_push <- false
	)

let error p =
	raise (Typer.Error (Typer.Custom "Malformed expression",p))

let do_jmp ctx pos =
	write ctx (AJump (pos - (ctx.code_pos + 1)))

let func ctx args constructor arguments =
	let default_flags = ThisRegister :: (if arguments then [] else [ArgumentsNoVar]) in
	let f = {
		f2_name = "";
		f2_args = args;
		f2_codelen = 0;
		f2_nregs = 0;
		f2_flags = (if constructor then SuperRegister :: default_flags else SuperNoVar :: default_flags);
	} in
	write ctx (AFunction2 f);
	let start_pos = ctx.code_pos in
	(fun nregs ->
		let delta = ctx.code_pos - start_pos in
		f.f2_codelen <- delta;
		f.f2_nregs <- nregs
	)

let setvar ?(retval=false) ctx = function
	| VarReg (-1) -> assert false (** true, false, null **)
	| VarReg n -> write ctx (ASetReg n); if not retval then write ctx APop
	| VarStr
	| VarObj as s -> 
		if retval then write ctx (ASetReg 0);
		write ctx (if s = VarStr then ASet else AObjSet);
		if retval then push ctx [VReg 0]
	| VarGetSet f ->
		push ctx [VInt 1; VSuper; VStr ("__set__" ^ f)];
		call ctx VarObj 1

let getvar ctx = function
	| VarReg (-1) -> () (** true, false, null **)
	| VarReg n -> push ctx [VReg n]
	| VarStr -> write ctx AEval
	| VarObj -> write ctx AObjGet
	| VarGetSet f ->
		push ctx [VInt 0; VSuper; VStr ("__get__" ^ f)];
		call ctx VarObj 0

let clean_stack ctx stack =
	Hashtbl.iter (fun name r ->
		if r.sp > stack then Hashtbl.remove ctx.locals name		
	) ctx.locals;
	ctx.stack <- stack	

let open_block ctx e = 
	let old_block = ctx.cur_block in
	let old_stack = ctx.stack in
	let start_size = ctx.stack_size in
	ctx.stack <- ctx.stack + 1;
	ctx.cur_block <- e;
	(fun() ->
		clean_stack ctx old_stack;
		pop ctx (ctx.stack_size - start_size);
		ctx.cur_block <- old_block
	)

let rec used_in_block curblock vname e =
	let in_lambda = ref curblock in
	let rec vloop (v,p) =
		match v with
		| EConst c ->
			(match c with
			| Ident v -> !in_lambda && v = vname
			| _ -> false)
		| ECast (v1,v2) 
		| EArray (v1,v2) ->
			vloop v1 || vloop v2
		| EBinop (_,v1,v2) ->
			vloop v1 || vloop v2
		| EField (v,_) ->
			vloop v
		| EStatic (["__With"],v) ->
			v = vname
		| EStatic _ ->
			false
		| EParenthesis v ->
			vloop v
		| EObjDecl decls -> 
			List.exists (fun (_,v) -> vloop v) decls
		| EArrayDecl vl ->
			List.exists vloop vl
		| ECall (v,vl) ->
			List.exists vloop (v :: vl)
		| ENew (v,vl) ->
			vloop v || List.exists vloop vl
		| EUnop (_,_,v) ->
			vloop v
		| EQuestion (v,v1,v2) ->
			vloop v || vloop v1 || vloop v2
		| ELambda f ->
			match f.fexpr with
			| None -> false
			| Some e ->
				let old = !in_lambda in
				in_lambda := true;
				let r = loop e in
				in_lambda := old;
				r
	and loop (e,p) =
		match e with
		| EFunction _ ->
			assert false
		| EVars (_,_,vl) ->
			List.exists (fun (_,_,v) -> 
				match v with
				| None -> false
				| Some v -> vloop v
			) vl
		| EBlock el ->
			List.exists loop el
		| EFor (el,conds,incrs,e) ->
			List.exists loop el || List.exists vloop conds || List.exists vloop incrs || loop e
		| EForIn (decl,v,e) ->
			loop decl || vloop v || loop e
		| EIf (v,e,eopt) ->
			vloop v || loop e || (match eopt with None -> false | Some e -> loop e)
		| EWhile (v,e,_) ->
			vloop v || loop e
		| ESwitch (v,cases) ->
			vloop v || List.exists (fun (v,e) -> (match v with None -> false | Some v -> vloop v) || loop e) cases
		| ETry (e,cl,fopt) ->
			loop e || List.exists (fun (n,_,e) -> vname = n || loop e) !cl || (match fopt with None -> false | Some e -> loop e)
		| EWith (v,e) ->
			vloop v || loop e
		| EReturn (Some v) ->
			vloop v
		| EVal v ->
			vloop v
		| EReturn None
		| EBreak
		| EContinue ->
			false
	in
	loop e

let super_binding_ident path fname =
	(match fst path with
	| [] -> ""
	| l -> String.concat "_" l ^ "_") ^ snd path ^ "_" ^ fname

let generate_package ?(fast=false) ctx l =
	let fast = fast && (match l with
		| [] -> true
		| p :: _ -> not (Hashtbl.mem ctx.locals p)
	) in
	if fast then begin
		match l with
		| [] -> VarStr
		| p :: l ->		
			push ctx [VStr p];
			write ctx AEval;
			List.iter (fun p -> push ctx [VStr p]; write ctx AObjGet) l;
			VarObj
	end else begin
		push ctx [VStr "_global"];
		write ctx AEval;
		List.iter (fun p ->
			push ctx [VStr p];
			write ctx AObjGet;
		) l;
		VarObj
	end

let rec generate_package_register ctx = function
	| [] -> ()
	| p :: [] ->
		ignore(generate_package ~fast:true ctx (p :: []));
		write ctx ANot;
		write ctx ANot;
		let j = cjmp ctx in
		push ctx [VStr "_global"];
		write ctx AEval;
		push ctx [VStr p; VInt 0; VStr "Object"];
		write ctx ANew;
		write ctx AObjSet;
		j()
	| p :: l ->
		let lrev = List.rev l in
		let all_but_last , last = List.rev (List.tl lrev), List.hd lrev in
		generate_package_register ctx (p :: all_but_last);
		ignore(generate_package ~fast:true ctx (p :: l));
		write ctx ANot;
		write ctx ANot;
		let j = cjmp ctx in
		push ctx [VStr "_global"];
		write ctx AEval;
		List.iter (fun p -> push ctx [VStr p]; write ctx AObjGet) (p :: all_but_last);
		push ctx [VStr last; VInt 0; VStr "Object"];
		write ctx ANew;
		write ctx AObjSet;
		j()

let generate_ident ctx s p =
	match s with
	| "this" ->
		VarReg 1
	| "undefined" ->
		write ctx (APush [PUndefined]);
		VarReg (-1)
	| "null" ->
		push ctx [VNull];
		VarReg (-1)
	| "true" ->
		write ctx (APush [PBool true]);
		VarReg (-1)
	| "false" ->
		write ctx (APush [PBool false]);
		VarReg (-1)
	| "_global" | "_root" | "arguments" ->
		push ctx [VStr s];
		VarStr
	| "super" -> 
		assert false
	| _ ->
		try
			let l = Hashtbl.find ctx.locals s in
			if l.reg = 0 then begin
				push ctx [VStr s];
				VarStr
			end else
				VarReg l.reg
		with Not_found ->
			push ctx [VStr s];
			VarStr

let unescape_chars s p = 
	let b = Buffer.create 0 in
	let rec loop esc i =
		if i = String.length s then
			()
		else
			let c = s.[i] in
			if esc then begin
				let inext = ref (i + 1) in
				(match c with
				| 'b' -> Buffer.add_char b '\b'
				| 'f' -> Buffer.add_char b (char_of_int 12)
				| 'n' -> Buffer.add_char b '\n'
				| 'r' -> Buffer.add_char b '\r'
				| 't' -> Buffer.add_char b '\t'
				| '"' | '\'' | '\\' -> Buffer.add_char b c
				| '0'..'3' ->
					let c = (try
						char_of_int (int_of_string ("0o" ^ String.sub s i 3))
					with _ ->
						raise (Lexer.Error (Lexer.Invalid_character c,p))
					) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| 'x' ->
					let c = (try
						char_of_int (int_of_string ("0x" ^ String.sub s (i+1) 2))
					with _ ->
						raise (Lexer.Error (Lexer.Invalid_character c,p))
					) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| 'u' ->
					let i = (try
						int_of_string ("0x" ^ String.sub s (i+1) 4)
					with _ ->
						raise (Lexer.Error (Lexer.Invalid_character c,p))
					) in
					let ub = UTF8.Buf.create 0 in
					UTF8.Buf.add_char ub (UChar.chr i);
					Buffer.add_string b (UTF8.Buf.contents ub);
					inext := !inext + 4
				| _ -> raise (Lexer.Error (Lexer.Invalid_character c,p)));
				loop false !inext;
			end else
				match c with
				| '\\' -> loop true (i + 1)
				| c ->
					Buffer.add_char b c;
					loop false (i + 1)
	in
	loop false 0;
	Buffer.contents b

let rec generate_constant ctx p = function
	| Int str -> (try push ctx [VInt32 (Int32.of_string str)] with _ -> generate_constant ctx p (Float str))
	| Float str -> push ctx [VFloat (try float_of_string str with _ -> error p)]
	| String s -> push ctx [VStr (unescape_chars s p)]
	| Ident s -> assert false

let generate_breaks ctx olds =
	List.iter (fun f -> f()) ctx.breaks;
	ctx.breaks <- olds

let generate_function_ref = ref (fun _ _ -> assert false)

let rec generate_access ?(forcall=false) ctx (v,p) =
	match v with		
	| EConst (Ident "super") ->
		(* for superconstructor *)
		if forcall then begin
			push ctx [VSuper];
			write ctx (APush [PUndefined]);
			VarObj
		end else
			VarReg 2
	| EConst (Ident s) ->
		generate_ident ctx s p
	| EField ((EConst (Ident "super"),_),s) when Class.is_getset ctx.current s ->
		VarGetSet s
	| EField (v,s) ->
		generate_val ctx v;
		push ctx [VStr s];
		VarObj
	| EStatic (["__With"],s) ->
		push ctx [VStr s];
		VarStr
	| EStatic (p,s) ->
		let k = generate_package ~fast:true ctx p in
		push ctx [VStr s];
		k
	| EArray (va,vb) ->
		generate_val ctx va;
		generate_val ctx vb;
		VarObj
	| _ ->
		if not forcall then error p;
		generate_val ctx (v,p);
		write ctx (APush [PUndefined]);
		VarObj

and generate_binop retval ctx op v1 v2 =
	let gen a =
		generate_val ctx v1;
		generate_val ctx v2;
		write ctx a
	in
	match op with
	| OpAssign ->
		let k = generate_access ctx v1 in
		generate_val ctx v2;
		setvar ~retval ctx k
	| OpAssignOp op ->
		let k = generate_access ctx v1 in
		generate_binop true ctx op v1 v2;
		setvar ~retval ctx k
	| OpAdd -> gen AAdd
	| OpMult -> gen AMultiply
	| OpDiv -> gen ADivide
	| OpSub -> gen ASubtract
	| OpEq -> gen AEqual
	| OpPhysEq -> gen APhysEqual
	| OpPhysNotEq ->
		gen APhysEqual;
		write ctx ANot
	| OpNotEq -> 
		gen AEqual;
		write ctx ANot
	| OpGt -> gen AGreater
	| OpGte ->
		gen ACompare;
		write ctx ANot
	| OpLt -> gen ACompare
	| OpLte ->
		gen AGreater;
		write ctx ANot
	| OpAnd -> gen AAnd
	| OpOr -> gen AOr
	| OpXor -> gen AXor
	| OpBoolAnd ->
		generate_val ctx v1;
		write ctx ADup;
		write ctx ANot;
		let jump_end = cjmp ctx in
		write ctx APop;
		generate_val ctx v2;
		jump_end()
	| OpBoolOr ->
		generate_val ctx v1;
		write ctx ADup;
		let jump_end = cjmp ctx in
		write ctx APop;
		generate_val ctx v2;
		jump_end()
	| OpShl -> gen AShl
	| OpShr -> gen AShr
	| OpUShr -> gen AAsr
	| OpMod -> gen AMod

and generate_geturl ctx c vars p =
	let k = match vars with
		| [v] when c = "getURL" ->
			generate_val ctx v;
			push ctx [VStr "_self"];
			0
		| v1 :: v2 :: l -> 
			generate_val ctx v1;
			generate_val ctx v2;
			(match l with
			| [] -> 0
			| [EConst (String "GET"),_] -> 1
			| [EConst (String "POST"),_] -> 2
			| (_,p) :: [] -> error p
			| _ -> error p)
		| _  -> error p
	in
	write ctx (AGetURL2 (k + (match c with "getURL" -> 0 | "loadMovie" -> 64 | "loadVariables" -> 192 | _ -> assert false)))

and generate_call ?(newcall=false) ctx v vl =
	match fst v , vl with
	| EConst (Ident "trace") , args ->
		(match !ftrace with
		| None ->
			(match args with
			| [v] ->
				generate_val ctx v;
				write ctx ATrace
			| _ -> error (pos v))
		| Some "" | Some "no" ->
			()
		| Some f ->
			let rec loop f =
				try
					let p , f = String.split f "." in
					let p2 , f = loop f in
					p :: p2 , f
				with
					Invalid_string -> [] , f
			in
			let p , f = loop f in
			let pos = snd v in
			let e = EStatic (p,f) , pos in
			let line = Lexer.get_error_line pos in
			generate_call ctx e 
					(args @ [
						(EConst (String (s_type_path (Class.path ctx.current) ^ "::" ^ ctx.curmethod))) , pos;
						(EConst (String (String.concat "\\\\" (String.nsplit pos.pfile "\\")))) , pos;
						(EConst (Int (string_of_int line))) , pos
					]))
	| EConst (Ident "instanceof") , [v1;v2] ->
		generate_val ctx v1;
		generate_val ctx v2;
		write ctx AInstanceOf
	| EConst (Ident "typeof") , [v] ->
		generate_val ctx v;
		write ctx ATypeOf;
	| EConst (Ident "chr") , [v] ->
		generate_val ctx v;
		write ctx AChr;
	| EConst (Ident "ord") , [v] ->
		generate_val ctx v;
		write ctx AOrd;
	| EConst (Ident "int") , [v] ->
		generate_val ctx v;
		write ctx AToInt
	| EConst (Ident "string") , [v] ->
		generate_val ctx v;
		write ctx AToString
	| EConst (Ident "random") , [v] ->
		generate_val ctx v;
		write ctx ARandom
	| EConst (Ident "delete") , [v] ->
		let v = (match v with EParenthesis v , _ -> v | _ -> v) in
		(match generate_access ctx v with
		| VarObj -> write ctx ADeleteObj
		| VarReg n when n <> -1 -> ()
		| _ -> write ctx ADelete)
	| EConst (Ident "throw") , [v] ->
		generate_val ctx v;
		write ctx AThrow
	| EConst (Ident "eval") , [v] ->
		generate_val ctx v;
		write ctx AEval
	| EConst (Ident "getTimer"), [] ->
		write ctx AGetTimer
	| EConst (Ident "targetPath") , [v] ->
		generate_val ctx v;
		write ctx ATargetPath
	| EConst (Ident "FSCommand2") , l ->
		List.iter (generate_val ctx) (List.rev l);		
		let nargs = List.length l in
		push ctx [VInt nargs];
		write ctx AFSCommand2;
		ctx.stack <- ctx.stack - nargs
	| EConst (Ident "fscommand") , [v] ->
		push ctx [VStr "FSCommand:"];
		generate_val ctx v;
		write ctx AStringAdd;
		push ctx [VStr ""];
		write ctx (AGetURL2 0)
	| EConst (Ident "fscommand") , [v1;v2] ->
		push ctx [VStr "FSCommand:"];
		generate_val ctx v1;
		write ctx AStringAdd;
		generate_val ctx v2;
		write ctx (AGetURL2 0)
	| EConst (Ident "print") , [v1;v2] ->
		let str = (match fst v2 with 
			| EConst (String "bmovie") -> "print:"
			| EConst (String "bframe") -> "print:#bframe"
			| EConst (String "bmax") -> "print:#bmax"
			| _ -> 	raise (Typer.Error (Typer.Custom "print parameter should be either bmovie, bframe or bmax",pos v2))
		) in
		push ctx [VStr str];
		generate_val ctx v1;
		write ctx (AGetURL2 0)
	| EConst (Ident ("getURL" as x)) , params
	| EConst (Ident ("loadMovie" as x)) , params
	| EConst (Ident ("loadVariables" as x)) , params ->
		generate_geturl ctx x params (pos v)
	| EField ((EConst (Ident "super"),_),fname) , args ->
		let nargs = List.length args in
		List.iter (generate_val ctx) (List.rev args);
		push ctx [VInt nargs; VSuper; VStr fname];		
		call ctx VarObj nargs;
	| EConst (Ident "getVersion") , _ ->
		push ctx [VStr "/:$version"];
		write ctx AEval;
	| EConst (Ident "stopAllSounds"), [] ->
		write ctx AStopSounds
	| _ , _ ->
		let nargs = List.length vl in
		List.iter (generate_val ctx) (List.rev vl);
		push ctx [VInt nargs];
		let k = generate_access ~forcall:true ctx v in
		if newcall then
			new_call ctx k nargs
		else
			call ctx k nargs

and generate_val ?(retval=true) ctx (v,p) =
	match v with
	| EConst (Ident _)
	| EArray _
	| EField _
	| EStatic _ ->
		let k = generate_access ctx (v,p) in
		getvar ctx k
	| EConst c ->
		generate_constant ctx p c
	| EParenthesis v ->
		generate_val ~retval ctx v
	| ECast ((EStatic ([],"String"),_),v) ->
		generate_val ctx v;
		write ctx AToString
	| ECast ((EStatic ([],"Number"),_),v) ->
		generate_val ctx v;
		write ctx AToNumber;
	| ECast ((EStatic ([],"Boolean"),_),v) ->
		generate_val ctx v;
		write ctx ANot;
		write ctx ANot;
	| ECast (_,v) when !version = Some 6 ->
		generate_val ctx v
	| ECast (v1,v2) ->
		generate_val ctx v1;
		generate_val ctx v2;
		write ctx ACast
	| EQuestion (v,v1,v2) ->
		generate_val ctx v;		
		let jump_else = cjmp ctx in
		generate_val ctx v2;
		let jump_end = jmp ctx in
		jump_else();
		generate_val ctx v1;
		jump_end();
		ctx.stack_size <- ctx.stack_size - 1;
	| EBinop (op,v1,v2) ->
		generate_binop retval ctx op v1 v2
	| ELambda f ->
		!generate_function_ref ctx f
	| ECall (v,vl) ->
		generate_call ctx v vl
	| EObjDecl fields ->
		let nfields = List.length fields in
		List.iter (fun (s,v) ->
			push ctx [VStr s];
			generate_val ctx v
		) fields;
		push ctx [VInt nfields];
		write ctx AObject;
		ctx.stack_size <- ctx.stack_size - (nfields * 2);
	| EArrayDecl vl ->
		let nfields = List.length vl in
		List.iter (generate_val ctx) (List.rev vl);
		push ctx [VInt nfields];
		write ctx AInitArray;
		ctx.stack_size <- ctx.stack_size - nfields;
	| ENew (v,args) ->
		generate_call ~newcall:true ctx v args
	| EUnop (Not,_,v) -> 
		generate_val ctx v;
		write ctx ANot
	| EUnop (Neg,x,(EConst (Int s),p2)) ->
		(try
			push ctx [VInt32 (Int32.neg (Int32.of_string s))]
		with
			_ -> generate_val ctx (EUnop (Neg,x,(EConst (Float s),p2)),p))
	| EUnop (Neg,_,(EConst (Float f),p)) ->
		push ctx [VFloat (0. -. (try float_of_string f with _ -> error p))]
	| EUnop (Neg,_,v) ->
		push ctx [VInt 0];
		generate_val ctx v;
		write ctx ASubtract
	| EUnop (NegBits,_,v) ->
		generate_val ctx v;
		push ctx [VInt (-1)]; 
		write ctx AXor
	| EUnop (op,flag,v) ->
		if retval && flag = Postfix then begin
			let k = generate_access ctx v in
			getvar ctx k
		end;
		ignore(generate_access ctx v);
		let k = generate_access ctx v in
		getvar ctx k;
		write ctx (match op with Increment -> AIncrement | Decrement -> ADecrement | _ -> assert false);
		setvar ~retval:(retval && flag = Prefix) ctx k

let generate_local_var ctx (vname,_,vinit) =
	if used_in_block false vname ctx.cur_block || ctx.reg_count >= 250 then begin
		push ctx [VStr vname];
		Hashtbl.add ctx.locals vname { reg = 0; sp = ctx.stack };
		match vinit with
		| None -> write ctx ALocalVar
		| Some v ->
			generate_val ctx v;
			write ctx ALocalAssign
	end else begin
		ctx.reg_count <- ctx.reg_count + 1;
		let r = ctx.reg_count in
		Hashtbl.add ctx.locals vname { reg = r; sp = ctx.stack };
		match vinit with
		| None -> 
			()
		| Some v ->
			generate_val ctx v;
			setvar ctx (VarReg r)
	end

let gen_forins ctx all =
	for i = 1 to (if all then ctx.forins else 1) do
		push ctx [VNull];
		write ctx AEqual;
		write ctx ANot;
		write ctx (ACondJump (-4));
	done

let rec generate_expr ctx (e,p) =
	match e with
	| EFunction _ ->
		assert false
	| EVars (_,_,vl) ->
		List.iter (generate_local_var ctx) vl
	| EBlock el ->
		let block_end = open_block ctx (e,p) in
		List.iter (generate_expr ctx) el;
		block_end()
	| EFor (inits,conds,incrs,e) ->
		let block_end = open_block ctx e in
		List.iter (generate_expr ctx) inits;
		let test = jmp ctx in
		let start_pos = ctx.code_pos in
		let old_continue = ctx.continue_pos in
		let old_breaks = ctx.breaks in
		ctx.breaks <- [];
		ctx.continue_pos <- start_pos;
		ctx.opt_push <- false;
		List.iter (fun v -> generate_expr ctx (EVal v,null_pos)) incrs;
		test();
		let jumps = ref [] in
		List.iter (fun cond -> 
			generate_val ctx cond;
			write ctx ANot;
			jumps := cjmp ctx :: !jumps;
		) conds;
		generate_expr ctx e;
		do_jmp ctx start_pos;
		List.iter (fun j -> j()) !jumps;
		generate_breaks ctx old_breaks;
		ctx.continue_pos <- old_continue;
		block_end()
	| EForIn (decl,v,e) ->
		let block_end = open_block ctx e in
		generate_val ctx v;
		write ctx AEnum2;
		let start_pos = ctx.code_pos in
		let old_continue = ctx.continue_pos in
		let old_breaks = ctx.breaks in
		ctx.breaks <- [];
		ctx.continue_pos <- start_pos;
		ctx.opt_push <- false;
		ctx.forins <- ctx.forins + 1;
		write ctx (ASetReg 0);
		push ctx [VNull];
		write ctx AEqual;
		let jump_end = cjmp ctx in
		(match fst decl with
		| EVal ((EConst (Ident _),_) as x) ->
			let k = generate_access ctx x in
			push ctx [VReg 0];
			setvar ctx k
		| EVars (_,_,[(x,_,None)]) ->
			push ctx [VStr x];
			Hashtbl.add ctx.locals x { reg = 0; sp = ctx.stack };
			push ctx [VReg 0];
			write ctx ALocalAssign
		| _ ->
			error (pos decl));
		generate_expr ctx e;
		do_jmp ctx start_pos;
		let has_breaks = (ctx.breaks <> []) in
		generate_breaks ctx old_breaks;
		if has_breaks then gen_forins ctx false;
		jump_end();
		ctx.forins <- ctx.forins - 1;
		ctx.continue_pos <- old_continue;
		block_end()
	| EIf (v,e,eelse) ->
		generate_val ctx v;
		write ctx ANot;
		let jump_else = cjmp ctx in
		generate_expr ctx e;
		(match eelse with
		| None -> jump_else()
		| Some e ->
			let jump_end = jmp ctx in
			jump_else();
			generate_expr ctx e;
			jump_end())
	| EVal v ->
		let s = ctx.stack_size in
		generate_val ~retval:false ctx v;
		pop ctx (ctx.stack_size - s)
	| EWhile (v,e,flag) ->
		let jump_begin = (match flag with NormalWhile -> (fun()->()) | DoWhile -> jmp ctx) in
		let start_pos = ctx.code_pos in
		let old_continue = ctx.continue_pos in
		let old_breaks = ctx.breaks in
		ctx.breaks <- [];
		ctx.opt_push <- false;
		ctx.continue_pos <- start_pos;
		generate_val ctx v;
		write ctx ANot;
		let jump_end = cjmp ctx in
		jump_begin();
		generate_expr ctx e;
		do_jmp ctx start_pos;
		generate_breaks ctx old_breaks;
		ctx.continue_pos <- old_continue;
		jump_end()
	| EWith (v,e) ->
		generate_val ctx v;
		write ctx (AWith 0);
		let start = ctx.code_pos in
		generate_expr ctx e;
		let delta = ctx.code_pos - start in
		DynArray.set ctx.ops (start - 1) (AWith delta);
	| EBreak ->
		ctx.breaks <- jmp ctx :: ctx.breaks
	| EContinue ->
		do_jmp ctx ctx.continue_pos
	| EReturn None ->
		gen_forins ctx true;
		write ctx (APush [PUndefined]);
		write ctx AReturn	
	| EReturn (Some v) ->
		gen_forins ctx true;
		generate_val ctx v;
		write ctx AReturn
	| ESwitch (v,cases) ->
		generate_val ctx v;
		write ctx (ASetReg 0);
		let old_breaks = ctx.breaks in
		let first_case = ref true in
		let def_pos = ref (fun () -> ()) in
		ctx.breaks <- [];
		let cases = List.map (fun (v,e) ->
			match v with
			| None ->
				(fun () -> (!def_pos)(); def_pos := (fun() -> ())) , e
			| Some v -> 
				if !first_case then
					first_case := false
				else
					push ctx [VReg 0];
				generate_val ctx v;
				write ctx APhysEqual;
				cjmp ctx , e
		) cases in
		def_pos := jmp ctx;
		List.iter (fun (j,e) ->
			j();
			generate_expr ctx e
		) cases;
		(!def_pos)();
		generate_breaks ctx old_breaks
	| ETry (e,cl,fo) ->
		let tdata = {
			tr_style = TryRegister 0;
			tr_trylen = 0;
			tr_catchlen = None;
			tr_finallylen = None;
		} in
		write ctx (ATry tdata);
		let p = ctx.code_pos in
		generate_expr ctx e;
		let jump_end = jmp ctx in
		tdata.tr_trylen <- ctx.code_pos - p;
		let p = ctx.code_pos in
		let end_throw = ref true in
		let first_catch = ref true in
		let jumps = List.map (fun (name,t,e) ->
			Hashtbl.add ctx.locals name { reg = 0; sp = ctx.stack };
			let next_catch = (match t with
			| None -> 
				end_throw := false;
				write ctx APop;
				push ctx [VStr name;VReg 0];
				write ctx ALocalAssign;
				generate_expr ctx e;
				(fun() -> ())
			| Some t ->
				if not !first_catch then write ctx APop;
				getvar ctx (generate_access ctx (EStatic t,pos e));
				push ctx [VReg 0];
				write ctx ACast;
				write ctx ADup;
				push ctx [VNull];
				write ctx AEqual;
				let c = cjmp ctx in
				push ctx [VStr name];
				write ctx ASwap;
				write ctx ALocalAssign;
				generate_expr ctx e;
				c
			) in
			first_catch := false;
			let j = jmp ctx in
			next_catch();
			Hashtbl.remove ctx.locals name;
			j
		) !cl in
		if !end_throw && !cl <> [] then begin
			write ctx APop;
			push ctx [VReg 0];
			write ctx AThrow;
		end;
		if !cl <> [] then tdata.tr_catchlen <- Some (ctx.code_pos - p);
		List.iter (fun j -> j()) jumps;
		jump_end();
		(match fo with
		| None -> ()
		| Some e ->
			let p = ctx.code_pos in
			generate_expr ctx e;
			tdata.tr_finallylen <- Some (ctx.code_pos - p))

let super_call = EVal (ECall ((EConst (Ident "super"),null_pos),[]),null_pos) , null_pos

let generate_function ?(constructor=false) ctx f =
	match f.fexpr with
	| None -> ()
	| Some fexpr ->
		let old_name = ctx.curmethod in
		let stack_base , old_nregs = ctx.stack , ctx.reg_count in
		let have_super = used_in_block true "super" fexpr in
		let reg_super = have_super || (constructor && Class.superclass ctx.current <> None) in
		let old_forin = ctx.forins in
		ctx.reg_count <- (if reg_super then 2 else 1);
		if f.fname <> "" then ctx.curmethod <- f.fname;
		ctx.forins <- 0;
		ctx.stack <- ctx.stack + 1;
		let args = List.map (fun (aname,_) ->
			let r = 
				(if used_in_block false aname fexpr then
					0
				else begin
					ctx.reg_count <- ctx.reg_count + 1;
					ctx.reg_count
				end)
			in
			Hashtbl.add ctx.locals aname { reg = r; sp = ctx.stack };
			r , aname
		) f.fargs in
		let fdone = func ctx args reg_super (used_in_block true "arguments" fexpr) in
		if constructor && Class.superclass ctx.current <> None && not have_super then generate_expr ctx super_call;
		generate_expr ctx fexpr;
		if f.fgetter = Setter then begin
			push ctx [VInt 0;VThis;VStr ("__get__"^f.fname)];
			call ctx VarObj 0;
			write ctx AReturn;
		end;
		fdone (ctx.reg_count + 1);
		clean_stack ctx stack_base;
		ctx.forins <- old_forin;
		ctx.reg_count <- old_nregs;
		ctx.curmethod <- old_name

let generate_super_bindings ctx =
	Hashtbl.iter (fun (path,fname) flag ->
		if not flag then begin
			Hashtbl.replace ctx.super_bindings (path,fname) true;
			let ident = super_binding_ident path fname in
			push ctx [VStr ident];
			let k = generate_access ctx (EStatic path,null_pos) in
			getvar ctx k;
			push ctx [VStr "prototype"];
			getvar ctx VarObj;
			push ctx [VStr fname];
			getvar ctx VarObj;
			setvar ctx VarStr;
		end;
	) ctx.super_bindings

let generate_class_code ctx clctx h =
	let cpath , cname = Class.path clctx in
	getvar ctx (generate_access ctx (EStatic (cpath,cname),null_pos));
	write ctx ANot;
	write ctx ANot;
	let jump_end_def = cjmp ctx in
	if not (Hashtbl.mem h cpath) then begin
		generate_package_register ctx cpath;
		Hashtbl.add h cpath ();
	end;
	let k = generate_package ctx cpath in
	push ctx [VStr cname];
	(match Class.constructor clctx with
	| None -> 
		let fdone = func ctx [] true false in
		(match Class.superclass clctx with
		| None -> ()
		| Some _ -> generate_expr ctx super_call);
		fdone 3
	| Some f ->
		generate_function ~constructor:true ctx f);
	write ctx (ASetReg 0);
	setvar ctx k;
	(match Class.superclass clctx with
	| None -> ()
	| Some csuper when !version = Some 6 ->	
		(* myclass.prototype.__proto__ = superclass.prototype *)
		push ctx [VReg 0; VStr "prototype"];
		getvar ctx VarObj;
		push ctx [VStr "__proto__"];
		getvar ctx (generate_access ctx (EStatic (Class.path csuper),null_pos));
		push ctx [VStr "prototype"];
		getvar ctx VarObj;
		setvar ctx VarObj;
		(* myclass.prototype.__constructor__ = superclass *)
		push ctx [VReg 0; VStr "prototype"];
		getvar ctx VarObj;
		push ctx [VStr "__constructor__"];
		getvar ctx (generate_access ctx (EStatic (Class.path csuper),null_pos));
		setvar ctx VarObj
	| Some csuper ->
		push ctx [VReg 0];
		getvar ctx (generate_access ctx (EStatic (Class.path csuper),null_pos));
		write ctx AExtends);
	push ctx [VReg 0; VStr "prototype"];
	getvar ctx VarObj;
	write ctx (ASetReg 1);
	write ctx APop;
	let getters = Hashtbl.create 0 in
	List.iter (fun f ->
		match f.fexpr with
		| None -> ()
		| Some _ ->
			push ctx [VReg (if f.fstatic = IsMember then 1 else 0)];
			let name = (match f.fgetter with
				| Normal -> 
					if f.fname = "main" && f.fstatic = IsStatic && !enable_main then begin
						match !(ctx.main) with
						| None -> ctx.main := Some (Class.path clctx);
						| Some path -> failwith ("Duplicate main entry point : " ^ s_type_path path ^ " and " ^ s_type_path (Class.path clctx))
					end;
					f.fname
				| Getter -> 
					Hashtbl.add getters (f.fname,Getter,f.fstatic) ();
					"__get__" ^ f.fname
				| Setter -> 
					Hashtbl.add getters (f.fname,Setter,f.fstatic) ();
					"__set__" ^ f.fname)
			in
			push ctx [VStr name];
			generate_function ctx f;
			setvar ctx VarObj;
	) (Class.methods clctx);
	let dones = Hashtbl.create 0 in
	Hashtbl.iter (fun (name,get,stat) _ ->
		if Hashtbl.mem dones (name,get,stat) then
			()
		else
		let reg = (if stat = IsMember then 1 else 0) in
		let getter = (get = Getter || Hashtbl.mem getters (name,Getter,stat)) in
		let setter = (get = Setter || Hashtbl.mem getters (name,Setter,stat)) in
		let no_getset = AFunction { f_name = ""; f_args = []; f_codelen = 0 } in

		Hashtbl.add dones (name,Getter,stat) ();
		Hashtbl.add dones (name,Setter,stat) ();
		if setter then begin
			push ctx [VReg reg; VStr ("__set__" ^ name)];
			getvar ctx VarObj;
		end else
			write ctx no_getset;			
		if getter then begin
			push ctx [VReg reg; VStr ("__get__" ^ name)];
			getvar ctx VarObj;
		end else
			write ctx no_getset;
		push ctx [VStr name; VInt 3];
		push ctx [VReg reg; VStr "addProperty"];
		call ctx VarObj 3;
		write ctx APop;
	) getters;
	List.iter (fun cintf ->
		getvar ctx (generate_access ctx (EStatic (Class.path cintf),null_pos));
	) (Class.interfaces clctx);
	let nintf = List.length (Class.interfaces clctx) in
	if nintf > 0 then begin
		push ctx [VInt nintf; VReg 0];
		write ctx AImplements;
		ctx.stack_size <- ctx.stack_size - nintf;
	end;
	push ctx [VInt 1; VNull; VReg 1; VInt 3; VStr "ASSetPropFlags"];
	call ctx VarStr 3;
	write ctx APop;
	List.iter (fun (name,stat,v) ->
		push ctx [VReg (if stat = IsMember then 1 else 0); VStr name];
		generate_val ctx v;
		setvar ctx VarObj;
	) (Class.initvars clctx);
	generate_super_bindings ctx;
	jump_end_def()

let to_utf8 str =
	try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code -> 
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UChar.of_char c)) str;
			UTF8.Buf.contents b

let use_components = ref false
let separate = ref true
let keep = ref false
let bgcolor = ref 0xFFFFFF
let frame = ref 1
let header = ref None
let excludes = Hashtbl.create 0

let new_context ctx =
	{ ctx with
		idents = Hashtbl.create 0;
		locals = Hashtbl.create 0;
		stack_size = 0;
		ops = DynArray.create();
		code_pos = 1;
		ident_count = 0;
		stack = 0;
		reg_count = 0;
		opt_push = false;
	}

let is_excluded (path,name) =
	if Hashtbl.mem excludes (s_type_path (path,name)) then
		true
	else
		let rec loop = function
			| [] -> false
			| p :: l ->
				Hashtbl.mem excludes (String.concat "." (List.rev ("*" :: p :: l))) || loop l
		in
		loop (List.rev path)

let generate file out ~compress exprs =
	let file , linkage =
		(try
			let f,l = String.split file "@" in
			f , Some l
		with
			Invalid_string ->
				file , None)
	in
	let ctx = {
		main = ref None;
		idents = Hashtbl.create 0;
		ops = DynArray.create();
		super_bindings = Hashtbl.create 0;
		current = Class.empty;
		code_pos = 1;
		ident_count = 0;
		stack = 0;
		reg_count = 0;
		locals = Hashtbl.create 0;
		stack_size = 0;
		cur_block = (EBreak,null_pos);
		breaks = [];
		forins = 0;
		continue_pos = 0;
		opt_push = false;
		curmethod = "";
	} in
	DynArray.add ctx.ops (AStringPool []);
	let tags = ref [] in
	let hpackages = Hashtbl.create 0 in
	Class.generate (fun clctx ->
		ctx.current <- clctx;
		let ctx = (if !separate then new_context ctx else ctx) in
		if not (Class.intrinsic clctx) && not (is_excluded (Class.path clctx)) then begin
			if !separate then DynArray.add ctx.ops (AStringPool []);
			let ssize = ActionScript.actions_length ctx.ops in
			generate_class_code ctx clctx (if !separate then Hashtbl.create 0 else hpackages);
			if !separate then tags := ("__Packages." ^ s_type_path (Class.path clctx),ctx.idents,ctx.ops) :: !tags;
			let size = ActionScript.actions_length ctx.ops in
			if size - ssize >= 1 lsl 15 then failwith ("Class " ^ s_type_path (Class.path clctx) ^ " excess 32K bytecode limit, please split it");
		end;
	) exprs;	
	if not !separate then tags := ("__Packages.MTASC",ctx.idents,ctx.ops) :: !tags;
	(match !(ctx.main) with
	| None ->
		if !enable_main then failwith "Main entry point not found";
	| Some (p,clname) -> 
		let ctx = new_context ctx in 
		DynArray.add ctx.ops (AStringPool []);
		(*//    (main class).main(this); *)
		push ctx [VStr "MTASC_MAIN"];
		write ctx (ASetReg 0);
		write ctx APop;
		push ctx [VStr "this"];
		write ctx AEval;
		push ctx [VInt 1];
		let k = generate_package ~fast:true ctx p in
		push ctx [VStr clname];
		getvar ctx k;
		push ctx [VStr "main"];
		call ctx VarObj 1;
		write ctx APop;
		tags := ("",ctx.idents,ctx.ops) :: !tags;
	);
	tags := List.rev !tags;
	List.iter (fun (n,idents,ops) ->
		let idents = Hashtbl.fold (fun ident pos acc -> (ident,pos) :: acc) idents [] in
		let idents = List.sort ~cmp:(fun (_,p1) (_,p2) -> compare p1 p2) idents in
		DynArray.set ops 0 (AStringPool (List.map (fun (id,_) -> to_utf8 id) idents));
	) !tags;
	let tag ?(ext=false) d = {
		tid = 0;
		textended = ext;
		tdata = d;
	} in
	let header , data = (match !header with
		| None ->
			let ch = IO.input_channel (open_in_bin file) in
			let header, data = (try Swf.parse ch with IO.No_more_input | IO.Overflow _ | IO.Bits_error -> failwith "Input swf is corrupted") in
			IO.close_in ch;
			header , data
		| Some h ->
			let data = [tag (TSetBgColor { cr = !bgcolor lsr 16; cg = (!bgcolor lsr 8) land 0xFF; cb = !bgcolor land 0xFF }) ] in
			let data = data @ (Array.to_list (Array.init !frame (fun _ -> tag TShowFrame))) in
			h , data)
	in
	let header = (match !version with None -> header | Some v -> { header with h_version = v }) in
	let found = ref false in
	let curf = ref !frame in
	let regs = ref [] in
	let found_ids = ref [] in
	let insert loop showf acc l =
		if !found || !curf > 1 then begin
			curf := !curf - 1;
			loop (showf @ acc) l
		end else begin
			found := true;
			let main = ref None in
			let rec loop_tag cid l = 
				if List.exists ((=) cid) !found_ids then
					loop_tag (cid + 1) l
				else
					loop_tag_rec cid l
			and loop_tag_rec cid = function				
				| [] -> []
				| ("",_,ops) :: l ->
					main := Some (tag ~ext:true (TDoAction ops));
					loop_tag (cid + 1) l
				| (name,_,ops) :: l ->
					tag ~ext:true (TClip { c_id = cid; c_frame_count = 1; c_tags = [] }) ::
					tag ~ext:true (TExport [{ exp_id = cid; exp_name = name }]) ::
					tag ~ext:true (TDoInitAction { dia_id = cid; dia_actions = ops }) ::
					loop_tag (cid + 1) l
			in
			let t = List.rev (loop_tag 0x5000 !tags) in
			loop (showf @ (match !main with None -> [] | Some m -> [m]) @ !regs @ t @ acc) l
		end
	in	
	let replace_package p cid x y z = 
		if p = "__Packages.MTASC" || (not !use_components && not !keep) then
			[]
		else try 			
			let t = List.find (fun (n,_,_) -> p = n) !tags in
			tags := List.filter ((!=) t) !tags;
			[x;y;tag ~ext:true (TDoInitAction { dia_id = cid; dia_actions = (match t with (_,_,o) -> o) })]
		with Not_found ->
			if !use_components && String.length p > 14 && String.sub p 0 14 = "__Packages.mx." then
				[x;y;z]
			else if !keep then
				[x;y;z]
			else
				[]
	in
	let rec loop acc = function
		| [] ->
			if not !found then failwith ("Frame " ^ string_of_int !frame ^ " not found in SWF");
			List.rev acc
		| ({ tdata = TDoAction a } as x1) :: ({ tdata = TShowFrame } as x2) :: l ->
			if DynArray.length a > 0 && (match DynArray.get a 0 with AStringPool ("MTASC_MAIN" :: _) -> true | _ -> false) then
				loop acc (x2 :: l)
			else
				insert loop [x2;x1] acc l
		| ({ tdata = TShowFrame } as x) :: l ->
			insert loop [x] acc l
		| ({ tdata = TClip _ } as x) :: ({ tdata = TExport [{ exp_name = e; exp_id = cid }] } as y) :: ({ tdata = TDoInitAction _ } as z) :: l ->
			let l2 = replace_package e cid x y z in
			if l2 <> [] then found_ids := cid :: !found_ids;
			loop ((List.rev l2) @ acc) l
		| { tdata = TDoInitAction { dia_actions = d } } as x :: l ->
			let process mcname clname =
				let cpath = (match List.rev (String.nsplit clname ".") with [] -> assert false | x :: l -> List.rev l , x) in
				(try
					ignore(Class.getclass ctx.current cpath)
				with
				_ -> 
					if not !use_components || (match cpath with ("mx" :: _, _) -> false | _ -> true) then
						prerr_endline ("Warning : The MovieClip " ^ mcname ^ " needs the class " ^ clname ^ " which was not compiled :\nPlease force compilation of this class by adding it to the commandline."));				
				if !found then
					loop (x :: acc) l
				else begin
					regs := x :: !regs;
					loop acc l
				end
			in
			(match DynArray.to_list d with
			| [
				APush [PString clname];
				AEval;
				APush [PString mcname;PInt _;PString "Object"];
				AEval;
				APush [PString "registerClass"];
				AObjCall;
				APop
			] ->
				process mcname clname
			| [
				AStringPool [clname;"Object";"registerClass"];
				APush [PStack 0];
				AEval;
				APush [PStack 0;PInt _;PStack 1];
				AEval;
				APush [PStack 2];
				AObjCall;
				APop;
			] -> 				
				process clname clname
			| _ ->
				loop (x :: acc) l);
		| x :: l ->
			loop (x :: acc) l
	in
	let ch = IO.output_channel (open_out_bin out) in
	Swf.write ch (header,loop [] data);
	IO.close_out ch

let make_header s =
	let sl = String.nsplit s ":" in
	try
		let make w h fps =
			let w = int_of_string w in
			let h = int_of_string h in
			{
				h_version = 7;
				h_size = {
					rect_nbits = if (max w h) >= 820 then 16 else 15;
					left = 0;
					top = 0;
					right = w * 20;
					bottom = h * 20;
				};
				h_frame_count = 1;
				h_fps = to_float16 (float_of_string fps);
				h_compressed = true;
			}
		in
		match sl with
		| [w;h;fps] ->
			make w h fps
		| [w;h;fps;color] ->
			bgcolor := int_of_string ("0x" ^ color);
			make w h fps;
		| _ ->
			raise Exit
	with
		_ -> raise (Arg.Bad "Invalid header format")

let rec trim f =
	let l = String.length f in
	if l = 0 then
		""
	else match f.[l - 1] with
		| '\r' | '\n' -> trim (String.sub f 0 (l - 1))
		| _ -> f

let exclude_file f =
	let lines = (try
		let ch = open_in (Plugin.find_file f) in
		let lines = Std.input_list ch in
		close_in ch;
		lines
	with Not_found | Sys_error _ ->
		String.nsplit f ";"
	) in
	List.iter (fun f ->
		let f = trim f in
		if f <> "" then Hashtbl.replace excludes f ()
	) lines

;;
generate_function_ref := generate_function;
SwfParser.init SwfZip.inflate SwfZip.deflate;
SwfParser.full_parsing := false; (* faster, safer *)
Swf.warnings := false;
let swf = ref None in
let out = ref None in
Plugin.add [
	("-swf",Arg.String (fun f -> swf := Some f),"<file> : swf file to update");
	("-out",Arg.String (fun f -> out := Some f),"<file> : swf output file");
	("-keep",Arg.Unit (fun () -> keep := true),": does not remove AS2 classes from input SWF");
	("-frame",Arg.Int (fun i -> if i <= 0 then raise (Arg.Bad "Invalid frame"); frame := i),"<frame> : export into target frame (must exist in the swf)");
	("-main",Arg.Unit (fun () -> enable_main := true),": enable main entry point");
	("-header",Arg.String (fun s -> header := Some (make_header s)),"<header> : specify header format 'width:height:fps'");
	("-group",Arg.Unit (fun () -> separate := false),": group classes into a single clip");
	("-exclude",Arg.String (fun f -> exclude_file f),"<file> : exclude classes listed in file");
	("-version",Arg.Int (fun n -> version := Some n),": change SWF version (6,7,8,...)");	
	("-trace",Arg.String (fun t -> ftrace := Some t),"<function> : specify a TRACE function");
]
(fun t ->
	if !keep && !header <> None then failwith "-keep cannot be used together with -header";
	if !Plugin.verbose && Hashtbl.length excludes > 0 then Printf.printf "Excludes : %s\n" (String.concat ";" (List.of_enum (Hashtbl.keys excludes)));
	match !swf with 
	| None -> () 
	| Some f -> generate f (match !out with None -> f | Some f -> f) ~compress:true (Typer.exprs t)
);
