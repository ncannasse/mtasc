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
open SwfZip
open ExtHashtbl
open ExtString

type kind = 
	| VarReg of int
	| VarStr
	| VarObj

type local_ctx = {
	reg : int;
	sp : int;
}

type context =  {
	idents : (string,int) Hashtbl.t;
	ops : action DynArray.t;
	super_bindings : (type_path * string,bool) Hashtbl.t;
	locals : (string,local_ctx) Hashtbl.t;
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
}

type push_style =
	| VStr of string
	| VInt of int
	| VInt32 of int32
	| VFloat of float
	| VThis
	| VNull
	| VSuper

let stack_delta = function
	| APush l -> List.length l
	| ASetReg _ -> 0
	| AAdd | ADivide | ASubtract | AMultiply | AMod -> -1
	| AAnd | AOr | AXor | AShl | AShr | AAsr -> -1
	| ACompare | AGreater -> -1
	| AEval | ANot | AJump _ | AToInt | AToString -> 0
	| ACondJump _ -> -1
	| AEqual | APhysEqual -> -1
	| ANew -> -1 (** only if 0 params **)
	| AObject | AInitArray -> 0 (** calculated outside **)
	| ASet -> -2
	| APop -> -1
	| AFunction2 _ -> 1	
	| ADup -> 1
	| AObjGet -> -1
	| AObjSet -> -3
	| ALocalVar -> -1
	| ALocalAssign -> -2
	| AReturn -> -1
	| AGetURL2 _ -> -1
	| ADeleteObj | AInstanceOf -> -1
	| AExtends | AImplements -> -2
	| AEnum2 -> -1
	| AIncrement | ADecrement | AChr | AOrd | ARandom | ADelete | AGetTimer | ATypeOf -> 0
	| AObjCall | ACall | ANewMethod -> assert false
	| op -> failwith ("Unknown stack delta for " ^ (ActionScript.action_string (fun _ -> "") 0 op))

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
	) in
	DynArray.add ctx.ops op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let new_call ctx kind n  =
	let op , n = (match kind with
		| VarReg n ->
			write ctx (APush [PReg n;PUndefined]);
			ANewMethod , n + 2			
		| VarStr -> 
			ANew , n + 1
		| VarObj ->
			ANewMethod , n + 2) in
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

let func ctx args constructor =
	let default_flags = [ThisRegister;ArgumentsNoVar] in
	let f = {
		f2_name = "";
		f2_args = args;
		f2_codelen = 0;
		f2_nregs = 0;
		f2_flags = (if constructor then SuperRegister :: default_flags else SuperNoVar :: default_flags);
	} in
	write ctx (AFunction2 f);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.ops - 1 in
	(fun nregs ->
		let delta = ctx.code_pos - start_pos in
		f.f2_codelen <- delta;
		f.f2_nregs <- nregs
	)

let setvar ?(retval=false) ctx = function
	| VarReg (-1) -> assert false (** true, false, null **)
	| VarReg n -> write ctx (ASetReg n); if not retval then write ctx APop
	| VarStr -> write ctx ASet
	| VarObj -> write ctx AObjSet

let getvar ctx = function
	| VarReg (-1) -> () (** true, false, null **)
	| VarReg n -> write ctx (APush [PReg n])
	| VarStr -> write ctx AEval
	| VarObj -> write ctx AObjGet

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
		| EArray (v1,v2) ->
			vloop v1 || vloop v2
		| EBinop (_,v1,v2) ->
			vloop v1 || vloop v2
		| EField (v,_) ->
			vloop v
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
		| ENew (_,vl) ->
			List.exists vloop vl
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
		| ESwitch (v,cases,eopt) ->
			vloop v || List.exists (fun (v,e) -> vloop v || loop e) cases || (match eopt with None -> false | Some e -> loop e)
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
	| "_global" | "_root" ->
		push ctx [VStr s];
		VarStr
	| "__CLASS__" ->
		let p , name = Class.path ctx.current in
		let id = (match p with [] -> "" | _ -> String.concat "." p ^ ".") ^ name in
		push ctx [VStr id];
		VarReg (-1)
	| "__METHOD__" ->
		push ctx [VStr ctx.curmethod];
		VarReg (-1)
	| "__FILE__" ->
		push ctx [VStr p.pfile];
		VarReg (-1)
	| "__LINE__" ->
		let line = Lexer.get_error_line p in
		push ctx [VInt line];
		VarReg (-1)
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
			raise (Typer.Error (Typer.Custom ("Uncatched ident " ^ s),p))

let unescape_chars s = 
	let s = String.concat "\n" (String.nsplit s "\\n") in
	let s = String.concat "\r" (String.nsplit s "\\r") in
	let s = String.concat "\t" (String.nsplit s "\\t") in
	let s = String.concat "\"" (String.nsplit s "\\\"") in
	let s = String.concat "'" (String.nsplit s "\\'") in
	let s = String.concat "\\" (String.nsplit s "\\\\") in
	s

let generate_constant ctx p = function
	| Int str -> push ctx [VInt32 (try Int32.of_string str with _ -> error p)]
	| Float str -> push ctx [VFloat (try float_of_string str with _ -> error p)]
	| String s -> push ctx [VStr (unescape_chars s)]
	| Ident s -> assert false

let generate_breaks ctx olds =
	List.iter (fun f -> f()) ctx.breaks;
	ctx.breaks <- olds

let generate_function_ref = ref (fun _ _ -> assert false)

let rec generate_access ?(forcall=false) ctx (v,p) =
	match v with
	| EConst (Ident "super") ->
		(* for superconstructor *)
		if not forcall then error p;
		push ctx [VSuper];
		write ctx (APush [PUndefined]);
		VarObj
	| EConst (Ident s) ->
		generate_ident ctx s p
	| EField (v,s) ->
		generate_val ctx v;
		push ctx [VStr s];
		VarObj
	| EStatic path ->
		(match Class.resolve ctx.current path with
		| [] , "Xml" ->
			push ctx [VStr "XML"];
			VarStr
		| [] , "XmlNode" ->
			push ctx [VStr "XMLNode"];
			VarStr
		| [] , "XmlSocket" ->
			push ctx [VStr "XMLSocket"];
			VarStr
		| p , s ->
			let k = generate_package ~fast:true ctx p in
			push ctx [VStr s];
			k)
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
		(match generate_access ~forcall:true ctx v with
		| VarObj -> write ctx ADeleteObj
		| _ -> write ctx ADelete)
	| EConst (Ident "getTimer"), [] ->
		write ctx AGetTimer
	| EConst (Ident ("getURL" as x)) , params
	| EConst (Ident ("loadMovie" as x)) , params
	| EConst (Ident ("loadVariables" as x)) , params ->
		generate_geturl ctx x params (pos v)
	| EField ((EConst (Ident "super"),_),fname) , args ->
		let path = Class.resolve_supervar ctx.current fname in
		let ident = super_binding_ident path fname in
		let nargs = List.length args + 1 in
		List.iter (generate_val ctx) (List.rev args);		
		push ctx [VThis; VInt nargs; VStr ident];
		write ctx AEval;
		push ctx [VStr "call"];
		call ctx VarObj nargs;
		if not (Hashtbl.mem ctx.super_bindings (path,fname)) then
			Hashtbl.add ctx.super_bindings (path,fname) false
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
		generate_val ctx v
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
	| EUnop (Neg,_,(EConst (Int s),p)) ->		
		push ctx [VInt32 (Int32.neg (try Int32.of_string s with _ -> error p))]
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
		let k = generate_access ctx v in
		let k = generate_access ctx v in
		getvar ctx k;
		write ctx (match op with Increment -> AIncrement | Decrement -> ADecrement | _ -> assert false);
		if retval && flag = Prefix then write ctx (ASetReg 0);
		setvar ctx k;
		if retval && flag = Prefix then write ctx (APush [PReg 0])

let generate_local_var ctx (vname,_,vinit) =
	if used_in_block false vname ctx.cur_block then begin
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
		List.iter (generate_val ~retval:false ctx) incrs;
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
		write ctx (ASetReg 0);
		push ctx [VNull];
		write ctx AEqual;
		let jump_end = cjmp ctx in
		(match fst decl with
		| EVal (EConst (Ident x),_) ->
			push ctx [VStr x];
			write ctx (APush [PReg 0]);
			write ctx ASet;
		| EVars (_,_,[(x,_,None) as l]) ->
			push ctx [VStr x];
			Hashtbl.add ctx.locals x { reg = 0; sp = ctx.stack };
			write ctx (APush [PReg 0]);
			write ctx ALocalAssign
		| _ ->
			error (pos decl));
		generate_expr ctx e;
		do_jmp ctx start_pos;
		jump_end();
		generate_breaks ctx old_breaks;
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
	| EBreak ->
		ctx.breaks <- jmp ctx :: ctx.breaks
	| EContinue ->
		do_jmp ctx ctx.continue_pos
	| EReturn None ->
		write ctx (APush [PUndefined]);
		write ctx AReturn	
	| EReturn (Some v) ->
		generate_val ctx v;
		write ctx AReturn		
	| ESwitch (v,cases,edefault) ->
		generate_val ctx v;
		write ctx (ASetReg 0);
		let old_breaks = ctx.breaks in
		let first_case = ref true in
		ctx.breaks <- [];
		let cases = List.map (fun (v,e) ->
			if !first_case then
				first_case := false
			else
				write ctx (APush [PReg 0]);
			generate_val ctx v;
			write ctx APhysEqual;
			cjmp ctx , e
		) cases in
		let jump_default = jmp ctx in
		List.iter (fun (j,e) ->
			j();
			generate_expr ctx e
		) cases;
		jump_default();
		(match edefault with
		| None -> ()
		| Some e ->
			generate_expr ctx e);
		generate_breaks ctx old_breaks

let generate_function ?(constructor=false) ctx f =
	match f.fexpr with
	| None -> ()
	| Some fexpr ->
		let old_name = ctx.curmethod in
		let stack_base , old_nregs = ctx.stack , ctx.reg_count in
		let reg_super = constructor && used_in_block true "super" fexpr in
		ctx.reg_count <- (if reg_super then 2 else 1);
		if f.fname <> "" then ctx.curmethod <- f.fname;
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
		let fdone = func ctx args reg_super in
		generate_expr ctx fexpr;
		fdone (ctx.reg_count + 1);
		clean_stack ctx stack_base;
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

let generate_class_code ctx clctx =
	let cpath , cname = Class.path clctx in
	let gen_full_path() =
		let k = generate_package ctx cpath in
		push ctx [VStr cname];
		k
	in
	generate_package_register ctx cpath;
	let k = gen_full_path() in
	(match Class.constructor clctx with
	| None -> 
		let fdone = func ctx [] true in
		fdone 3;
	| Some f ->
		generate_function ~constructor:true ctx f);
	setvar ctx k;
	(match Class.superclass clctx with
	| None -> ()
	| Some csuper ->
		let k = gen_full_path() in
		getvar ctx k;
		let k = generate_access ctx (EStatic (Class.path csuper),null_pos) in 
		getvar ctx k;
		write ctx AExtends);
	List.iter (fun f ->
		match f.fexpr with
		| None -> ()
		| Some _ ->
			let k = gen_full_path() in
			getvar ctx k;
			if f.fstatic = IsMember then begin
				push ctx [VStr "prototype"];
				getvar ctx VarObj;
			end;
			push ctx [VStr f.fname];
			generate_function ctx f;
			setvar ctx VarObj;
	) (Class.methods clctx);
	List.iter (fun cintf ->
		getvar ctx (generate_access ctx (EStatic (Class.path cintf),null_pos));
	) (Class.interfaces clctx);
	let nintf = List.length (Class.interfaces clctx) in
	if nintf > 0 then begin
		push ctx [VInt nintf];
		getvar ctx (gen_full_path());
		write ctx AImplements;
		ctx.stack_size <- ctx.stack_size - nintf;
	end;
	List.iter (fun (name,v) ->
		let k = gen_full_path() in
		getvar ctx k;
		push ctx [VStr name];
		generate_val ctx v;
		setvar ctx VarObj;
	) (Class.statics clctx);
	generate_super_bindings ctx

let to_utf8 str =
	let b = UTF8.Buf.create 0 in
	String.iter (fun c -> UTF8.Buf.add_char b (UChar.of_char c)) str;
	UTF8.Buf.contents b

let generate file ~compress ~keep exprs =
	let file , linkage =
		(try
			let f,l = String.split file "@" in
			f , Some l
		with
			Invalid_string ->
				file , None)
	in
	let ctx = {
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
		continue_pos = 0;
		opt_push = false;
		curmethod = "";
	} in
	DynArray.add ctx.ops (AStringPool []);
	push ctx [VStr "Compiled with MTASC : http://tech.motion-twin.com"];
	write ctx APop;
	Class.generate (fun clctx ->
		ctx.current <- clctx;
		if not (Class.intrinsic clctx) then generate_class_code ctx clctx
	) exprs;
	let idents = Hashtbl.fold (fun ident pos acc -> (ident,pos) :: acc) ctx.idents [] in
	let idents = List.sort (fun (_,p1) (_,p2) -> compare p1 p2) idents in
	DynArray.set ctx.ops 0 (AStringPool (List.map (fun (id,_) -> to_utf8 id) idents));	
	
	let ch = IO.input_channel (open_in_bin file) in
	let header, data = Swf.parse ch in
	IO.close_in ch;

	let ch = IO.output_channel (open_out_bin file) in
	let tag d = {
		tid = 0;
		textended = false;
		tdata = d;
	} in
	let rec loop = function
		| [] -> assert false
		| ({ tdata = TShowFrame } :: l as all) -> 
			let clip_id = 0xFFFF in
			tag (TClip { c_id = clip_id ; c_frame_count = 1; c_tags = [tag TShowFrame] }) ::
			tag (TExport [{ exp_id = clip_id; exp_name = "__Packages.MTASC" }]) ::
			tag (TDoInitAction { dia_id = clip_id; dia_actions = ctx.ops }) ::
			all
		| { tdata = TClip _ } :: { tdata = TExport [{ exp_name = e }] } :: { tdata = TDoInitAction _ } :: l when
			(not keep || e = "__Packages.MTASC") &&
			String.length e > 11 &&
			String.sub e 0 11 = "__Packages." -> loop l
		| x :: l ->
			x :: loop l
	in
	Swf.write ch (header,loop data);
	IO.close_out ch

;;
generate_function_ref := generate_function;