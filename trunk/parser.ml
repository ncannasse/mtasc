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

type error_msg =
	| Unexpected of token
	| Unclosed_parenthesis
	| Duplicate_default

exception Error of error_msg * pos

let warning = ref (fun (msg : string) (p : pos) -> ())
let use_components = ref false
let last_comment = ref None

let error_msg = function
	| Unexpected t -> "Unexpected "^(s_token t)
	| Unclosed_parenthesis -> "Unclosed parenthesis"
	| Duplicate_default -> "Duplicate default"

let error m p = raise (Error (m,p))

let priority = function
	| OpAssign | OpAssignOp _ -> -4
	| OpBoolOr -> -3
	| OpBoolAnd -> -2
	| OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte | OpPhysEq | OpPhysNotEq -> -1
	| OpOr | OpAnd | OpXor -> 0
	| OpShl | OpShr | OpUShr -> 1
	| OpAdd | OpSub -> 2
	| OpMult | OpDiv -> 3
	| OpMod -> 4

let is_not_assign = function
	| OpAssign | OpAssignOp _ -> false
	| _ -> true

let can_swap _op op =
	let p1 = priority _op in
	let p2 = priority op in
	if p1 < p2 then
		true
	else if p1 = p2 && p1 >= 0 then (* numerical ops are left-assoc *)
		true
	else
		false

let rec make_binop op e ((v,p2) as e2) =
	match v with
	| EBinop (_op,_e,_e2) when can_swap _op op && (is_not_assign _op || is_not_assign op) ->
		let _e = make_binop op e _e in
		EBinop (_op,_e,_e2) , punion (pos _e) (pos _e2)
	| EQuestion (_e,_e1,_e2) when is_not_assign op ->
		let _e = make_binop op e _e in
		EQuestion ( _e, _e1, _e2) , punion (pos e) (pos _e2)
	| _ ->
		EBinop (op,e,e2) , punion (pos e) (pos e2)

let rec make_unop op ((v,p2) as e) p1 =
	match v with
	| EBinop (bop,e,e2) -> EBinop (bop, make_unop op e p1 , e2) , (punion p1 p2)
	| EQuestion (_e,_e1,_e2) ->
		let _e = make_unop op _e p1 in
		EQuestion ( _e, _e1, _e2) , punion p1 (pos _e2)
	| _ ->
		EUnop (op,Prefix,e), punion p1 p2

let rec make_path e =
	let rec loop acc (e,_) =
		match e with
		| EConst (Ident s) -> s :: acc
		| EField (e,f) -> loop (f :: acc) e
		| _ -> raise Stream.Failure
	in
	loop [] e

let wrap_var e =
	match e with
	| EVars _ , p -> EBlock [e] , p
	| _ -> e

let rec	parse_code = parser
	| [< '(Eof,_) >] -> []
	| [< '(Next,_); el = parse_code >] -> el
	| [< e = parse_signature; el = parse_code >] -> e :: el

and parse_signature = parser
	| [< '(BkOpen,_); _ = parse_metadata; s = parse_signature >] -> s
	| [< '(Kwd Import,p1); p , w = parse_import >] -> EImport (p,w) , p1
	| [< '(Kwd Interface,p1); path = parse_class_path; herits = parse_herits; '(BrOpen,p); el , p2 = parse_class true >] ->
		EInterface (path,herits,(EBlock el,punion p p2)) , punion p1 p2
	| [< flags = parse_class_flags; '(Kwd Class,p); path = parse_class_path; herits = parse_herits; '(BrOpen,op); s >] ->
		let el, p2 = parse_class (List.exists ((=) HIntrinsic) flags) s in
		EClass (path, flags @ herits, (EBlock el, punion op p2)) , punion p p2
	| [< _ = parse_include; s = parse_signature >] -> s

and parse_herits = parser
	| [< '(Kwd Extends,_); p = parse_class_path; l = parse_herits >] -> HExtends p :: l
	| [< '(Kwd Implements,_); p = parse_class_path; l = parse_other_implements >] -> HImplements p :: l
	| [< >] -> []

and parse_other_implements = parser
	| [< '(Sep,_); p = parse_class_path; l = parse_other_implements >] -> HImplements p :: l
	| [< l = parse_herits >] -> l

and parse_class_flags = parser
	| [< '(Kwd Intrinsic,_); l = parse_class_flags >] -> HIntrinsic :: l
	| [< '(Kwd Dynamic,_); l = parse_class_flags >] -> HDynamic :: l
	| [< >] -> []

and parse_class interf = parser
	| [< '(BrClose,p) >] -> [] , p
	| [< '(Next,_); n = parse_class interf >] -> n
	| [< '(BkOpen,_); _ = parse_metadata; i = parse_class interf >] -> i
	| [< _ = parse_include; s = parse_class interf >] -> s
	| [< flags = parse_field_flags IsMember None; f = parse_class_field flags interf; fl , p = parse_class interf >] -> f :: fl , p

and parse_field_flags stat pub = parser
	| [< '(Kwd Static,_) when stat = IsMember; f = parse_field_flags IsStatic pub >] -> f
	| [< '(Kwd Public,_) when pub = None; f = parse_field_flags stat (Some IsPublic) >] -> f
	| [< '(Kwd Private,_) when pub = None; f = parse_field_flags stat (Some IsPrivate) >] -> f
	| [< >] -> stat , (match pub with None -> IsPublic | Some p -> p)

and parse_class_field (stat,pub) interf = parser
	| [< '(Kwd Var,p1); vl, p2 = parse_vars p1 >] -> EVars (stat,pub,vl) , punion p1 p2
	| [< '(Kwd Function,p1); name, g = parse_fun_name; '(POpen,_); args , p2 = parse_args; t = parse_type_option; s >] ->
		EFunction {
			fname = name;
			fargs = args;
			ftype = t;
			fstatic = stat;
			fpublic = pub;
			fgetter = g;
			fexpr = if interf then None else Some (parse_expr s);
		} , punion p1 p2

and parse_fun_name = parser
	| [< '(Kwd k,p) when Filename.basename p.pfile = "TopLevel.as" >] -> s_keyword k , Normal
	| [< '(Const (Ident name),_); s >] ->
		match name with
		| "get" | "set" ->
			(match s with parser
			| [< '(Const (Ident name2),_) >] -> name2 , if name = "get" then Getter else Setter
			| [< >] -> name , Normal)
		| _ ->
			name , Normal

and parse_expr = parser
	| [< '(BrOpen,p1); el , p2 = parse_block parse_expr p1 >] -> EBlock el , punion p1 p2
	| [< '(Kwd For,p); '(POpen,_); c = parse_expr_opt; e = parse_for p c >] -> e
	| [< '(Kwd If,p); cond = parse_eval; e = parse_expr_opt; e2 , p2 = parse_else (pos e) >] -> EIf (cond,wrap_var e,e2), punion p p2
	| [< '(Kwd Return,p); v , p2 = parse_eval_option p; >] -> EReturn v , punion p p2
	| [< '(Kwd Break,p) >] -> EBreak , p
	| [< '(Kwd Continue,p) >] -> EContinue , p
	| [< '(Kwd While,p1); v = parse_eval; e = parse_expr_opt >] -> EWhile (v,wrap_var e,NormalWhile) , punion p1 (pos e)
	| [< '(Kwd Do,p1); e = parse_expr; '(Kwd While,_); v = parse_eval; >] -> EWhile (v,wrap_var e,DoWhile) , punion p1 (pos v)
	| [< '(Kwd Switch,p1); v = parse_eval; '(BrOpen,_); el, p2 = parse_switch false >] -> ESwitch (v,el) , punion p1 p2
	| [< '(Kwd Var,p1); vl, p2 = parse_vars p1 >] -> EVars (IsMember,IsPublic,vl), punion p1 p2
	| [< '(Kwd Try,p1); e = parse_expr; c = parse_catches; f = parse_finally >] -> ETry (wrap_var e,ref c,f) , punion p1 (pos e)
	| [< '(Kwd With,p1); v = parse_eval; e = parse_expr >] -> EWith (v,wrap_var e) , punion p1 (pos e)
	| [< e = parse_eval >] -> EVal e , pos e
	| [< _ = parse_include; e = parse_expr >] -> e

and parse_eval = parser
	| [< '(Kwd Function,p1); '(POpen,_); args, _ = parse_args; t = parse_type_option; e = parse_expr;
		v = parse_eval_next (ELambda {
			fname = "";
			fargs = args;
			ftype = t;
			fgetter = Normal;
			fstatic = IsStatic;
			fpublic = IsPublic;
			fexpr = Some e;
		} , punion p1 (pos e)) >] -> v
	| [< '(Kwd Throw,p1); e = parse_delete (EConst (Ident "throw"),p1) >] -> e
	| [< '(Kwd Delete,p1); e = parse_delete (EConst (Ident "delete"),p1) >] -> e
	| [< '(Kwd Typeof,p1); e = parse_delete (EConst (Ident "typeof"),p1) >] -> e
	| [< '(Kwd New,p1); v, p2 = parse_eval; s >] ->
		(match v with
		| ECall (v,args) -> parse_eval_next (ENew (v,args), punion p1 p2) s
		| _ -> parse_eval_next (ENew ((v,p2),[]), punion p1 p2) s)
	| [< '(Const c,p); e = parse_eval_next (EConst c,p)  >] -> e
	| [< '(Kwd This,p); e = parse_eval_next (EConst (Ident "this"),p) >] -> e
	| [< '(POpen,p1); e = parse_eval; '(PClose,p2); e = parse_eval_next (EParenthesis e , punion p1 p2) >] -> e
	| [< '(BrOpen,p1); el, p2 = parse_field_list; e = parse_eval_next (EObjDecl el, punion p1 p2) >] -> e
	| [< '(BkOpen,p1); el, p2 = parse_array; e = parse_eval_next (EArrayDecl el,punion p1 p2) >] -> e
	| [< '(Unop op,p1) when is_prefix op; e = parse_eval >] -> make_unop op e p1
	| [< '(Binop OpSub,p1); e = parse_eval >] -> make_unop Neg e p1
	| [< _ = parse_include; e = parse_eval_next (EObjDecl [],null_pos) >] -> e

and parse_eval_next e = parser
	| [< '(BkOpen,_); e2 = parse_eval; '(BkClose,p2); e = parse_eval_next (EArray (e,e2) , punion (pos e) p2) >] -> e
	| [< '(Binop op,_); e2 = parse_eval; >] -> make_binop op e e2
	| [< '(Kwd And,_); e2 = parse_eval; >] -> make_binop OpBoolAnd e e2
	| [< '(Dot,_); '(Const (Ident field),p2); e = parse_eval_next (EField (e,field), punion (pos e) p2) >] -> e
	| [< '(POpen,_); args = parse_eval_list; '(PClose,p2); e = parse_eval_next (ECall (e,args), punion (pos e) p2) >] -> e
	| [< '(Unop op,p2) when is_postfix e op; e = parse_eval_next (EUnop (op,Postfix,e), punion (pos e) p2) >] -> e
	| [< '(Question,_); v1 = parse_eval; '(DblDot,_); v2 = parse_eval; e = parse_eval_next (EQuestion (e,v1,v2), punion (pos e) (pos v2)) >] -> e
	| [< '(Kwd InstanceOf,p); v = parse_eval; s >] ->
		let iof v = ECall ((EConst (Ident "instanceof"), p),[e;v]) , punion (pos e) (pos v) in
		let rec loop = function
			| EBinop (op,e1,e2) , pv -> EBinop (op,loop e1,e2) , punion p pv
			| EQuestion (e,e1,e2) , pv -> EQuestion(loop e,e1,e2) , punion p pv
			| v -> iof v
		in
		parse_eval_next (loop v) s
	| [< >] -> e

and parse_delete v = parser
	| [< e = parse_eval; s >] ->
		let rec loop = function
			| EBinop (op,e1,e2) , _ ->
				EBinop (op,loop e1,e2) , punion (pos e) (pos v)
			| e ->
				ECall (v , [e]) , punion (pos e) (pos v)
		in
		parse_eval_next (loop e) s
	| [< e = parse_eval_next v >] -> e

and parse_catches = parser
	| [< '(Kwd Catch,_); '(POpen,_); '(Const (Ident name),_); t = parse_type_option; '(PClose,_); e = parse_expr; l = parse_catches >] -> (name, t, e) :: l
	| [< >] -> []

and parse_finally = parser
	| [< '(Kwd Finally,_); e = parse_expr >] -> Some e
	| [< >] -> None

and parse_eval_option p = parser
	| [< v = parse_eval >] -> Some v , pos v
	| [< >] -> None, p

and parse_eval_list = parser
	| [< v = parse_eval; vl = parse_eval_list2 >] -> v :: vl
	| [< '(Next,_) >] -> []
	| [< >] -> []

and parse_eval_list2 = parser
	| [< '(Sep,_); v = parse_eval; vl = parse_eval_list2 >] -> v :: vl
	| [< '(Next,_) >] -> []
	| [< >] -> []

and parse_field_list = parser
	| [< '(Const (Ident fname),_); '(DblDot,_); e = parse_eval; el , p = parse_field_list2 >] -> (fname,e) :: el , p
	| [< '(BrClose,p) >] -> [] , p

and parse_field_list2 = parser
	| [< '(Sep,_); '(Const (Ident fname),_); '(DblDot,_); e = parse_eval; el , p = parse_field_list2 >] -> (fname,e) :: el , p
	| [< '(BrClose,p) >] -> [] , p

and parse_array = parser
	| [< e = parse_eval; el , p = parse_array2 >] -> e :: el , p
	| [< '(BkClose,p) >] -> [] , p

and parse_array2 = parser
	| [< '(Sep,_); e = parse_eval; el , p = parse_array2 >] -> e :: el , p
	| [< '(BkClose,p) >] -> [] , p

and parse_else p = parser
	| [< '(Next,_); e = parse_else p >] -> e
	| [< '(Kwd Else,_); e = parse_expr >] -> Some (wrap_var e), pos e
	| [< >] -> None , p

and parse_expr_opt = parser
	| [< e = parse_expr >] -> e
	| [< '(Next,p) >] -> EBlock [] , p

and parse_for p c = parser
	| [< '(Kwd In,_); v = parse_eval; '(PClose,p2); e = parse_expr_opt >] -> EForIn(c,v,wrap_var e) , punion p p2
	| [< cl = parse_for_conds; l1 = parse_eval_list; l2 = parse_eval_list; '(PClose,p2); e = parse_expr_opt >] -> EFor(c :: cl,l1,l2,wrap_var e) , punion p p2

and parse_for_conds = parser
	| [< '(Sep,_); e = parse_expr; l = parse_for_conds >] -> e :: l
	| [< '(Next,_) >] -> []
	| [< >] -> []

and parse_args = parser
	| [< '(Const (Ident name),_); t = parse_type_option; al , p = parse_args2 >] -> (name , t) :: al , p
	| [< '(PClose,p) >] -> [] , p

and parse_args2 = parser
	| [< '(Sep,_); '(Const (Ident name),_); t = parse_type_option; al , p = parse_args2 >] -> (name , t) :: al , p
	| [< '(PClose,p) >] -> [] , p

and parse_vars p = parser
	| [< '(Const (Ident name),p); t = parse_type_option; v = parse_var_init; vl , p = parse_vars_next p >] -> (name , t, v) :: vl , p
	| [< >] -> [] , p

and parse_vars_next p = parser
	| [< '(Sep,_); vl , p = parse_vars p >] -> vl , p
	| [< >] -> [] , p

and parse_var_init = parser
	| [< '(Binop OpAssign,_); v = parse_eval >] -> Some v
	| [< >] -> None

and parse_switch def = parser
	| [< '(BrClose,p) >] -> [] , p
	| [< '(Kwd Case,p); v = parse_eval; '(DblDot,_); c = parse_switch_clause; el, p2 = parse_switch def >] -> (Some v,(EBlock c,p)) :: el , p2
	| [< '(Kwd Default,p); '(DblDot,_); c = parse_switch_clause; el, p2 = parse_switch true >] ->
		if def then error Duplicate_default p;
		(None, (EBlock c,p)) :: el , p2

and parse_switch_clause = parser
	| [< e = parse_expr; el = parse_switch_clause >] -> e :: el
	| [< '(Next,_); el = parse_switch_clause >] -> el
	| [< >] -> []

and parse_block callb sp = parser
	| [< e = callb; el,p = parse_block callb sp >] -> e :: el , p
	| [< '(Next,_); el = parse_block callb sp >] -> el
	| [< '(BrClose,p) >] -> [] , p
	| [< '(Eof,_) >] -> error Unclosed_parenthesis sp

and parse_expr_list p = parser
	| [< e = parse_expr; el, p = parse_expr_list (pos e) >] -> e :: el , p
	| [< '(Next,_); el = parse_expr_list p >] -> el
	| [< >] -> [] , p

and parse_type_option = parser
	| [< '(DblDot,_); t = parse_class_path >] -> Some t
	| [< >] -> None

and parse_class_path s =
	last_comment := None;
	match s with parser
	| [< '(Const (Ident "Array"),_); s >] ->
		(match !last_comment with
		| None -> parse_class_path2 "Array" s
		| Some s -> ["#" ^ s] , "Array")
	| [< '(Const (Ident name),_); p = parse_class_path2 name >] -> p

and parse_class_path2 name = parser
	| [< '(Dot,_); p , n = parse_class_path >] -> name :: p , n
	| [< >] -> [] , name

and parse_import = parser
	| [< '(Const (Ident name),_); p = parse_import2 name >] -> p
	| [< '(Binop OpMult,_) >] -> [] , None

and parse_import2 name = parser
	| [< '(Dot,_); p , n = parse_import >] -> name :: p , n
	| [< >] -> [] , Some name

and parse_metadata = parser
	| [< '(BkClose,_) >] -> ()
	| [< '(_) ; () = parse_metadata >] -> ()

and parse_include = parser
	| [< '(Sharp,p1); '(Const (Ident "include"),_); '(Const (String inc),p2) >] ->
		let t = "ComponentVersion.as" in
		let tl = String.length t in
		if String.length inc < tl || String.sub inc (String.length inc - tl) tl <> t then (!warning) "unsupported #include" (punion p1 p2)

let parse code file =
	let old = Lexer.save() in
	Lexer.init file;
	let last = ref (Eof,null_pos) in
	let comments = ref [] in
	let rec next_token x =
		let t, p = Lexer.token code in
		match t with
		| Comment s | CommentLine s ->
			last_comment := Some s;
			comments := (s,p) :: !comments;
			next_token x
		| _ ->
			last := (t , p);
			Some (t , p)
	in
	try
		let l = parse_code (Stream.from next_token) in
		Lexer.restore old;
		l , List.rev !comments
	with
		| Stream.Error _
		| Stream.Failure ->
			Lexer.restore old;
			error (Unexpected (fst !last)) (pos !last)
		| e ->
			Lexer.restore old;
			raise e
