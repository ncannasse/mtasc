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

let error_msg = function
	| Unexpected t -> "Unexpected "^(s_token t)
	| Unclosed_parenthesis -> "Unclosed parenthesis"
	| Duplicate_default -> "Duplicate default"

let error m p = raise (Error (m,p))

let priority = function
	| OpAssign | OpAssignOp _ -> -3
	| OpBoolAnd | OpBoolOr -> -2
	| OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte | OpPhysEq | OpPhysNotEq -> -1
	| OpAdd | OpSub -> 0
	| OpMult | OpDiv -> 1
	| OpOr | OpAnd | OpXor -> 2
	| OpShl | OpShr | OpMod | OpUShr -> 3

let is_not_assign = function
	| OpAssign | OpAssignOp _ -> false
	| _ -> true

let rec make_binop op e ((v,p2) as e2) =
	match v with
	| EBinop (_op,_e,_e2) when priority _op <= priority op && (is_not_assign _op || is_not_assign op) ->
		let _e = make_binop op e _e in
		EBinop (_op,_e,_e2) , punion (pos _e) (pos _e2)
	| EQuestion (_e,_e1,_e2) when is_not_assign op ->
		EQuestion ( (EBinop(op,e,_e) , punion (pos e) (pos _e)) , _e1 , _e2) , punion (pos e) (pos _e2)
	| _ ->
		EBinop (op,e,e2) , punion (pos e) (pos e2)

let rec make_unop op ((v,p2) as e) p1 = 
	match v with
	| EBinop (bop,e,e2) -> EBinop (bop, make_unop op e p1 , e2) , (punion p1 p2)
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
	| [< flags = parse_field_flags IsMember IsPublic; f = parse_class_field flags interf; fl , p = parse_class interf >] -> f :: fl , p

and parse_field_flags stat pub = parser
	| [< '(Kwd Static,_); f = parse_field_flags IsStatic pub >] -> f
	| [< '(Kwd Public,_); f = parse_field_flags stat IsPublic >] -> f
	| [< '(Kwd Private,_); f = parse_field_flags stat IsPrivate >] -> f
	| [< >] -> stat , pub

and parse_class_field (stat,pub) interf = parser
	| [< '(Kwd Var,p1); vl, p2 = parse_vars p1 >] -> EVars (stat,pub,vl) , punion p1 p2
	| [< '(Kwd Function,p1); '(Const (Ident name),_); name , g = parse_getter name; '(POpen,_); args , p2 = parse_args; t = parse_type_option; s >] -> 
		EFunction {
			fname = name;
			fargs = args;
			ftype = t;
			fstatic = stat;
			fpublic = pub;
			fgetter = g;
			fexpr = if interf then None else Some (parse_expr s);
		} , punion p1 p2

and parse_expr = parser
	| [< '(BrOpen,p1); el , p2 = parse_block parse_expr p1 >] -> EBlock el , punion p1 p2
	| [< '(Kwd For,p); '(POpen,_); c = parse_expr; e = parse_for p c >] -> e
	| [< '(Kwd If,p); cond = parse_eval; e = parse_expr; e2 , p2 = parse_else (pos e) >] -> EIf (cond,e,e2), punion p p2
	| [< '(Kwd Return,p); v , p2 = parse_eval_option p; >] -> EReturn v , punion p p2
	| [< '(Kwd Break,p); >] -> EBreak , p
	| [< '(Kwd Continue,p); >] -> EContinue , p
	| [< '(Kwd While,p1); v = parse_eval; e = parse_expr >] -> EWhile (v,e,NormalWhile) , punion p1 (pos e)
	| [< '(Kwd Do,p1); e = parse_expr; '(Kwd While,_); v = parse_eval; >] -> EWhile (v,e,DoWhile) , punion p1 (pos v)
	| [< '(Kwd Switch,p1); v = parse_eval; '(BrOpen,_); el , eo, p2 = parse_switch >] -> ESwitch (v,el,eo) , punion p1 p2
	| [< '(Kwd Var,p1); vl, p2 = parse_vars p1 >] -> EVars (IsMember,IsPublic,vl), punion p1 p2
	| [< '(Kwd Try,p1); e = parse_expr; c = parse_catches; f = parse_finally >] -> ETry (e,c,f) , punion p1 (pos e)
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
	| [< '(Const (Ident "delete"),p1); e = parse_delete (EConst (Ident "delete"),p1) >] -> e
	| [< '(Const (Ident "typeof"),p1); e = parse_delete (EConst (Ident "typeof"),p1) >] -> e
	| [< '(Const (Ident "new"),p1); p = parse_class_path; e = parse_new (EStatic p,p1) p1 >] -> e
	| [< '(Const c,p); e = parse_eval_next (EConst c,p)  >] -> e
	| [< '(POpen,p1); e = parse_eval; '(PClose,p2); e = parse_eval_next (EParenthesis e , punion p1 p2) >] -> e
	| [< '(BrOpen,p1); el, p2 = parse_field_list; e = parse_eval_next (EObjDecl el, punion p1 p2) >] -> e
	| [< '(BkOpen,p1); el, p2 = parse_array; e = parse_eval_next (EArrayDecl el,punion p1 p2) >] -> e
	| [< '(Unop op,p1) when is_prefix op; e = parse_eval >] -> make_unop op e p1
	| [< '(Binop OpSub,p1); e = parse_eval >] -> make_unop Neg e p1
	| [< _ = parse_include; e = parse_eval_next (EObjDecl [],null_pos) >] -> e

and parse_eval_next e = parser
	| [< '(BkOpen,_); e2 = parse_eval; '(BkClose,p2); e = parse_eval_next (EArray (e,e2) , punion (pos e) p2) >] -> e
	| [< '(Binop op,_); e2 = parse_eval; >] -> make_binop op e e2
	| [< '(Dot,_); '(Const (Ident field),p2); e = parse_eval_next (EField (e,field), punion (pos e) p2) >] -> e
	| [< '(POpen,_); args = parse_eval_list; '(PClose,p2); e = parse_eval_next (ECall (e,args), punion (pos e) p2) >] -> e
	| [< '(Unop op,p2) when is_postfix op; e = parse_eval_next (EUnop (op,Postfix,e), punion (pos e) p2) >] -> e
	| [< '(Question,_); v1 = parse_eval; '(DblDot,_); v2 = parse_eval; e = parse_eval_next (EQuestion (e,v1,v2), punion (pos e) (pos v2)) >] -> e
	| [< '(Const (Ident "instanceof"),p); v = parse_eval; e = parse_eval_next (ECall ((EConst (Ident "instanceof"),p),[e;v]),punion (pos e) (pos v)) >] -> e
	| [< >] -> e

and parse_delete v = parser
	| [< e = parse_eval >] -> ECall (v , [e]) , punion (pos e) (pos v)
	| [< e = parse_eval_next v >] -> e 

and parse_new e p1 = parser
	| [< '(POpen,_); args = parse_eval_list; '(PClose,p2); e = parse_eval_next (ENew (e,args), punion p1 p2) >] -> e	

and parse_catches = parser
	| [< '(Kwd Catch,_); '(POpen,_); '(Const (Ident name),_); t = parse_type_option; '(PClose,_); e = parse_expr; l = parse_catches >] -> (name, t, e) :: l
	| [< >] -> []

and parse_finally = parser
	| [< '(Kwd Finally,_); e = parse_expr >] -> Some e
	| [< >] -> None

and parse_expr_option p = parser
	| [< e = parse_expr >] -> Some e , pos e
	| [< >] -> None, p

and parse_eval_option p = parser
	| [< v = parse_eval >] -> Some v , pos v
	| [< >] -> None, p

and parse_eval_list = parser
	| [< v = parse_eval; vl = parse_eval_list >] -> v :: vl
	| [< '(Sep,_); vl = parse_eval_list >] -> vl
	| [< '(Next,_) >] -> []
	| [< >] -> []

and parse_field_list = parser
	| [< '(Const (Ident fname),_); '(DblDot,_); e = parse_eval; el , p = parse_field_list >] -> (fname,e) :: el , p
	| [< '(Sep,_); el = parse_field_list >] -> el
	| [< '(BrClose,p) >] -> [] , p

and parse_array = parser
	| [< e = parse_eval; el , p = parse_array >] -> e :: el , p
	| [< '(Sep,_); e = parse_array >] -> e
	| [< '(BkClose,p) >] -> [] , p

and parse_else p = parser
	| [< '(Next,_); e = parse_else p >] -> e
	| [< '(Kwd Else,_); e = parse_expr >] -> Some e, pos e
	| [< >] -> None , p

and parse_for p c = parser
	| [< '(Const (Ident "in"),_); v = parse_eval; '(PClose,p2); e = parse_expr >] -> EForIn(c,v,e) , punion p p2
	| [< cl = parse_for_conds; l1 = parse_eval_list; l2 = parse_eval_list; '(PClose,p2); e = parse_expr >] -> EFor(c :: cl,l1,l2,e) , punion p p2

and parse_for_conds = parser
	| [< '(Sep,_); e = parse_expr; l = parse_for_conds >] -> e :: l
	| [< '(Next,_) >] -> []

and parse_args = parser
	| [< '(Const (Ident name),_); t = parse_type_option; al , p = parse_args >] -> (name , t) :: al , p
	| [< '(Sep,_); al= parse_args >] -> al
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

and parse_switch = parser
	| [< '(BrClose,p) >] -> [] , None , p
	| [< '(Kwd Case,p); v = parse_eval; '(DblDot,_); c = parse_switch_clause; el, eo, p2 = parse_switch >] -> (v,(EBlock c,p)) :: el , eo , p2
	| [< '(Kwd Default,p); '(DblDot,_); c = parse_switch_clause; el, eo, p2 = parse_switch >] -> 
		if eo <> None then error Duplicate_default p;
		el , Some (EBlock c,p) , p2

and parse_switch_clause = parser
	| [< e = parse_expr; el = parse_switch_clause >] -> e :: el
	| [< '(Next,_); el = parse_switch_clause >] -> el
	| [< >] -> []

and parse_block callb sp = parser
	| [< e = callb; el,p = parse_block callb sp >] -> e :: el , p
	| [< '(Next,_); el = parse_block callb sp >] -> el
	| [< '(BrClose,p); >] -> [] , p
	| [< '(Eof,_) >] -> error Unclosed_parenthesis sp

and parse_expr_list p = parser
	| [< e = parse_expr; el, p = parse_expr_list (pos e) >] -> e :: el , p
	| [< '(Next,_); el = parse_expr_list p >] -> el
	| [< >] -> [] , p

and parse_type_option = parser
	| [< '(DblDot,_); t = parse_class_path >] -> Some t
	| [< >] -> None

and parse_class_path = parser
	| [< '(Const (Ident name),_); p = parse_class_path2 name >] -> p

and parse_class_path2 name = parser
	| [< '(Dot,_); p , n = parse_class_path >] -> name :: p , n
	| [< >] -> [] , name

and parse_import = parser
	| [< '(Const (Ident name),_); p = parse_import2 name >] -> p
	| [< '(Binop OpMult,_); >] -> [] , None

and parse_import2 name = parser
	| [< '(Dot,_); p , n = parse_import >] -> name :: p , n
	| [< >] -> [] , Some name

and parse_metadata = parser
	| [< '(BkClose,_) >] -> ()
	| [< '(_) ; () = parse_metadata >] -> ()

and parse_getter name = parser
	| [< '(Const (Ident fname),_); >] -> 
		fname, (match name with
		| "get" -> Getter
		| "set" -> Setter
		| _ -> raise Stream.Failure)
	| [< >] -> name , Normal

and parse_include = parser 
	| [< '(Sharp,p1); '(Const (Ident "include"),_); '(Const (String _),p2) >] ->
		print_endline ("Warning : unsupported #include in " ^ p1.pfile)

let parse code file =
	let old = Lexer.save() in
	Lexer.init file;
	let last = ref (Eof,null_pos) in
	let comments = ref [] in
	let rec next_token x =
		let t, p = Lexer.token code in
		match t with 
		| Comment s | CommentLine s -> 
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
