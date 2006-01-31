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

type pos = {
	pfile : string;
	pmin : int;
	pmax : int;
}

type keyword =
	| Function
	| Class
	| Var
	| If
	| Else
	| While
	| Do
	| For
	| Break
	| Continue
	| Return
	| Interface
	| Extends
	| Implements
	| Import
	| Switch
	| Case
	| Default
	| Static
	| Intrinsic
	| Dynamic
	| Public
	| Private
	| Try
	| Catch
	| Finally
	| With
	| In
	| InstanceOf
	| New
	| This
	| Throw
	| Typeof
	| Delete
	| Void
	(* deprecated *)
	| Add | And | Or | Eq | Ne | KwdNot | Le | Lt | Ge | Gt
	| IfFrameLoaded | On | OnClipEvent | TellTarget
	
type binop =
	| OpAdd
	| OpMult
	| OpDiv
	| OpSub
	| OpAssign
	| OpEq
	| OpPhysEq
	| OpNotEq
	| OpPhysNotEq
	| OpGt
	| OpGte
	| OpLt
	| OpLte
	| OpAnd
	| OpOr
	| OpXor
	| OpBoolAnd
	| OpBoolOr
	| OpShl
	| OpShr
	| OpUShr
	| OpMod
	| OpAssignOp of binop

type unop =
	| Increment
	| Decrement
	| Not
	| Neg
	| NegBits

type constant =
	| Int of string
	| Float of string
	| String of string
	| Ident of string

type token =
	| Eof
	| Const of constant
	| Kwd of keyword
	| Comment of string
	| CommentLine of string
	| Binop of binop
	| Unop of unop
	| Next
	| Sep
	| BrOpen
	| BrClose
	| BkOpen
	| BkClose
	| POpen
	| PClose
	| Dot
	| DblDot
	| Question
	| Sharp

type unop_flag =
	| Prefix
	| Postfix

type while_flag =
	| NormalWhile
	| DoWhile

type static_flag = 
	| IsMember
	| IsStatic

type public_flag =
	| IsPublic
	| IsPrivate

type getter_flag =
	| Normal
	| Getter
	| Setter

type type_path = string list * string

type func = {
	fname : string;
	fargs : (string * type_path option) list;
	ftype : type_path option;
	fstatic : static_flag;
	fpublic : public_flag;
	fgetter : getter_flag;
	fexpr : expr option;
}

and herit = 
	| HExtends of type_path
	| HImplements of type_path
	| HIntrinsic
	| HDynamic

and eval_def =
	| EConst of constant
	| EArray of eval * eval
	| EBinop of binop * eval * eval
	| EField of eval * string
	| EParenthesis of eval
	| EObjDecl of (string * eval) list
	| EArrayDecl of eval list
	| ECall of eval * eval list
	| ENew of eval * eval list
	| EUnop of unop * unop_flag * eval
	| EQuestion of eval * eval * eval
	| ELambda of func
	| EStatic of type_path
	| ECast of eval * eval

and eval = eval_def * pos

and expr_def =
	| EVars of static_flag * public_flag * (string * type_path option * eval option) list
	| EFunction of func
	| EBlock of expr list
	| EFor of expr list * eval list * eval list * expr
	| EForIn of expr * eval * expr
	| EIf of eval * expr * expr option
	| EWhile of eval * expr * while_flag
	| ESwitch of eval * (eval option * expr) list
	| ETry of expr * (string * type_path option * expr) list ref * expr option
	| EWith of eval * expr
	| EReturn of eval option
	| EBreak
	| EContinue
	| EVal of eval

and expr = expr_def * pos

and sign_def =
	| EClass of type_path * herit list * expr
	| EInterface of type_path * herit list * expr
	| EImport of string list * string option

and signature = sign_def * pos

let pos = snd

let is_postfix (e,_) = function
	| Increment | Decrement -> (match e with EConst _ | EField _ | EStatic _ | EArray _ -> true | _ -> false)
	| Not | Neg | NegBits -> false

let is_prefix = function
	| Increment | Decrement -> true
	| Not | Neg | NegBits -> true

let base_class_name = snd

let null_pos = { pfile = "<null>"; pmin = -1; pmax = -1 }

let set_eval (e : eval) (v : eval_def) =
	Obj.set_field (Obj.repr e) 0 (Obj.repr v)

let punion p p2 =
	{
		pfile = p.pfile;
		pmin = min p.pmin p2.pmin;
		pmax = max p.pmax p2.pmax;
	}

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s

let s_escape s =
	let b = Buffer.create (String.length s) in
	for i = 0 to (String.length s) - 1 do
		match s.[i] with
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| c -> Buffer.add_char b c
	done;
	Buffer.contents b

let s_constant = function
	| Int s -> s
	| Float s -> s
	| String s -> "\"" ^ s_escape s ^ "\""
	| Ident s -> s
	
let s_keyword = function
	| Function -> "function"
	| Class -> "class"
	| Static -> "static"
	| Var -> "var"
	| If -> "if"
	| Else -> "else"
	| While -> "while"
	| Do -> "do"
	| For -> "for"
	| Break -> "break"
	| Return -> "return"
	| Continue -> "continue"
	| Interface -> "interface"
	| Extends -> "extends"
	| Implements -> "implements"
	| Import -> "import"
	| Switch -> "switch"
	| Case -> "case"
	| Default -> "default"
	| Intrinsic -> "intrinsic"
	| Dynamic -> "dynamic"
	| Private -> "private"
	| Public -> "public"
	| Try -> "try"
	| Catch -> "catch"
	| Finally -> "finally"
	| With -> "with"
	| In -> "in"
	| InstanceOf -> "instanceof"
	| New -> "new"
	| This -> "this"
	| Throw -> "throw"
	| Typeof -> "typeof"
	| Delete -> "delete"
	| Void -> "void"
	(* deprecated *)
	| Add -> "add"
	| And -> "and"
	| Or -> "or"
	| Eq -> "eq"
	| Ne -> "ne"
	| KwdNot -> "not"
	| Le -> "le"
	| Lt -> "lt"
	| Ge -> "ge"
	| Gt -> "gt"
	| IfFrameLoaded -> "ifFrameLoaded"
	| On -> "on"
	| OnClipEvent -> "onClipEvent"
	| TellTarget -> "tellTarget"

let rec s_binop = function
	| OpAdd -> "+"
	| OpMult -> "*"
	| OpDiv -> "/"
	| OpSub -> "-"
	| OpAssign -> "="
	| OpEq -> "=="
	| OpPhysEq -> "==="
	| OpNotEq -> "!="
	| OpPhysNotEq -> "!=="
	| OpGte -> ">="
	| OpLte -> "<="
	| OpGt -> ">"
	| OpLt -> "<"
	| OpAnd -> "&"
	| OpOr -> "|"
	| OpXor -> "^"
	| OpBoolAnd -> "&&"
	| OpBoolOr -> "||"
	| OpShr -> ">>"
	| OpUShr -> ">>>"
	| OpShl -> "<<"
	| OpMod -> "%"
	| OpAssignOp op -> s_binop op ^ "="

let s_unop = function
	| Increment -> "++"
	| Decrement -> "--"
	| Not -> "!"
	| Neg -> "-"
	| NegBits -> "~"

let s_token = function
	| Eof -> "<end of file>"
	| Const c -> s_constant c
	| Kwd k -> s_keyword k
	| Comment s -> "/*"^s^"*/"
	| CommentLine s -> "//"^s
	| Binop o -> s_binop o
	| Unop o -> s_unop o
	| Next -> ";"
	| Sep -> ","
	| BkOpen -> "["
	| BkClose -> "]"
	| BrOpen -> "{"
	| BrClose -> "}"
	| POpen -> "("
	| PClose -> ")"
	| Dot -> "."
	| DblDot -> ":"
	| Question -> "?"
	| Sharp -> "#"

exception Invalid_expression of pos

let rec check_val (v,p) =
	match v with
	| EBinop (OpAssign,_,_)
	| EBinop (OpAssignOp _,_,_)
	| ECall _
	| EUnop (Increment,_,_)
	| EUnop (Decrement,_,_)
	| EQuestion _
		-> ()
	| ENew _
	| EConst _
	| EArray _ 
	| EBinop _
	| EField _
	| EObjDecl _
	| EArrayDecl _
	| EUnop _
	| ELambda _
	| EStatic _
	| ECast _
		-> raise (Invalid_expression p)
	| EParenthesis v ->
		check_val v

let rec check_expr (e,p) =
	match e with
	| EVars (_,_,vl) -> ()
	| EFunction f -> 
		(match f.fexpr with None -> () | Some e -> check_expr e)
	| EBlock el ->
		List.iter check_expr el
	| EFor (el, _ , _ , e ) ->
		List.iter check_expr el;
		check_expr e
	| EForIn (_,_,e) ->
		check_expr e
	| EIf (_,e,eo) ->
		check_expr e;
		(match eo with None -> () | Some e -> check_expr e)
	| EWhile (_,e,_) ->
		check_expr e
	| ESwitch (_,cl) ->
		List.iter (fun (_,e) -> check_expr e) cl;
	| ETry (e,cl,eo) ->
		check_expr e;
		List.iter (fun (_,_,e) -> check_expr e) !cl;
		(match eo with None -> () | Some e -> check_expr e)
	| EWith (_,e) ->
		check_expr e;
	| EReturn _
	| EBreak
	| EContinue -> ()		
	| EVal v ->
		check_val v

let check_sign (s,p) = 
	match s with
	| EClass (_,_,e) -> check_expr e
	| EInterface (_,_,e) -> check_expr e
	| EImport _ -> ()
