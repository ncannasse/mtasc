
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

type binop =
	| OpAdd
	| OpMult
	| OpDiv
	| OpSub
	| OpAssign
	| OpEq
	| OpNotEq
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
	| Name of string

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
	| Arrow
	| Quote of string

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

type type_path = string list * string

type func = {
	fname : string;
	fargs : (string * type_path option) list;
	ftype : type_path option;
	fstatic : static_flag;
	fpublic : public_flag;
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
	| EStatic of type_path
	| EParenthesis of eval
	| EObjDecl of (string * eval) list
	| EArrayDecl of eval list
	| ECall of eval * eval list
	| ENew of type_path * eval list
	| EUnop of unop * unop_flag * eval
	| EQuestion of eval * eval * eval
	| ELambda of func

and eval = eval_def * pos

and expr_def =
	| EVars of static_flag * public_flag * (string * type_path option * eval option) list
	| EFunction of func
	| EBlock of expr list
	| EFor of expr list * eval list * eval list * expr
	| EForIn of expr * eval * expr
	| EIf of eval * expr * expr option
	| EWhile of eval * expr * while_flag
	| ESwitch of eval * (eval * expr) list * expr option
	| EReturn of eval option
	| EBreak
	| EContinue
	| EVal of eval

and expr = expr_def * pos

and sign_def =
	| EClass of type_path * herit list * expr
	| EInterface of type_path * herit list * expr
	| EImport of type_path

and signature = sign_def * pos

let pos = snd

let is_postfix = function
	| Increment | Decrement -> true
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
	| Name s -> s
	
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

let rec s_binop = function
	| OpAdd -> "+"
	| OpMult -> "*"
	| OpDiv -> "/"
	| OpSub -> "-"
	| OpAssign -> "="
	| OpEq -> "=="
	| OpNotEq -> "!="
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
	| Arrow -> "->"
	| Quote s -> s 