open Expr

type type_decl =
	| Var of class_context
	| Function of class_context list * class_context

and class_context = {
	path : type_path;
	name : string;
	interface : bool;
	fields : (string,type_decl) Hashtbl.t;
	super : class_context;
	implements : class_context list;
}

type context = {
	class_path : string list;
	files : (string,signature list) Hashtbl.t;
	classes : (type_path,class_context) Hashtbl.t;
	mutable imports : (string,type_path) Hashtbl.t;
}

type error_msg =
	| Class_not_found of type_path
	| Class_name_mistake of type_path

exception Error of error_msg * pos
exception File_not_found of string

let verbose = ref false

let error msg p = raise (Error (msg,p))

let error_msg = function
	| Class_not_found p -> "class not found : " ^ s_type_path p
	| Class_name_mistake p -> "class name mistake : should be " ^ s_type_path p

let create cpath = {
	class_path = cpath;
	files = Hashtbl.create 0;
	classes = Hashtbl.create 0;
	imports = Hashtbl.create 0;
}

let verbose_msg m =
	if !verbose then begin
		print_endline m;
		flush stdout;
	end

let type_file ctx req_path el =
	let old_imports = ctx.imports in
	ctx.imports <- Hashtbl.create 0;
	
	ctx.imports <- old_imports;
	assert false


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
				 | [] -> snd path ^ ".mt"
				 | _ -> String.concat "/" (fst path) ^ "/" ^ snd path ^ ".mt")
			in
			try
				type_file ctx path (load_file ctx file_name)
			with
				File_not_found _ -> error (Class_not_found path) p
