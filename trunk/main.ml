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
open Printf
open ExtString

type p_style =
	| StyleJava
	| StyleMSVC

let print_style = ref StyleJava

let rec split l str =
	let rec loop = function
		| [] -> -1
		| x :: l ->
			try
				let p = String.index str x in
				let p2 = loop l in
				if p2 = -1 || p2 > p then
					p
				else
					p2
			with
				Not_found -> loop l
	in
	let p = loop l in
	if p = -1 then
		[str]
	else if p = 0 then
		split l (String.sub str 1 (String.length str - 1))
	else
		let sub = String.sub str 0 p in
		sub :: (split l (String.sub str (p+1) (String.length str - (p+1))))

let class_name file =
	let path = Filename.dirname file in
	let path = (match split ['/';'\\'] path with "." :: l -> l | l -> l) in
	let file = Filename.basename file in
	path , try
		Filename.chop_extension file
	with
		_ -> file

let normalize_path p =
	let l = String.length p in
	if l = 0 then
		"./"
	else match p.[l-1] with
		| '\\' | '/' -> p
		| _ -> p ^ "/"

let rec parse_class_path base_path path =
	if path = "" then
		[]
	else
		let relative fp l =
			if String.length fp >= 2 && (fp.[0] = '/' || fp.[0] = '.' || fp.[1] = ':') then
				fp :: l
			else
				fp :: (base_path ^ fp) :: l
		in
		try
			let p = String.index path ';' in
			let fp = normalize_path (String.sub path 0 (p-1)) in
			let remains = String.sub path (p+1) (String.length path - p - 1) in
			relative fp (parse_class_path base_path remains)
		with
			Not_found ->
				relative (normalize_path path) []

let read_package path =
	let npath = normalize_path path in
	let rec loop = function
		| [] ->
			Printf.eprintf "Warning : package %s not found\n" path;
			[]
		| cpath :: l ->
			let filepath = normalize_path (cpath ^ path) in
			match Array.to_list (try Sys.readdir filepath with Sys_error _ -> [||]) with
			| [] -> loop l
			| files ->
				match List.filter (fun f -> String.ends_with (String.lowercase f) ".as") files with
				| [] -> loop l
				| files -> List.map (fun f -> npath ^ f) files
	in
	loop !Plugin.class_path

let report ?(do_exit=true) (msg,p) etype printer =
	let error_printer file line =
		match !print_style with
		| StyleJava -> sprintf "%s:%d:" file line
		| StyleMSVC -> sprintf "%s(%d):" file line
	in
	let epos = Lexer.get_error_pos error_printer p in
	prerr_endline (sprintf "%s : %s %s" epos etype (printer msg));
	if do_exit then exit 1
;;
try
	let exe_ext = match Sys.os_type with "Win32" | "Cygwin" -> ".exe" | _ -> "" in
	let usage = "Motion-Twin ActionScript2 Compiler 1.14 - (c)2004-2008 Motion-Twin\n Usage : mtasc" ^ exe_ext ^ " [options] <files...>\n Options :" in
	let base_path = normalize_path (try Extc.executable_path() with _ -> ".") in
	let files = ref [] in
	let time = Sys.time() in
	Plugin.class_path := [base_path;"";"/"];
	let args_spec = [
		("-pack",Arg.String (fun path -> files := read_package path @ !files),"<path> : compile all files in target package");
		("-cp",Arg.String (fun path -> Plugin.class_path := parse_class_path base_path path @ !Plugin.class_path),"<paths> : add classpath");
		("-v",Arg.Unit (fun () -> Typer.verbose := true; Plugin.verbose := true),": turn on verbose mode");
		("-strict",Arg.Unit (fun () -> Typer.strict_mode := true),": turn on strict mode");
		("-infer",Arg.Unit (fun () -> Typer.local_inference := true),": turn on local variables inference");
		("-wimp",Arg.Unit (fun () -> Typer.warn_imports := true),": turn on warnings for unused imports");
		("-msvc",Arg.Unit (fun () -> print_style := StyleMSVC),": use MSVC style errors");
		("-mx",Arg.Unit (fun () ->
			Typer.use_components := true;
			Parser.use_components := true;
			GenSwf.use_components := true;
		),": use precompiled mx package");
	] @ !Plugin.options in
	Arg.parse args_spec (fun file -> files := file :: !files) usage;
	Plugin.class_path := (base_path ^ "std/") :: !Plugin.class_path;
	if (match !GenSwf.version with Some x -> x >= 8 | None -> false) then Plugin.class_path := (base_path ^ "std8/") :: !Plugin.class_path;
	Hashtbl.remove Lexer.keywords "add";
	Parser.warning := (fun msg pos -> report ~do_exit:false (msg,pos) "Warning" (fun msg -> msg));
	if !files = [] then begin
		Arg.usage args_spec usage
	end else begin
		if !Plugin.verbose then print_endline ("Classpath : " ^ (String.concat ";" !Plugin.class_path));
		let typer = (try
				Typer.create !Plugin.class_path
			with
				Typer.Error (Typer.Class_not_found ([],"StdPresent"),_) -> failwith "Directory 'std' containing MTASC class headers cannot be found :\nPlease install it or set classpath using '-cp' so it can be found.")
		in
		List.iter (fun file ->
			let path = class_name file in
			ignore(Typer.load_class typer path Typer.argv_pos);
		) (List.rev !files);
		Typer.finalize typer;
		List.iter (fun f -> f typer) !Plugin.calls;
		if !Plugin.verbose then print_endline ("Time spent : " ^ string_of_float (Sys.time() -. time));
	end;
with
	| Expr.Invalid_expression p -> report ((),p) "parse error" (fun() -> "Invalid Expression")
	| Lexer.Error (m,p) -> report (m,p) "syntax error" Lexer.error_msg
	| Parser.Error (m,p) -> report (m,p) "parse error" Parser.error_msg
	| Typer.Error (m,p) -> report (m,p) "type error" Typer.error_msg
	| Typer.File_not_found file ->
		prerr_endline (sprintf "File not found %s" file);
		exit 1
	| Failure msg ->
		prerr_endline msg;
		exit 1;
