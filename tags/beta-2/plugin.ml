
let verbose = ref false

let options = ref ([] : (string * Arg.spec * string) list)

let calls = ref ([] : (Typer.context -> unit) list)

let class_path = ref ([] : string list)

let find_file f =
	let rec loop = function
		| [] -> raise Not_found
		| p :: l ->
			let file = p ^ f in
			if Sys.file_exists file then
				file
			else
				loop l
	in
	loop !class_path

let add l f = 
	options := l @ !options;
	calls := f :: !calls

