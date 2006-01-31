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

