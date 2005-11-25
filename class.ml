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

type vars = (string,static_flag) Hashtbl.t

type generated =
	| NotYet
	| Generating
	| Done

type context = {
	path : type_path;
	vars : vars;
	herits : herit list;
	exprs : signature list;
	expr : expr;
	classes : (type_path , context) Hashtbl.t;
	filename : string;
	is_interface : bool;
	mutable interfaces : context list;
	mutable superclass : context option;
	mutable constructor : func option;
	mutable generated : generated;
	mutable initvars : (string * static_flag * eval) list;
	mutable methods : func list;
}

let empty =  {
	path = ([],"<empty>");
	vars = Hashtbl.create 0;
	herits = [];
	expr = (EBlock [],null_pos);
	classes = Hashtbl.create 0;
	filename = "<empty>";
	is_interface = false;
	interfaces = [];
	superclass = None;
	constructor = None;
	generated = NotYet;
	initvars = [];
	exprs = [];
	methods = [];
}
	

let path clctx =
	clctx.path

let getclass clctx p =
	Hashtbl.find clctx.classes p

let filename clctx =
	clctx.filename

let expr clctx =
	clctx.expr

let full_exprs clctx =
	clctx.exprs

let constructor clctx =
	clctx.constructor

let superclass clctx =
	clctx.superclass

let methods clctx =
	clctx.methods

let initvars clctx =
	clctx.initvars

let intrinsic clctx =
	List.exists (( = ) HIntrinsic) clctx.herits

let interface clctx =
	clctx.is_interface

let interfaces clctx =
	clctx.interfaces

let is_getset clctx v =
	match clctx.superclass with
	| None -> false
	| Some c ->
		List.exists (fun f -> f.fname = v && f.fgetter <> Normal) c.methods

let rec resolve_supervar c name =
	match c.superclass with
	| None -> 
		assert false
	| Some c ->
		try
			if Hashtbl.find c.vars name = IsStatic then
				resolve_supervar c name
			else
				c.path
		with
			Not_found -> resolve_supervar c name


let generate_exprs h fname el =
	let add_class interf path herits e =
		Hashtbl.add h path {
			filename = fname;
			path = path;
			vars = Hashtbl.create 0;
			herits = herits;
			is_interface = interf;
			classes = h;
			constructor = None;
			superclass = None;
			interfaces = [];
			initvars = [];
			methods = [];
			generated = NotYet;
			expr = e;
			exprs = el;
		}
	in
	let rec loop (e,p) =
		match e with
		| EClass (path,herits,e) ->
			add_class false path herits e
		| EInterface (path,herits,e) ->
			add_class true path herits e
		| EImport _ ->
			()
	in
	List.iter loop el

let rec generate_class_vars h gen clctx (e,p) =
	match e with
	| EVars (static_flag,public_flag,vl) ->
		List.iter (fun (name,_,vinit) ->
			Hashtbl.add clctx.vars name static_flag;
			match vinit with
			| Some v ->
				clctx.initvars <- (name,static_flag,v) :: clctx.initvars;
				if static_flag = IsStatic then generate_class_static_refs h gen clctx v
			| _ -> ()
		) vl
	| EFunction f ->		
		if f.fname = snd clctx.path then
			clctx.constructor <- Some f
		else begin
			Hashtbl.add clctx.vars f.fname f.fstatic;
			clctx.methods <- f :: clctx.methods
		end
	| EBlock el ->
		List.iter (generate_class_vars h gen clctx) el
	| _ ->
		assert false

and generate_class_static_refs h gen clctx v =
	let check p =
		let clctx2 = (try Hashtbl.find h p with Not_found -> assert false) in
		if clctx2 != clctx then generate_class h gen clctx2
	in
	let rec loop (v,p) =
		match v with
		| EField (v,_)
		| EParenthesis v
		| EUnop (_,_,v) ->
			loop v
		| EArray (v1,v2) 
		| ECast (v1,v2) 
		| EBinop (_,v1,v2) ->
			loop v1;
			loop v2
		| EObjDecl vl ->
			List.iter (fun (_,v) -> loop v) vl
		| EArrayDecl vl ->
			List.iter loop vl
		| ECall (v,vl) ->
			List.iter loop (v :: vl)
		| EQuestion (v,v1,v2) ->
			loop v;
			loop v1;
			loop v2
		| EStatic p ->
			check p;
		| ENew (v,vl) ->
			List.iter loop (v :: vl)
		| EConst _ 
		| ELambda _ ->
			()
	in
	loop v

and generate_class h gen clctx =
	match clctx.generated with
	| Done -> ()
	| Generating -> prerr_endline ("Warning : loop in generation for class " ^ s_type_path clctx.path)
	| NotYet ->
		let generate_herit = function
			| HIntrinsic | HDynamic -> ()
			| HExtends path when not clctx.is_interface ->
				(try
					let hctx = Hashtbl.find h path in					
					clctx.superclass <- Some hctx;
					generate_class h gen hctx
				with
					Not_found -> assert false)
			| HExtends path 
			| HImplements path ->
				try
					let hctx = Hashtbl.find h path in
					clctx.interfaces <- hctx :: clctx.interfaces;
					generate_class h gen hctx
				with
					Not_found -> assert false
		in
		clctx.generated <- Generating;
		List.iter generate_herit clctx.herits;
		generate_class_vars h gen clctx clctx.expr;
		clctx.methods <- List.rev clctx.methods;
		clctx.initvars <- List.rev clctx.initvars;
		gen clctx;
		clctx.generated <- Done

let generate gen exprs =
	let h = Hashtbl.create 0 in
	Hashtbl.iter (fun fname el -> generate_exprs h fname el) exprs;
	Hashtbl.iter (fun _ cl -> generate_class h gen cl) h