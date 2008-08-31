DESTDIR=
PREFIX=/usr/local
ZLIB=/usr/lib/libz.so

build:
	( cd ocaml/extc ; ocamlc extc_stubs.c ;\
	ocamlfind ocamlopt -package extlib -a -o extc.cmxa -cclib ../extc/extc_stubs.o -cclib $(ZLIB) extc.mli extc.ml )
	cd ocaml/swflib ; ocamlfind ocamlopt -package extlib -a -o swflib.cmxa -I .. -I ../extc swf.ml swfZip.ml actionScript.ml swfParser.ml
	( cd ocaml/mtasc ; ocamllex lexer.mll ;	ocamlopt -c expr.ml lexer.ml ; ocamlopt -c -pp camlp4o parser.ml ;\
	ocamlfind ocamlopt -package extlib -c -I .. -I ../extc -I ../swflib typer.ml class.ml plugin.ml genSwf.ml main.ml ;\
	ocamlfind ocamlopt -package extlib -linkpkg -o mtasc -cclib $(ZLIB) extLib.cmxa ../extc/extc.cmxa ../swflib/swflib.cmxa expr.cmx lexer.cmx parser.cmx typer.cmx class.cmx plugin.cmx genSwf.cmx main.cmx )

clean:
	rm -f ocaml/*/*.cma ocaml/*/*.cmi ocaml/*/*.cmo ocaml/*/*.cmx
	rm -f ocaml/*/*.cmxa ocaml/*/*.o ocaml/*/*.a
	rm -f ocaml/mtasc/lexer.ml ocaml/mtasc/mtasc

install:
	mkdir -p $(DESTDIR)$(PREFIX)/bin/ $(DESTDIR)$(PREFIX)/share/
	cp ocaml/mtasc/mtasc $(DESTDIR)$(PREFIX)/bin/
	cp -r ocaml/mtasc/std $(DESTDIR)$(PREFIX)/share/
	cp -r ocaml/mtasc/std8 $(DESTDIR)$(PREFIX)/share/

.PHONY: build install clean
