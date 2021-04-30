native:
	ocamlbuild -r -no-hygiene -use-menhir -menhir "menhir --unused-tokens" src/main.native

byte:
	ocamlbuild -r -no-hygiene -use-menhir -menhir "menhir --unused-tokens" src/main.byte

clean:
	ocamlbuild -clean