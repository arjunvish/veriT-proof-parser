native:
	ocamlbuild -r -no-hygiene -use-menhir main.native

byte:
	ocamlbuild -r -no-hygiene main.d.byte

test:
	./runtests.sh

clean:
	ocamlbuild -clean
