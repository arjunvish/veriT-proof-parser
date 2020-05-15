native:
	ocamlbuild -r -no-hygiene -use-menhir main.byte

byte:
	ocamlbuild -r -no-hygiene main.d.byte

test:
	./runtests.sh

clean:
	ocamlbuild -clean
