.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f ocamlfinal.zip

zip:
	rm -f ocamlfinal.zip
	zip -r ocamlfinal.zip . -x@exclude.lst

docs:
	dune build @doc
