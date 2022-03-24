.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/michael.exe
# OCAMLRUNPARAM=b dune exec test/nathan.exe
# OCAMLRUNPARAM=b dune exec test/juntao.exe

# test_nathan:
# 	OCAMLRUNPARAM=b dune exec test/nathan.exe

# test_michael:
#  	OCAMLRUNPARAM=b dune exec test/michael.exe

# test_juntao:
# 	OCAMLRUNPARAM=b dune exec test/juntao.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f ocamlfinal.zip

zip:
	rm -f ocamlfinal.zip
	zip -r ocamlfinal.zip . -x@exclude.lst

doc:
	dune build @doc
