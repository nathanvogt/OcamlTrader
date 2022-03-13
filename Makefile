.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

# test:
# 	OCAMLRUNPARAM=b dune exec test/main.exe

# test_nathan:
# 	OCAMLRUNPARAM=b dune exec test/nathan.exe

test_michael:
 	OCAMLRUNPARAM=b dune exec test/michael.exe

# test_juntao:
# 	OCAMLRUNPARAM=b dune exec test/juntao.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f adventure.zip

doc:
	dune build @doc

check:
	@bash check.sh

finalcheck:
	@bash check.sh final
