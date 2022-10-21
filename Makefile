.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe
test:
	OCAMLRUNPARAM=b dune exec test/main.exe


clean:
	dune clean
	
code:
	-dune build
	code .
	! dune build --watch