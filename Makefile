.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe
test:
	OCAMLRUNPARAM=b dune exec test/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

clean:
	dune clean
	
code:
	-dune build
	code .
	! dune build --watch

zip:
	rm -f dbms.zip
	zip -r dbms.zip . -x@exclude.lst