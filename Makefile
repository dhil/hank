#
# Makefile for the Hank Programming Language project
#

# Build system: ocamlbuild
OBC=ocamlbuild
# bin-annot is required for Merlin and other IDE-like tools
OBC_FLAGS=-tag bin_annot -use-ocamlfind -Is common,parsing -cflag -safe-string
# Default compiler
CC=$(OBC) $(OBC_FLAGS)
# Custom toplevel compiler
TC=ocamlmktop

all: native

check-compiler:
	@HANDLERS_ENABLED=`ocamlopt -version | grep multicore`; \
	if [ $$? -ne 0 ]; then \
		echo "error: the OCaml compiler version $$(ocamlopt -version) does not support effect handlers." > /dev/stderr; \
		exit 1; \
	fi

native: check-compiler
	$(CC) main.native

run-tests: tests
	./driver.native -list-test

tests: tests/driver.ml
	$(OBC) -Is common,parsing -use-ocamlfind -pkgs "oUnit,qcheck" tests/driver.native

top: _build/common/continuation.cmo _build/common/utils.cmo _build/parsing/hParse.cmo
	ocamlmktop -o hanktop -I _build/common/ -I _build/parsing continuation.cmo utils.cmo hParse.cmo

clean:
	$(CC) -clean

.PHONY: all clean native tests
