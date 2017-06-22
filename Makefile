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

all: native byte

check-compiler:
	@HANDLERS_ENABLED=`ocamlopt -version | grep multicore`; \
	if [ $$? -ne 0 ]; then \
		echo "error: the OCaml compiler version $$(ocamlopt -version) does not support effect handlers." > /dev/stderr; \
		exit 1; \
	fi

byte: check-compiler
	$(CC) main.byte

native: check-compiler
	$(CC) main.native

run-tests: tests
	./driver.native -list-test

tests: tests/driver.ml
	$(OBC) -Is common,parsing -use-ocamlfind -pkgs "oUnit,qcheck" tests/driver.native

top: byte
#ocamlmktop -o hanktop -I _build/common/ -I _build/parsing continuation.cmo utils.cmo loc.cmo token.cmo lexer.cmo hParse.cmo
	ocamlmktop -o hanktop -I _build/common/ -I _build/parsing utils.cmo loc.cmo token.cmo

clean:
	$(CC) -clean

.PHONY: all clean native tests
