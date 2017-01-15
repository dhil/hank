#
# Makefile for the Hank Programming Language project
#

# Build system: ocamlbuild
OBC=ocamlbuild
# bin-annot is required for Merlin and other IDE-like tools
OBC_FLAGS=-tag bin_annot -use-ocamlfind -Is common,parsing -pkgs "cmdliner"
# Default compiler
CC=$(OBC) $(OBC_FLAGS)
# Custom toplevel compiler
TC=ocamlmktop

all: native

native: 
	$(CC) main.native

run-tests: tests
	./driver.native -list-test

tests: tests/driver.ml
	$(OBC) -Is common,parsing -use-ocamlfind -pkgs "oUnit,qcheck" tests/driver.native

top: _build/common/continuation.cmo _build/common/utils.cmo _build/parsing/eparse.cmo
	ocamlmktop -o hanktop -I _build/common/ -I _build/parsing continuation.cmo utils.cmo eparse.cmo

clean:
	$(CC) -clean

.PHONY: all clean native tests
