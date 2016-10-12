#
# Makefile for the Hank Programming Language project
#

# Build system: ocamlbuild
OBC=ocamlbuild
# bin-annot is required for Merlin and other IDE-like tools
OBC_FLAGS=-tag bin_annot -use-menhir -use-ocamlfind -I parsing
# Default compiler
CC=$(OBC) $(OBC_FLAGS)

all: native

native: 
	$(CC) main.native

clean:
	$(CC) -clean

.PHONY: all clean native
