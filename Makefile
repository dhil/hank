#
# Makefile for the Hank Programming Language project
#

all:
	dune build --only-packages hank --profile=development @install
	ln -fs _build/default/bin/main.exe hank

release:
	dune build --only-packages hank --profile=release @install

run-tests: tests
	./driver.native -list-test

clean:
	dune clean
	rm -f hank

.PHONY: all clean native tests
