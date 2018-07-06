#
# Makefile for the Hank Programming Language project
#

all:
	jbuilder build -p hank -j 4 @install
	ln -fs _build/default/bin/main.exe hank

run-tests: tests
	./driver.native -list-test

clean:
	jbuilder clean

.PHONY: all clean native tests
