#
# Makefile for the Hank Programming Language project
#

# Project root and build directory
ROOT:=$(shell dirname $(shell readlink -fn $(firstword $(MAKEFILE_LIST))))
BUILD_DIR:=$(ROOT)/_build

# The build command and some standard build system flags
BUILD=dune build
PACKAGES=hank
# Note: this relies on lazy expansion of `PACKAGES'.
COMMON_FLAGS=--only-packages $(PACKAGES) --build-dir=$(BUILD_DIR)
DEV_FLAGS=$(COMMON_FLAGS) --profile=development
REL_FLAGS=$(COMMON_FLAGS) --profile=release

.DEFAULT_GOAL:= all
.PHONY: all
all: build-dev link-executable

.PHONY: build-dev
build-dev: dune dune-project
	$(BUILD) $(DEV_FLAGS) @install

.PHONY: link-executable
link-executable: $(BUILD_DIR)/default/bin/main.exe
	ln -fs $(BUILD_DIR)/default/bin/main.exe hank

.PHONY: clean
clean:
	dune clean
	rm -f hank
