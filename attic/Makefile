VERSION = 
PREFIX = /usr/local

# todo: get erlang dir with code:lib_dir()
ERLANG_LIB = $(PREFIX)/lib/erlang/lib
REIA_LIB = $(ERLANG_LIB)/reia$(VERSION)

PARSER_SRC = src/**/*.xrl src/**/*.yrl
ERL_SRC = src/compiler/*.erl src/builtins/*.erl src/core/*.erl
REIA_SRC = src/builtins/*.re src/core/*.re

REIA_FILE = 

# todo: erlang version check
all: build test

build: leex parser reia

erl_compile:
	bin/erlc -o ebin +debug_info $(ERL_SRC)

reia_compile:
	bin/reiac ${REIA_SRC}

reia: erl_compile reia_compile ebin_mv

ebin_mv:
	mv *.beam ebin

# Leex (lexer generator for Erlang)
leex: src/leex/leex.beam ebin/reia_scan.beam

src/leex/leex.beam:
	erlc -W0 -o src/leex src/leex/leex.erl

# Compile reia_scan using leex
ebin/reia_scan.beam: src/leex/leex.beam
	bin/leex src/compiler/reia_scan.xrl
	erlc +debug_info +nowarn_unused_vars -o ebin src/compiler/reia_scan.erl

parser: ebin/reia_parse.beam

ebin/reia_parse.beam:
	bin/yecc src/compiler/reia_parse.yrl
	erlc +debug_info -o ebin src/compiler/reia_parse.erl

test: build
	bin/reia test/runner.re

install: uninstall
	mkdir $(REIA_LIB)
  
	cp LICENSE $(REIA_LIB)
	cp README.textile $(REIA_LIB)
	cp -r ebin $(REIA_LIB)
	cp -r src $(REIA_LIB)
	cp -r lib $(REIA_LIB)

	cp bin/ire $(PREFIX)/bin
	cp bin/reia $(PREFIX)/bin

uninstall:
	-rm -r $(REIA_LIB)
	-rm $(PREFIX)/bin/ire
	-rm $(PREFIX)/bin/reia

clean: distclean

distclean:
	rm -f ebin/*
	rm -f src/compiler/reia_scan.erl src/compiler/reia_parse.erl

ci: distclean test

cruise: ci
