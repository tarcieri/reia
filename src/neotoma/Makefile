ERL=erl
ERLC=erlc
ERLC_OPT=+debug_info -W -o ebin

all: src_src

tests: clean src_src src_tests
	${ERL} -pz ebin -pz ebin_tests -pz ebin_tests/examples -b start_sasl -noshell -s init stop -eval 'test_suite:test().'

ebin:
	mkdir ebin

ebin_tests:
	mkdir ebin_tests

ebin_tests/examples:
	mkdir ebin_tests/examples

special: src/herml_scan.erl src/herml_parse.erl

src_src: ebin src/neotoma.app
	cd src;erl -pz ../ebin -make

src_tests: ebin_tests test_examples
	cd tests;erl -pz ../ebin -make

src/neotoma.app: ebin
	cp src/neotoma.app ebin

test_examples: ebin_tests/examples
	cd tests/examples;erl -pz ../../ebin -make

clean:
	rm -rf ebin
	rm -rf ebin_tests

bootstrap: src_src
	${ERL} -pz ebin -b start_sasl -noshell -s init stop -eval 'peg_gen:bootstrap().'
	cd src;erl -pz ../ebin -make