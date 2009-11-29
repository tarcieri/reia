ERL=erl
ERLC=erlc
ERLC_OPT=+debug_info -W -o ebin

all: src_src

tests: clean src_src src_tests
	${ERL} -pz ebin -pz ebin_tests -pz ebin_tests/examples -b start_sasl -noshell -s init stop -eval 'test_suite:test().'

ebin:
	mkdir ebin

priv:
	mkdir priv

ebin_tests:
	mkdir ebin_tests

priv/peg_includes.erl: priv src/neotoma_peg.erl
	cat src/neotoma_peg.erl | grep -v "^%" | grep -v "^-" > priv/peg_includes.erl

src_src: ebin src/neotoma.app priv/peg_includes.erl
	cd src;erl -pz ../ebin -make

src_tests: ebin_tests
	cd tests;erl -pz ../ebin -make

src/neotoma.app: ebin
	cp src/neotoma.app ebin

clean:
	rm -rf ebin
	rm -rf ebin_tests

bootstrap: src_src
	${ERL} -pz ebin -b start_sasl -noshell -s init stop -eval 'neotoma:bootstrap().'
	cd src;erl -pz ../ebin -make
