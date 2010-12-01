all: compile

compile:
	@ ./rebar compile

tests:
	@ ./rebar eunit

clean:
	@ ./rebar clean

bootstrap: compile
	@ erl -pz ebin -b start_sasl -noshell -s init stop -eval 'neotoma:bootstrap().'
	@ ./rebar compile