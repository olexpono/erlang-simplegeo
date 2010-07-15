.PHONY: rel deps

ERL=erl
APPLICATION=mochierl_lib
DOC_OPTS={dir,\"../doc\"}

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

test:	all
	./rebar skip_deps=true eunit

docs:
	$(ERL) -noshell -pa ../ebin \
        -eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
        -s init stop
