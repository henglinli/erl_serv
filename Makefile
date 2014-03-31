.PHONY: deps test

ESCRIPT = /usr/local/bin/escript

all: compile generate

compile: deps
	${ESCRIPT} ./rebar compile

generate:
	${ESCRIPT} ./rebar -r generate

deps:
	${ESCRIPT} ./rebar get-deps

clean:
	${ESCRIPT} ./rebar clean
	@rm -rf *~
	@rm -rf rel/serv

distclean: clean
	${ESCRIPT} ./rebar delete-deps

DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools \
	 crypto inets webtool public_key mnesia eunit syntax_tools compiler
