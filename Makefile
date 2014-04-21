.PHONY: deps test
REBAR = $(shell pwd)/rebar
ESCRIPT = /usr/local/bin/escript

all: deps compile

compile:
	${ESCRIPT} $(REBAR) compile

deps:
	${ESCRIPT} $(REBAR) get-deps

generate:
	${ESCRIPT} $(REBAR) generate

clean:
	${ESCRIPT} $(REBAR) clean
	@rm -rf *~
	@rm -rf rel/serv

distclean: clean
	${ESCRIPT} ./rebar delete-deps

DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools \
	 crypto inets webtool public_key mnesia eunit syntax_tools compiler
