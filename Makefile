.PHONY: deps test

all: compile generate

compile: deps
	./rebar compile

generate:
	./rebar -r generate

deps:
	./rebar get-deps

clean:
	./rebar clean
	@rm -rf *~
	@rm -rf rel/serv

distclean: clean
	./rebar delete-deps

DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools \
	 crypto inets webtool public_key mnesia eunit syntax_tools compiler
