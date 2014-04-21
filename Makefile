.PHONY: deps test
REBAR = $(shell pwd)/rebar
ESCRIPT = /usr/local/bin/escript

all: deps compile

compile:
	${ESCRIPT} $(REBAR) compile

deps:
	${ESCRIPT} $(REBAR) get-deps

generate: compile
	${ESCRIPT} $(REBAR) generate

clean:
	${ESCRIPT} $(REBAR) clean
	@rm -rf *~
	@rm -rf rel/serv

distclean: clean
	${ESCRIPT} ./rebar delete-deps
