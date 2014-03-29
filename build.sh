#!/bin/sh
/usr/local/bin/escript rebar compile
cd rel
/usr/local/bin/escript ../rebar generate
