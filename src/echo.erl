-module(echo).

-include("thrift/gen-erl/echo_thrift.hrl").

-compile(export_all).

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).

ping() ->
    debug("ping()",[]),
    ok.