-module(serv).

-include("thrift/gen-erl/service_thrift.hrl").

-compile(export_all).

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).

ping() ->
    debug("ping()",[]),
    ok.