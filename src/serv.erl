-module(serv).

-include("thrift/gen-erl/serv_thrift.hrl").

-compile(export_all).

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).

handle_function(Function, Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok ->
             ok;
        Else -> {reply, Else}
    end.

handle_error(undefined, Reason) ->
    debug("error: ~p", [Reason]).

'Ping'() ->
    debug("ping()",[]),
    ok.
