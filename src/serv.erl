-module(serv).

-include("thrift/gen-erl/service_thrift.hrl").

-compile(export_all).

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).

ping() ->
    debug("ping()",[]),
    ok.

start() ->
    start(9999).

start(Port) ->
    Handler   = ?MODULE,
    thrift_socket_server:start([{handler, Handler},
                                {service, service_thrift},
                                {port, Port},
                                {name, serv},
				{framed, true}
			       ]).

stop(Server) ->
    thrift_socket_server:stop(Server).

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> {reply, Reply}
    end.
