-module(client).

-include("thrift/gen-erl/echo_thrift.hrl").

-export([t/0]).

t() ->
    Port = 9999,

    {ok, Client0} = thrift_client_util:new("127.0.0.1",
                                           Port,
                                           echo_thrift,
                                           [{framed, true}]),

    {_Client1, {ok, ok}} = thrift_client:call(Client0, ping, []),
    ok.