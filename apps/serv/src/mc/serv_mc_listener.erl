%% @doc entry point for TCP-based memcached service

-module(serv_mc_listener).

-behaviour(serv_listener).

-export([start_link/2]).

-export([sock_opts/0, new_connection/1]).
-export([get_listeners/0]).


%% @doc Starts the PB listener
-spec start_link(inet:ip_address() | string(),  non_neg_integer())
		-> {ok, pid()} | {error, term()}.
start_link(IpAddr, PortNum) ->
    serv_listener:start_link(?MODULE, IpAddr, PortNum).

%% @doc Preferred socket options for the listener.
-spec sock_opts() -> [gen_tcp:option()].
sock_opts() ->
    BackLog = app_helper:get_env(serv, pb_backlog, 128),
    NoDelay = app_helper:get_env(serv, disable_pb_nagle, true),
    [binary, {packet, raw}, {reuseaddr, true},
     {backlog, BackLog}, {nodelay, NoDelay}].

%% @doc The connection initiation callback for gen_nb_server, called
%% when a new socket is accepted.
-spec new_connection(inet:socket()) -> ok | {error, Reason::term()}.
new_connection(Socket) ->
    case serv_mc_binary_server:start_link() of
	{ok, Pid} ->
	    ok = gen_tcp:controlling_process(Socket, Pid),
	    ok = serv_mc_binary_server:set_socket(Pid, Socket),
	    ok;
	Error ->
	    Error
    end.

get_listeners() ->
    Listeners = app_helper:get_env(serv, pb, [{"127.0.0.1", 8087}]),
    [ {I, P} || {I, P} <- Listeners ].
