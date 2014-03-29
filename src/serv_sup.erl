
-module(serv_sup).

-behaviour(supervisor).

-define(LOGIN_SERVER, 'theChat@127.0.0.1').

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

connect_to_login_server() ->
    case net_kernel:connect_node(?LOGIN_SERVER) of
	true ->
	    error_logger:info_msg("connected to ~p.~n", [?LOGIN_SERVER]);
	_ ->
	    error_logger:info_msg("connect to ~p failed.~n", [?LOGIN_SERVER])
    end.


init([]) ->
    connect_to_login_server(),
    Server = {thrift_socket_server,
	      {thrift_socket_server, start, [[{handler, serv},
					      {service, serv_thrift},
					      {port, 9999},
					      {name, serv},
					      {framed, true}
					     ]]},
	      permanent,
	      5000,
	      worker,
	      [thrift_socket_server]},
    {ok, { {one_for_one, 5, 10}, [Server]} }.
