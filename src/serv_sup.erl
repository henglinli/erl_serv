
-module(serv_sup).

-behaviour(supervisor).

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

init([]) ->
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
