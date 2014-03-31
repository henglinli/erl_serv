
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

init([]) ->
    RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
		    permanent, 5000, supervisor, [ranch_sup]},    
    ListenerSpec = ranch:child_spec(echo_serv, 100,
				    ranch_tcp, [{port, 9999}],
				    echo, []),
    {ok, {{one_for_one, 10, 10}, [RanchSupSpec, ListenerSpec]}}.
