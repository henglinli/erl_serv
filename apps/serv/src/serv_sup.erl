-module(serv_sup).
-include("serv.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro
-define (IF (Bool, A, B), if Bool -> A; true -> B end).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    ServVnodeSpec = {?SERV,
		     {riak_core_vnode_master, start_link, [serv_vnode]},
		     Restart, Shutdown, worker, [riak_core_vnode_master]},

    ServMcSupSpec = {serv_mc_sup,
		     {serv_mc_sup, start_link, []},
		     Restart, Shutdown, supervisor, [serv_mc_sup]},

    %% riak_ensemble
    EnsemblesKV =  {serv_kv_ensembles,
		    {serv_kv_ensembles, start_link, []},
		    permanent, 30000, worker, [serv_kv_ensembles]},

    %% Build the process list...
    Processes = lists:flatten([
			       [EnsemblesKV || riak_core_sup:ensembles_enabled()],
			       ServMcSupSpec,
			       ServVnodeSpec
			      ]),

    {ok, {SupFlags, Processes}}.
