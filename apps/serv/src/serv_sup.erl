-module(serv_sup).
-include("serv.hrl").

-behaviour(supervisor).

-include_lib("rafter/include/rafter_opts.hrl").

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

    %% ServChatSpec = {serv_chat,
    %%              {serv_chat, start_link, []},
    %%              Restart, Shutdown, worker,
    %%              [serv_chat]},

    %% RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
    %%              Restart, Shutdown, Type, [ranch_sup]},

    %% ListenerSpec = ranch:child_spec(serv, 5,
    %%                              ranch_tcp, [{port, 9999}, {packet, 2}],
    %%                              serv_session,
    %%                              []),

    ServVnodeSpec = {?SERV,
		     {riak_core_vnode_master, start_link, [serv_vnode]},
		     Restart, Shutdown, worker, [riak_core_vnode_master]},
    %% ServFsmPoolSpec = serv_fsm_pool:pool_spec(),

    %% ServFsmSupSpec = {serv_fsm_sup,
    %%                {serv_fsm_sup, start_link, []},
    %%                Restart, Shutdown, supervisor, [serv_fsm_sup]},

    ServWokerPoolSupSpec = {serv_worker_pool_sup,
			    {serv_worker_pool_sup, start_link, []},
			    Restart, Shutdown, supervisor, [serv_worker_pool_sup]},

    ServPbSupSpec = {serv_pb_sup,
		     {serv_pb_sup, start_link, []},
		     Restart, Shutdown, supervisor, [serv_pb_sup]},

    %% Figure out which processes we should run...
    _HasStorageBackend = (app_helper:get_env(serv, storage_backend) /= undefined),
    %% rafter
    Backend = app_helper:get_env(serv, storage_backend, serv_rafter_backend_eleveldb),
    RafterDir = app_helper:get_env(serv, rafter_root, "rafter"),
    filelib:ensure_dir(RafterDir),
    Opts = #rafter_opts{state_machine=Backend, logdir=RafterDir},

    _Rafter = {serv_rafter_sup,
	      {rafter_consensus_sup, start_link,
	       [{serv_rafter, erlang:node()}, Opts]},
	      permanent, 5000, supervisor, [rafter_consensus_sup]},

    %% riak_ensemble
    EnsemblesKV =  {serv_kv_ensembles,
		    {serv_kv_ensembles, start_link, []},
		    permanent, 30000, worker, [serv_kv_ensembles]},


    %% Build the process list...
    Processes = lists:flatten([
			       %%?IF(HasStorageBackend, Rafter, []),
			       [EnsemblesKV || riak_core_sup:ensembles_enabled()],
			       ServWokerPoolSupSpec,
			       ServPbSupSpec,
			       ServVnodeSpec
			      ]),

    {ok, {SupFlags, Processes}}.
