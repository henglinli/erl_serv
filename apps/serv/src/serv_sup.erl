-module(serv_sup).
-include("serv.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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
                            Restart, Shutdown, worker, [serv_worker_pool_sup]},

    {ok, {SupFlags, [%RanchSupSpec,
                     %ListenerSpec,
                     ServVnodeSpec,
                     %ServFsmPoolSpec
                     %ServFsmSupSpec
                     ServWokerPoolSupSpec
                    ]}}.
