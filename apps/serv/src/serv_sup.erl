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
    _Type = supervisor,

    %% ServSessionMapSpec = {serv_session_map,
    %%			  {serv_session_map, start_link, []},
    %%			  Restart, Shutdown, worker,
    %%			  [serv_session_map]},

    %% ServChatSpec = {serv_chat,
    %%		    {serv_chat, start_link, []},
    %%		    Restart, Shutdown, worker,
    %%		    [serv_chat]},

    %% RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
    %%		    Restart, Shutdown, Type, [ranch_sup]},

    %% ListenerSpec = ranch:child_spec(serv, 5,
    %%				    ranch_tcp, [{port, 9999}, {packet, 2}],
    %%				    serv_session,
    %%				    []),

    ServVMasterSpec = {?SERV_VMASTER,
    		   {riak_core_vnode_master, start_link, [serv_vnode]},
    		   Restart, Shutdown, worker, [riak_core_vnode_master]},

    %% Entry = {?ENTRY_VMASTER,
    %% 	     {riak_core_vnode_master, start_link, [serv_vnode_entry]},
    %% 	     Restart, Shutdown, worker, [riak_core_vnode_master]},

    %% Stat = {?STAT_VMASTER,
    %% 	    {riak_core_vnode_master, start_link, [serv_vnode_stat]},
    %% 	    Restart, Shutdown, worker, [riak_core_vnode_master]},

    FsmPut = {serv_fsm_put_sup,
	      {serv_fsm_put_sup, start_link, []},
	      Restart, infinity, supervisor, [serv_fsm_put_sup]},

    FsmGet = {serv_fsm_get_sup,
	      {serv_fsm_get_sup, start_link, []},
	      Restart, infinity, supervisor, [serv_fsm_get_sup]},

    {ok, {SupFlags, [%ServSessionMapSpec,
		     %ServChatSpec,
		     %RanchSupSpec,
		     %ListenerSpec,
		     ServVMasterSpec,
		     %% Entry,
		     %% Stat,
		     FsmPut,
		     FsmGet
		    ]}}.