-module(serv_sup).

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

init(_Args) ->
    EntryRoute = {["serv", "ping"], serv_wm_ping, []},
    webmachine_router:add_route(EntryRoute),
    VMaster = { serv_vnode_master,
                  {riak_core_vnode_master, start_link, [serv_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster]}}.
