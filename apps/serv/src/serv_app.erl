-module(serv_app).
-include("serv.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %riak_core_util:start_app_deps(serv),
    case serv_sup:start_link() of
	{ok, Pid} ->
	    ok = riak_core:register(serv, [{vnode_module, serv_vnode}]),
	    ok = riak_core_node_watcher:service_up(?SERV_VMASTER, self()),
	    ok = riak_core_ring_events:add_guarded_handler(serv_ring_event_handler, []),
	    ok = riak_core_node_watcher_events:add_guarded_handler(serv_node_event_handler, []),
	    %% ok = riak_core:register(serv, [{vnode_module, serv_vnode_stat}]),
	    %% ok = riak_core_node_watcher:service_up(?STAT_SERVICE, self()),

	    %% ok = riak_core:register(serv, [{vnode_module, serv_vnode_entry}]),
	    %% ok = riak_core_node_watcher:service_up(?ENTRY_SERVICE, self()),
	    %% EntryRoute = {["serv", "ping"], serv_wm_ping, []},
	    %% webmachine_router:add_route(EntryRoute),
	    {ok, Pid};
	{error, Reason} ->
	    {error, Reason};
	_Other ->
	    {error, unknown}
    end.

stop(_State) ->
   ok.

%% -spec start_api(Services::riak_api_pb_service:registration()) -> 
%% 		       supervisor:startlink_ret().
%% start_api(Services) ->    
%%     riak_core_util:start_app_deps(riak_api),
    
%%     case riak_api_sup:start_link() of
%%         {ok, Pid} ->
%%             riak_core:register(riak_api, [{stat_mod, riak_api_stat}]),
%%             ok = riak_api_pb_service:register(Services),
%%             {ok, Pid};
%%         {error, Reason} ->
%%             {error, Reason}
%%     end.

%% -spec stop_api(Services::riak_api_pb_service:registration()) -> ok.
%% stop_api(Services) ->
%%     ok = riak_api_pb_service:deregister(Services),
%%     ok.
