-module(serv_app).
-include("serv.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    riak_core_util:start_app_deps(serv),
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
	    true = serv_pb_handler:register(5, serv_pb_handler_chat),
	    {ok, Pid};
	{error, Reason} ->
	    {error, Reason};
	_Other ->
	    {error, unknown}
    end.

stop(_State) ->
    true = serv_pb_handler:deregister(5),
    ok.
