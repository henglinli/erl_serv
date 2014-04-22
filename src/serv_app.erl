-module(serv_app).

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

	    ok = riak_core_ring_events:add_guarded_handler(serv_ring_event_handler, []),
	    ok = riak_core_node_watcher_events:add_guarded_handler(serv_node_event_handler, []),
	    ok = riak_core_node_watcher:service_up(serv, self()),

	    EntryRoute = {["serv", "ping"], serv_wm_ping, []},
	    webmachine_router:add_route(EntryRoute),
   
	    {ok, Pid};
	{error, Reason} ->
	    {error, Reason}
    end.

stop(_State) ->
    ok.
