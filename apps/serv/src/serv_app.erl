-module(serv_app).
-include("serv.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_LIMIT, 100000).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    riak_core_util:start_app_deps(serv),
    %% start sidejob supervisor
    case app_helper:get_env(serv, client_limit, ?DEFAULT_LIMIT) of
	undfined ->
	    ok;
	Limit ->
	    sidejob:new_resource(serv_pb_server_sj, sidejob_supervisor, Limit)
    end,

    case serv_sup:start_link() of
	{ok, Pid} ->
	    ok = riak_core:register(serv, [{vnode_module, serv_vnode},
					   {stat_mod, serv_pb_stat}
					  ]),
	    %% ok = riak_core_node_watcher:service_up(?SERV, self()),
	    ok = riak_core_ring_events:add_guarded_handler(serv_event_handler_ring, []),
	    ok = riak_core_node_watcher_events:add_guarded_handler(serv_event_handler_node, []),
	    %% EntryRoute = {["serv", "ping"], serv_wm_ping, []},
	    %% webmachine_router:add_route(EntryRoute),
	    true = serv_pb_handler:register(?CHAT_CODE, serv_pb_handler_chat),
	    {ok, Pid};
	{error, Reason} ->
	    {error, Reason};
	_Other ->
	    {error, unknown}
    end.

stop(_State) ->
    true = serv_pb_handler:deregister(?CHAT_CODE),
    ok.
