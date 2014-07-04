-module(serv_vnode).
-behaviour(riak_core_vnode).
-include("serv.hrl").

-export([start_vnode/1,
	 init/1,
	 terminate/2,
	 handle_command/3,
	 is_empty/1,
	 delete/1,
	 handle_handoff_command/3,
	 handoff_starting/2,
	 handoff_cancelled/1,
	 handoff_finished/2,
	 handle_handoff_data/2,
	 encode_handoff_item/2,
	 handle_coverage/4,
	 handle_exit/3]).

-ignore_xref([
	      start_vnode/1
	     ]).

-record(state, {partition}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

-ifdef(USE_POOL).
init([Partition]) ->
    WorkerPoolSize = app_helper:get_env(serv, worker_pool_size, 10),
    WorkerPool = {pool, serv_worker, WorkerPoolSize, []},
    {ok, #state{partition = Partition}, [WorkerPool]}.
-else.
init([Partition]) ->
    {ok, #state{partition = Partition}}.
-endif.
%% proxy of forward message
-ifdef(USE_POOL).
forward(Message, Sender, State) ->
    {async, {forward, Message}, Sender, State}.
-else.
forward(Message, _Sender, State) ->
    lager:info("forward mesasge: ~p", [Message]),
    {reply, forward, State}.
-endif.
%% forward message
handle_command({forward, Message}, Sender, State) ->
    forward(Message, Sender, State);
%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, pong, State};

handle_command(Message, _Sender, State) ->
    lager:notice("Unknown message: ~P", [Message]),
    {reply, not_impl, State}.

handle_handoff_command(Message, Sender, State) ->
    lager:notice("handle_handoff_command(~p, ~p, State)", [Message, Sender]),
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    %%lager:info("handoff_starting(~p, State)", [TargetNode]),
    {true, State}.

handoff_cancelled(State) ->
    lager:info("handoff_cancelled(State)"),
    {ok, State}.

handoff_finished(TargetNode, State) ->
    lager:info("handoff_finished(~p, State)", [TargetNode]),
    {ok, State}.

handle_handoff_data(Data, State) ->
    lager:info("handle_handoff_data(~p, State)", [Data]),
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
