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
    %% lager:info("start_vnode(~p)", [I]),
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { partition=Partition }}.

%% Sample command: respond to a ping
handle_command(ping, Sender, State) ->
    lager:info("ping from: ~p", [Sender]),
    {noreply, State};

handle_command(_Message, _Sender, State) ->
    {noreply, State}.

handle_handoff_command(Message, Sender, State) ->
    lager:info("handle_handoff_command(~p, ~p, State)", [Message, Sender]),
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
