%% @doc Interface into the Real Time Statistics application.
-module(serv).
-include("serv.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
	 ping/0,
	 entry/2,
	 get/2,
	 set/3,
	 append/3,
	 incr/2,
	 incrby/3,
	 sadd/3
	]).
-export([get_dbg_preflist/2,
	 get_dbg_preflist/3]).
-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

% @doc Pings a random vnode to make sure communication is functional
-spec ping() -> ok | {error, Reson::term()}.
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    case riak_core_apl:get_apl(DocIdx, ?N, ?SERV_VMASTER) of
	[] ->
	    {error, "server down"};
	PrefList ->
	    riak_core_vnode_master:command(PrefList, ping, ?SERV_VMASTER)
    end.

%% @doc Process an entry.
entry(Client, Entry) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
				       term_to_binary(now())}),
    PrefList = riak_core_apl:get_apl(DocIdx, 1, ?ENTRY_SERVICE),
    [IdxNode] = PrefList,
    serv_vnode_entry:entry(IdxNode, Client, Entry).

%% @doc Get a stat's value.
get(Client, StatName) ->
    {ok, ReqID} = serv_fsm_get:get(Client, StatName),
    wait_for_reqid(ReqID, ?TIMEOUT).

get_dbg_preflist(Client, StatName) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
				       list_to_binary(StatName)}),
    riak_core_apl:get_apl(DocIdx, ?N, ?ENTRY_SERVICE).

get_dbg_preflist(Client, StatName, N) ->
    IdxNode = lists:nth(N, get_dbg_preflist(Client, StatName)),
    {ok, req_id, Val} =
	riak_core_vnode_master:command(IdxNode,
				       {get, req_id, StatName},
				       ?ENTRY_VMASTER),
    [IdxNode, Val].

%% @doc Set a stat's value, replacing the current value.
set(Client, StatName, Val) ->
    do_put(Client, StatName, set, Val).

%% @doc Append to a stat's value.
append(Client, StatName, Val) ->
    do_put(Client, StatName, append, Val).

%% @doc Increment the stat's value by 1.
incr(Client, StatName) ->
    do_put(Client, StatName, incr).

%% @doc Increment the stat's value by Val.
incrby(Client, StatName, Val) ->
    do_put(Client, StatName, incrby, Val).

%% @doc Add a memeber to the stat's set.
sadd(Client, StatName, Val) ->
    do_put(Client, StatName, sadd, Val).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
do_put(Client, StatName, Op) ->
    {ok, ReqID} = serv_fsm_put:put(Client, StatName, Op),
    wait_for_reqid(ReqID, ?TIMEOUT).

do_put(Client, StatName, Op, Val) ->
    {ok, ReqID} = serv_fsm_put:put(Client, StatName, Op, Val),
    wait_for_reqid(ReqID, ?TIMEOUT).

wait_for_reqid(ReqID, Timeout) ->
    receive
	{ReqID, ok} -> ok;
	{ReqID, ok, Val} -> {ok, Val}
    after Timeout ->
	    {error, timeout}
    end.
