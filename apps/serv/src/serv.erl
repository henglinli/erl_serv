%% @doc Interface into the Real Time Statistics application.
-module(serv).
-include("serv.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0,
	 async_ping/0,
	 get_apl/2
	]).
-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

% @doc Pings a random vnode to make sure communication is functional
-spec ping(ping | {async, ping}) -> pong | pang | term().
ping(How) ->
    DocIdx = riak_core_util:chash_key({<<"Ping">>,
				       erlang:term_to_binary(os:timestamp())}),
    case riak_core_apl:get_apl(DocIdx, 1, ?SERV) of
	[] ->
	    pang;
	PrefList ->
	    [IndexNode| _Rest] = PrefList,
	    riak_core_vnode_master:sync_command(IndexNode, How, ?SERV)
    end.

-spec ping() -> pong | pang | term().
ping() ->
    ping(ping).

-spec async_ping() -> pong | pang | term().
async_ping() ->
    ping({async, ping}).

-spec get_apl(binary(), integer()) -> node().
get_apl(User, N) when erlang:is_binary(User) ->
    DocIdx = riak_core_util:chash_key({<<"User">>, User}),
    riak_core_apl:get_apl(DocIdx, N, ?SERV).
