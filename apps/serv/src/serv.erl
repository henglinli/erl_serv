%% @doc Interface into the Real Time Statistics application.
-module(serv).
-include("serv.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0,
	 get_apl/3,
	 get_apl_user/2,
	 sync_send/3,
	 send_one/0
	]).

%%%===================================================================
%%% API
%%%===================================================================

% @doc Pings a random vnode to make sure communication is functional
-spec ping() -> pong | pang | term().
ping() ->
    DocIdx = riak_core_util:chash_key({?PING,
				       erlang:term_to_binary(os:timestamp())}),
    case riak_core_apl:get_apl(DocIdx, 1, ?SERV) of
	[] ->
	    pang;
	PrefList ->
	    [IndexNode| _Rest] = PrefList,
	    riak_core_vnode_master:sync_command(IndexNode, ping, ?SERV, ?TIMEOUT)
    end.

-spec get_apl(binary(), binary(), integer()) -> node().
get_apl(Bucket, Key, N)
  when erlang:is_binary(Bucket) andalso erlang:is_binary(Key) ->
    DocIdx = riak_core_util:chash_key({Bucket, Key}),
    riak_core_apl:get_apl(DocIdx, N, ?SERV).

get_apl_user(Name, N)
  when erlang:is_binary(Name) ->
    get_apl(?USER, Name, N).

-spec sync_send(ToWho :: binary(), Message :: binary(), N :: integer()) ->
			  forword | save | {error, Reason :: term()}.
sync_send(ToWho, Message, N)
  when erlang:is_binary(ToWho) andalso erlang:is_binary(Message) ->
    case serv_fsm_pool:checkout() of
	full ->
	    {error, busy};
	Pid ->
	    Result = serv_fsm:sync_send(Pid, ToWho, Message, N),
	    ok = serv_fsm_pool:checkin(Pid),
	    Result
    end.

send_one() ->
    sync_send(<<"lee">>, <<"hello">>, 1).
