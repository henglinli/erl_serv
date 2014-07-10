%%%-------------------------------------------------------------------
%%% @author HenryLee <lee@OSX.local>
%%% @copyright (C) 2014, HenryLee
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2014 by HenryLee <lee@OSX.local>
%%%-------------------------------------------------------------------
-module(serv).

-include("serv.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

%% API
-export([ping/0,
	 get_apl/3,
	 get_apl_user/2,
	 send/4
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

-spec send(From :: pid(),
	   ToWho :: binary(),
	   TheMessage :: term() | {Id :: integer(), Message :: binary()},
	   N :: integer()) ->
		  forword | save | {error, Reason :: term()}.
send(From, ToWho, {Id, Message}, N)
  when erlang:is_pid(From)
       andalso erlang:is_binary(ToWho)
       andalso erlang:is_integer(Id)
       andalso erlang:is_binary(Message)
       andalso erlang:is_integer(N) ->
    %% message {froward, Message} was send by serv_send_worker
    %% and handle by serv_vnode_work
    serv_worker_pool:handle_work({forward, ToWho, {Id, Message}, N}, From);

send(From, ToWho, Message, N) ->
    serv_worker_pool:handle_work({forward, ToWho, Message, N}, From).
