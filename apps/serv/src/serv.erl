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
	 send/1
	]).

%% serv_pb api
-export([register/1,
	 deregister/1]).

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

-spec send(Work :: term() |
		   {select,
		    From :: pid(),
		    User :: binary(),
		    N :: integer()} |
		   {forward,
		    Message :: #message{},
		    N :: integer()}) ->
		  forword | save | {error, Reason :: term()}.
%% forward message to other
send({forward, #message{} = _Message, _N} = Work) ->
%% work {froward, #message{}, N} was send by serv_send_worker
%% and handle by serv_vnode_work
    serv_worker_pool:handle_work(Work);
%% select server
send({select, _From, _User, _N} = Work) ->
    serv_worker_pool:handle_work(Work);
%% other message
send(_Work) ->
    {error, <<"not impl">>}.

-spec register(#session{}) -> ok | {error, term()}.
register(#session{user=User}=Session) ->
    case get_apl(?MESSAGE, User, 1) of
	[] ->
	    {error, <<"serv down">>};
	PrefList ->
	    [IndexNode| _RestPrefList] = PrefList,
	    riak_core_vnode_master:sync_command(IndexNode, {register, Session},
						?SERV, ?TIMEOUT)
    end;
%%
register(_) ->
    {error, <<"not impl">>}.
%%
-spec deregister(#session{}) -> ok | {error, term()}.
deregister(#session{pid=Pid, user=User}) ->
    case get_apl(?MESSAGE, User, 1) of
	[] ->
	    {error, <<"serv down">>};
	PrefList ->
	    [IndexNode| _RestPrefList] = PrefList,
	    riak_core_vnode_master:sync_command(IndexNode, {deregister, Pid},
						?SERV, ?TIMEOUT)
    end;
deregister(_) ->
    {error, <<"not impl">>}.
