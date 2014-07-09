%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_send_worker).

-include("serv.hrl").
-include("serv_pb_chat_pb.hrl").

-behaviour(serv_worker).

%% serv_worker callback
-export([init_worker/1, handle_work/3, reply/2]).

%% API
-export([]).

-record(state, {}).
%%%===================================================================
%%% API
%%%===================================================================
%% serv_worker callback
%% init worker
-spec init_worker(WorkerArgs :: term()) ->
    {ok, NewWorkState :: term()} | {error, Reason :: term()}.
init_worker([]) ->
    {ok, #state{}}.

%% handle worker
-spec handle_work(Work :: term(), WorkFrom :: term(), WorkState ::term()) ->
    {reply, Reply :: term(), NewWorkState :: term()} |
    {noreply, NewWorkState :: term()}.

handle_work({forward, ToWho, {Id, Message}, N}, _WorkFrom, WorkState) ->
    case get_apl(?MESSAGE, ToWho, N) of
	[] ->
	    Reply = encode_reply(Id, 1, <<"serv donw">>),
	    {reply, {?MODULE, Reply}, WorkState};
	PrefList ->
	    Reply = forward_reply(PrefList, Id, Message),
	    {reply, {?MODULE, Reply}, WorkState}
    end;

handle_work(Work, WorkFrom, WorkState) ->
    lager:debug("work ~p from ~p", [Work, WorkFrom]),
    {reply, {?MODULE, {unkown_work, Work}}, WorkState}.
%% reply
-spec reply(WorkFrom :: term(), Reply :: term()) ->
    ok | {error, Reason :: term()}.

%% message will reply to serv_pb_server
reply(WorkFrom, {?MODULE, Info}) ->
    serv_pb_server:sync_send({pid, WorkFrom}, Info);

reply(_WorkFrom, _Reply) ->
    {error, not_impl}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_apl(binary(), binary(), integer()) -> node().
get_apl(Bucket, Key, N)
  when erlang:is_binary(Bucket) andalso erlang:is_binary(Key) ->
    DocIdx = riak_core_util:chash_key({Bucket, Key}),
    riak_core_apl:get_apl(DocIdx, N, ?SERV).

% do_forward
do_forward(IndexNode, RestPrefList, Message) ->
    try
	case riak_core_vnode_master:sync_command(IndexNode, {forward, Message},
						 ?SERV, ?TIMEOUT) of
	    forward -> % message was forward
		forward;
	    not_found -> % not found target
		forward(RestPrefList, Message)
	end
    catch
	{_Reason, _Where} ->
	    error
    end.
% forward
-spec forward(term(), term()) -> forward | error | not_found.
forward([], _Message) ->
    not_found;
% forward
forward(PrefList, Message) ->
    %% send messge to one index node
    [IndexNode| RestPrefList] = PrefList,
    do_forward(IndexNode, RestPrefList, Message).

% forward_reply
forward_reply(PrefList, Id, Message) ->
    case forward(PrefList, Message) of
	error ->
	    encode_reply(Id, 2, <<"serv internal error">>);
	forward ->
	    encode_reply(Id, 0, <<"message was forwarded">>);
	not_found ->
	    encode_reply(Id, 3, <<"message was not handle">>)
    end.

-spec encode_reply(integer(), integer(), binary()) -> iolist().
encode_reply(Id, ErrCode, ErrMsg) ->
    Reply = serv_pb_chat_pb:encode(#reply{id=Id,
					  errcode=ErrCode,
					  errmsg=ErrMsg}),
    [8, Reply].
