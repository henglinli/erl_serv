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

handle_work({forward, ToWho, Message, N}, _WorkFrom, WorkState) ->
    case get_apl(?MESSAGE, ToWho, N) of
	[] ->
	    {reply, {error, serv_down}, WorkState};
	PrefList ->
	    Result = forward(PrefList, Message),
	    {reply, {?MODULE, Result}, WorkState}
    end;

handle_work(Work, WorkFrom, WorkState) ->
    lager:debug("work ~p from ~p", [Work, WorkFrom]),
    {reply, Work, WorkState}.
%% reply
-spec reply(WorkFrom :: term(), Reply :: term()) ->
    ok | {error, Reason :: term()}.

reply(WorkFrom, {?MODULE, Info}) ->
    WorkFrom ! Info;

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
forward([], _Message) ->
    not_found;
% forward
forward(PrefList, Message) ->
    %% send messge to one index node
    [IndexNode| RestPrefList] = PrefList,
    do_forward(IndexNode, RestPrefList, Message).
