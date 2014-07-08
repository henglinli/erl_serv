%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_send_worker).

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
    {ok, NewWorkerState :: term()} | {error, Reason :: term()}.
init_worker([]) ->
    {ok, #state{}}.

%% handle worker
-spec handle_work(Work :: term(), WorkFrom :: term(), WorkerState ::term()) ->
    {reply, Reply :: term(), NewWorerkState :: term()} |
    {noreply, NewWorkerState :: term()}.
handle_work(Work, WorkFrom, WorkerState) ->
    lager:info("work ~p from ~p", [Work, WorkFrom]),
    {noreply, WorkerState}.
%% reply
-spec reply(WorkFrom :: term(), Reply :: term()) ->
    ok | {error, Reason :: term()}.
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
