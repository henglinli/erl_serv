%%
%% Copyright (c) 2007-2011, 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% henglinli@gmail.com
%% changes:
%% remove vonde related
-module(serv_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
-export([start_link/1, handle_work/3, handle_work/4]).

-define(SERVER, ?MODULE).

-ifdef(PULSE).
-compile(export_all).
-compile({parse_transform, pulse_instrument}).
-compile({pulse_replace_module, [{gen_fsm, pulse_gen_fsm},
				 {gen_server, pulse_gen_server}]}).
-endif.

-record(state, {
	  module :: atom(),
	  modstate :: any(),
	  worker_pool :: term()
	 }).

%% init worker
-callback init_worker(WorkerArgs :: term()) ->
    {ok, NewWorkerState :: term()}.
%% handle worker
-callback handle_work(Work :: term(), WorkFrom :: term(), WorkerState ::term()) ->
    {reply, Reply :: term(), NewWorerkState :: term()} |
    {noreply, NewWorkerState :: term()}.
%% reply
-callback reply(WorkFrom :: term(), Reply :: term()) ->
    ok | {error, Reason :: term()}.

start_link(Args) ->
    WorkerMod = proplists:get_value(worker_callback_mod, Args),
    WorkerArgs = proplists:get_value(worker_args, Args, []),
    WorkerPool = proplists:get_value(worker_pool, Args, serv_worker_pool),
    gen_server:start_link(?SERVER, [WorkerMod,  WorkerArgs, WorkerPool], []).

handle_work(Worker, Work, From) ->
    handle_work(Worker, Work, From, erlang:self()).

handle_work(Worker, Work, From, Caller) ->
    gen_server:cast(Worker, {work, Work, From, Caller}).

init([Module,  WorkerArgs, WorkerPool]) ->
    {ok, WorkerState} = Module:init_worker(WorkerArgs),
    {ok, #state{module=Module, modstate=WorkerState, worker_pool=WorkerPool}}.

handle_call(Event, _From, State) ->
    lager:debug("serv_worker received synchronous event: ~p.", [Event]),
    {reply, not_impl, State}.

handle_cast({work, Work, WorkFrom, Caller},
	    #state{module = Mod, modstate = ModState} = State) ->
    NewModState = case Mod:handle_work(Work, WorkFrom, ModState) of
		      {reply, Reply, NS} ->
			  Mod:reply(WorkFrom, Reply),
			  NS;
		      {noreply, NS} ->
			  NS
		  end,
    %% check the worker back into the pool
    gen_fsm:send_all_state_event(Caller, {checkin, self()}),
    {noreply, State#state{modstate=NewModState}};

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
