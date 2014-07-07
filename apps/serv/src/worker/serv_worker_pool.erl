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

%% @doc This is a wrapper around a poolboy pool, that implements a
%% queue. That is, this process maintains a queue of work, and checks
%% workers out of a poolboy pool to consume it.
%%
%% The workers it uses send two messages to this process.
%%
%% The first message is at startup, the bare atom
%% `worker_started'. This is a clue to this process that a worker was
%% just added to the poolboy pool, so a checkout request has a chance
%% of succeeding. This is most useful after an old worker has exited -
%% `worker_started' is a trigger guaranteed to arrive at a time that
%% will mean an immediate poolboy:checkout will not beat the worker
%% into the pool.
%%
%% The second message is when the worker finishes work it has been
%% handed, the two-tuple, `{checkin, WorkerPid}'. This message gives
%% this process the choice of whether to give the worker more work or
%% check it back into poolboy's pool at this point. Note: the worker
%% should *never* call poolboy:checkin itself, because that will
%% confuse (or cause a race) with this module's checkout management.

%% henglinli@gmail.com
%% changes:
%% remove vnode related
-module(serv_worker_pool).

-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).

%% gen_fsm states
-export([ready/2, queueing/2, ready/3, queueing/3, shutdown/2, shutdown/3]).
-export([wait_start_wokers/2, wait_start_wokers/3]).
%% API
-export([start_link/0, start_link/3, stop/1, shutdown_pool/1, handle_work/2]).
-export([start_workers/3]).
-define(SERVER, ?MODULE).

-ifdef(PULSE).
-compile(export_all).
-compile({parse_transform, pulse_instrument}).
-compile({pulse_replace_module, [{gen_fsm, pulse_gen_fsm}]}).
-endif.

-record(state, {
	  queue = undefined :: queue:queue({work, Work :: term(), From :: term()}),
	  pool = undefined :: queue:queue({pid, Pid :: pid()}),
	  monitors = [] :: list(),
	  shutdown :: undefined | {pid(), reference()},
	  worker_module = undefined :: Mod :: atom()
	 }).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(PoolSize, CallBack, WorkerArgs) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE,
		       [PoolSize, CallBack, WorkerArgs], []).

start_workers(PoolSize, CallBack, WorkerArgs) ->
    gen_fsm:sync_send_event(?SERVER,
			    {start_workers, PoolSize, CallBack, WorkerArgs}).

handle_work(Work, From) ->
    gen_fsm:send_event(?SERVER, {work, Work, From}).

stop(Reason) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {stop, Reason}).

%% wait for all the workers to finish any current work
shutdown_pool(Wait) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {shutdown, Wait}, infinity).

start_worker(Pool, Args) ->
    case serv_worker_sup:start_worker(Args) of
	{ok, Pid} ->
	    NewPool = queue:in({pid, Pid}, Pool),
	    {ok, NewPool};
	{ok, Pid, _Info} ->
	    NewPool = queue:in({pid, Pid}, Pool),
	    {ok, NewPool};
	{error, Reason} ->
	    {error, Reason}
    end.

start_worker(1, Queue, Args, ok) ->
    start_worker(Queue, Args);

start_worker(_PoolSize, Reason, _Args, error) ->
    {error, Reason};

start_worker(PoolSize, Pool, Args, ok) ->
    {Result, NewPool} = start_worker(Pool, Args),
    start_worker(PoolSize - 1, NewPool, Args, Result).

init([]) ->
    {ok, wait_start_wokers, #state{pool=undefined, queue=undefined,
				   monitors=[], shutdown=undefined,
				   worker_module=undefined}};

init([PoolSize, WorkerMod, WorkerArgs]) ->
    FullWokerArgs = [{worker_args, WorkerArgs},
		     {worker_callback_mod, WorkerMod},
		     {worker_pool, ?SERVER}],

    case start_worker(PoolSize, queue:new(), FullWokerArgs, ok) of
	{ok, Pool} ->
	    {ok, ready, #state{pool=Pool, queue = queue:new(),
			       monitors=[], shutdown=undefined,
			       worker_module=WorkerMod}};
	{error, Reason} ->
	    {stop, Reason}
    end.

wait_start_wokers({start_workers, Size, WorkerMod, WorkerArgs}, _From, State) ->
    FullWokerArgs = [{worker_args, WorkerArgs},
		     {worker_callback_mod, WorkerMod},
		     {worker_pool, ?SERVER}],

    case start_worker(Size, queue:new(), FullWokerArgs, ok) of
	{ok, Pool} ->
	    {reply, ok, ready, #state{pool=Pool, queue = queue:new(),
				      monitors=[], shutdown=undefined,
				      worker_module=WorkerMod}};
	{error, Reason} ->
	    {stop, Reason, {error, Reason}, State}
    end;

wait_start_wokers(_Event, _From, State) ->
    {reply, not_impl, ready, State}.

wait_start_wokers(_Event, State) ->
    {next_state, wait_start_wokers, State}.

ready(_Event, _From, State) ->
    {reply, not_impl, ready, State}.

ready({work, Work, From} = Msg,
      #state{pool=Pool, queue=Q, monitors=Monitors} = State) ->
    case queue:out(Pool) of
	{empty, _Empty} ->
	    {next_state, queueing, State#state{queue=queue:in(Msg, Q)}};
	{{value, {pid, Pid}}, NewPool} when erlang:is_pid(Pid) ->
	    NewMonitors = monitor_worker(Pid, From, Work, Monitors),
	    serv_worker:handle_work(Pid, Work, From),
	    {next_state, ready,
	     State#state{pool=NewPool, monitors=NewMonitors}}
    end;

ready(_Event, State) ->
    {next_state, ready, State}.

queueing(_Event, _From, State) ->
    {reply, ok, queueing, State}.

queueing({work, _Work, _From} = Msg, #state{queue=Q} = State) ->
    {next_state, queueing, State#state{queue=queue:in(Msg, Q)}};

queueing(_Event, State) ->
    {next_state, queueing, State}.

shutdown(_Event, _From, State) ->
    {reply, ok, shutdown, State}.

shutdown({work, _Work, From}, #state{worker_module = Mod} = State) ->
    %% tell the process requesting work that we're shutting down
    Mod:reply(From, {error, serv_worker_shutdown}),
    {next_state, shutdown, State};

shutdown(_Event, State) ->
    {next_state, shutdown, State}.

handle_event({checkin, Pid}, shutdown, #state{pool=Pool, monitors=Monitors} = State) ->
    NewMonitors = demonitor_worker(Pid, Monitors),
    NewPool = queue:in({pid, Pid}, Pool),
    case Monitors of
	[] -> %% work all done, time to exit!
	    {stop, shutdown, State};
	_ ->
	    {next_state, shutdown,
	     State#state{monitors=NewMonitors, pool=NewPool}}
    end;

handle_event({checkin, Worker}, _, #state{pool=Pool, queue=Q, monitors=Monitors} = State) ->
    case queue:out(Q) of
	{{value, {work, Work, From}}, Rem} ->
	    %% there is outstanding work to do - instead of checking
	    %% the worker back in, just hand it more work to do
	    NewMonitors = monitor_worker(Worker, From, Work, Monitors),
	    serv_worker:handle_work(Worker, Work, From),
	    {next_state, queueing, State#state{queue=Rem,
					       monitors=NewMonitors}};
	{empty, Empty} ->
	    NewMonitors = demonitor_worker(Worker, Monitors),
	    NewPool = queue:in({pid, Worker}, Pool),
	    {next_state, ready,
	     State#state{queue=Empty, pool=NewPool, monitors=NewMonitors}}
    end;

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event({stop, Reason}, _From, _StateName, State) ->
    {stop, Reason, ok, State};
handle_sync_event({shutdown, Time}, From, _StateName,
		  #state{queue=Q, monitors=Monitors, worker_module=Mod} = State) ->
    discard_queued_work(Q, Mod),
    case Monitors of
	[] ->
	    {stop, shutdown, ok, State};
	_ ->
	    case Time of
		infinity ->
		    ok;
		_ when erlang:is_integer(Time) ->
		    erlang:send_after(Time, self(), shutdown),
		    ok
	    end,
	    {next_state, shutdown, State#state{shutdown=From, queue=queue:new()}}
    end;
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, unknown_message}, StateName, State}.

handle_info({'DOWN', _Ref, _, Pid, Info}, StateName,
	    #state{monitors=Monitors, worker_module=Mod} = State) ->
    %% remove the listing for the dead worker
    case lists:keyfind(Pid, 1, Monitors) of
	{Pid, _, From, Work} ->
	    Mod:reply(From, {error, {serv_worker_crash, Info, Work}}),
	    NewMonitors = lists:keydelete(Pid, 1, Monitors),
	    %% trigger to do more work will be 'worker_start' message
	    %% when poolboy replaces this worker (if not a 'checkin'
	    %% or 'handle_work')
	    {next_state, StateName, State#state{monitors=NewMonitors}};
	false ->
	    {next_state, StateName, State}
    end;

handle_info(shutdown, shutdown,
	    #state{monitors=Monitors, worker_module=Mod} = State) ->
    %% we've waited too long to shutdown, time to force the issue.
    _ = [Mod:reply(From, {error, serv_worker_shutdown}) ||
	    {_, _, From, _} <- Monitors],
    {stop, shutdown, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Keep track of which worker we pair with what work/from and monitor the
%% worker. Only active workers are tracked
monitor_worker(Worker, From, Work, Monitors) ->
    case lists:keyfind(Worker, 1, Monitors) of
	{Worker, Ref, _OldFrom, _OldWork} ->
	    %% reuse old monitor and just update the from & work
	    lists:keyreplace(Worker, 1, Monitors, {Worker, Ref, From, Work});
	false ->
	    Ref = erlang:monitor(process, Worker),
	    [{Worker, Ref, From, Work} | Monitors]
    end.

demonitor_worker(Worker, Monitors) ->
    case lists:keyfind(Worker, 1, Monitors) of
	{Worker, Ref, _From, _Work} ->
	    erlang:demonitor(Ref),
	    lists:keydelete(Worker, 1, Monitors);
	false ->
	    %% not monitored?
	    Monitors
    end.

discard_queued_work(Q, Mod) ->
    case queue:out(Q) of
	{{value, {work, _Work, From}}, Rem} ->
	    Mod:reply(From, {error, serv_worker_shutdown}),
	    discard_queued_work(Rem, Mod);
	{empty, _Empty} ->
	    ok
    end.
