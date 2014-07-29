%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_worker_pool).

-behaviour(gen_fsm).

%% API
-export([start_link/0, handle_work/1]).
%% API, called by serv_worker
-export([notify/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% gen_fsm states
-export([wait_worker_start/2, wait_worker_start/3,
	 ready/2, queueing/2, ready/3, queueing/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  queue = undefined :: queue:queue({work, Work :: term()}),
	  pool = undefined :: queue:queue({pid, Pid :: pid()})
	 }).

%%%===================================================================
%%% API
%%%===================================================================
handle_work(Work) ->
    gen_fsm:send_event(?SERVER, {work, Work}).

notify({worker_start, _Pid} = Info) ->
    gen_fsm:send_event(?SERVER, Info);

notify({last_worker_start, _Pid} = Info) ->
    gen_fsm:send_event(?SERVER, Info);

notify({pid, _Pid} = Info) ->
    gen_fsm:send_event(?SERVER, Info).
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, wait_worker_start,
     #state{queue=queue:new(), pool=queue:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
wait_worker_start({worker_start, Pid},
		  #state{pool=Pool} = State) ->
    NewPool = queue:in({pid, Pid}, Pool),
    {next_state, wait_worker_start,
     State#state{pool=NewPool}};

wait_worker_start({last_worker_start, Pid},
		  #state{pool=Pool} = State) ->
    NewPool = queue:in({pid, Pid}, Pool),
    {next_state, ready,
     State#state{pool=NewPool}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
wait_worker_start(_Event, _From, State) ->
    {reply, {error, <<"not impl">>}, ready, State}.

ready(_Event, _From, State) ->
    {reply, {error, <<"not impl">>}, ready, State}.

ready({work, Work} = Msg,
      #state{pool=Pool, queue=Queue} = State) ->
    case queue:out(Pool) of
	{empty, EmptyPool} ->
	    {next_state, queueing,
	     State#state{queue=queue:in(Msg, Queue),
			 pool=EmptyPool}};
	{{value, {pid, Pid} = Worker}, NewPool}
	  when erlang:is_pid(Pid) ->
	    serv_worker:handle_work(Worker, Work),
	    {next_state, ready,
	     State#state{pool=NewPool}}
    end;

ready(_Event, State) ->
    {next_state, ready, State}.

queueing(_Event, _From, State) ->
    {reply, ok, queueing, State}.

queueing({work, _Work} = Msg, #state{queue=Queue} = State) ->
    {next_state, queueing, State#state{queue=queue:in(Msg, Queue)}};

queueing(_Event, State) ->
    {next_state, queueing, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event({pid, _Pid} = Worker, _StateName,
	     #state{pool=Pool, queue=Queue} = State) ->
    case queue:out(Queue) of
	{{value, {work, Work}}, Rem} ->
	    %% there is outstanding work to do - instead of checking
	    %% the worker back in, just hand it more work to do
	    serv_worker:handle_work(Worker, Work),
	    {next_state, queueing, State#state{queue=Rem}};
	{empty, EmptyQueue} ->
	    NewPool = queue:in({pid, Worker}, Pool),
	    {next_state, ready,
	     State#state{queue=EmptyQueue, pool=NewPool}}
    end;

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, <<"not impl">>}, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
