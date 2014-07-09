%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_worker).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, start_link/3]).
-export([handle_work/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {module :: atom(),
		modstate :: any()}).

%% init worker
-callback init_worker(WorkerArgs :: term()) ->
    {ok, NewWorkState :: term()} |
    {error, Reason :: term()}.
%% handle worker
-callback handle_work(Work :: term(), WorkFrom :: term(), WorkState ::term()) ->
    {reply, Reply :: term(), NewWorkState :: term()} |
    {noreply, NewWorkState :: term()}.
%% reply
-callback reply(WorkFrom :: term(), Reply :: term()) ->
    ok | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================
handle_work(Worker, Work, From) ->
    gen_server:cast(Worker, {work, Work, From}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    WorkerMod = proplists:get_value(worker_callback_mod, Args),
    WorkerArgs = proplists:get_value(worker_args, Args, []),
    gen_server:start_link(?MODULE, [WorkerMod,  WorkerArgs], []).

start_link(Args, last) ->
    WorkerMod = proplists:get_value(worker_callback_mod, Args),
    WorkerArgs = proplists:get_value(worker_args, Args, []),
    gen_server:start_link(?MODULE, [WorkerMod,  WorkerArgs, last], []);

start_link(Name, Args)
  when erlang:is_atom(Name) ->
    WorkerMod = proplists:get_value(worker_callback_mod, Args),
    WorkerArgs = proplists:get_value(worker_args, Args, []),
    gen_server:start_link({local, Name}, ?MODULE, [WorkerMod,  WorkerArgs], []).

start_link(Name, Args, last)
  when erlang:is_atom(Name) ->
    WorkerMod = proplists:get_value(worker_callback_mod, Args),
    WorkerArgs = proplists:get_value(worker_args, Args, []),
    gen_server:start_link({local, Name}, ?MODULE, [WorkerMod,  WorkerArgs, last], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Module,  WorkerArgs]) ->
    {ok, WorkerState} = Module:init_worker(WorkerArgs),
    ok = serv_worker_pool:notify({worker_start, erlang:self()}),
    {ok, #state{module=Module, modstate=WorkerState}};

init([Module,  WorkerArgs, last]) ->
    {ok, WorkerState} = Module:init_worker(WorkerArgs),
    ok = serv_worker_pool:notify({last_worker_start, erlang:self()}),
    {ok, #state{module=Module, modstate=WorkerState}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    lager:debug("serv_worker received synchronous Reuest: ~p.", [Request]),
    {reply, not_impl, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({work, Work, WorkFrom},
	    #state{module = Mod, modstate = ModState} = State) ->
    NewModState = case Mod:handle_work(Work, WorkFrom, ModState) of
		      {reply, Reply, NS} ->
			  Mod:reply(WorkFrom, Reply),
			  NS;
		      {noreply, NS} ->
			  NS
		  end,
    %% check the worker back into the pool
    %% gen_fsm:send_all_state_event(Caller, {checkin, self()}),
    ok = serv_worker_pool:notify({checkin, erlang:self()}),
    {noreply, State#state{modstate=NewModState}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
