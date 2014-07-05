%%%-------------------------------------------------------------------
%%% @author HenryLee <lee@OSX.local>
%%% @copyright (C) 2014, HenryLee
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2014 by HenryLee <lee@OSX.local>
%%%-------------------------------------------------------------------
-module(serv).

-behaviour(gen_fsm).

-include("serv.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([start_fsm_pool/2, wait_worker/3]).

-export([ping/0,
	 get_apl/3,
	 get_apl_user/2,
	 sync_send/3,
	 send_one/0
	]).

-define(SERVER, ?MODULE).

-define(SERV_FSM_POOL_SIZE, 8).

-record(state, {serv_fsm_pool_size :: integer(),
		serv_fsm_pool :: queue:queue({serv_fsm_pid, pid()})}).

%%%===================================================================
%%% API
%%%===================================================================

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
    {ok, start_fsm_pool, #state{serv_fsm_pool_size = 0,
				serv_fsm_pool = queue:new()}, 0}.

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

do_start_fsm(ServFsmPool) ->
    case serv_fsm_sup:start_fsm() of
	{error, Reason} ->
	    {error, Reason};
	{ok, Pid} ->
	    queue:in({serv_fsm_pid, Pid}, ServFsmPool)
    end.

start_fsm(1, ServFsmPool) ->
    do_start_fsm(ServFsmPool);

start_fsm(N, ServFsmPool) ->
    case do_start_fsm(ServFsmPool) of
	{error, Reason} ->
	    {error, Reason};
	NewServFsmPool ->
	    start_fsm(N - 1, NewServFsmPool)
    end.

start_fsm_pool(timeout, State = #state{serv_fsm_pool = ServFsmPool}) ->
    ServFsmPoolSize = ?SERV_FSM_POOL_SIZE,
    case start_fsm(ServFsmPoolSize, ServFsmPool) of
	{error,  Reason} ->
	    {stop, Reason, State};
	NewServFsmPool ->
	    {next_state, wait_work, 
	     State#state{serv_fsm_pool_size = ServFsmPoolSize,
			 serv_fsm_pool = NewServFsmPool}}
    end;

start_fsm_pool(_Event, State) ->
    {next_state, start_fsm_pool, State}.

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

wait_worker({sync_send, _ToWho, _Message, _N}, _From, 
	    State) -> 
    %% todo, impl it
    Reply = not_impl,
    {reply, Reply, start_fsm_pool, State}.
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
    Reply = not_impl,
    {reply, Reply, StateName, State}.

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

stop_fsm(ServFsmPoolSize, ServFsmPool) ->
    case queue:out(ServFsmPool) of
	{empty, _ServFsmPool} ->
	    done;
	{{value, {serv_fsm_pid, Pid}}, NewServFsmPool} ->
	    stop_fsm(ServFsmPoolSize - 1, Pid, NewServFsmPool)
    end.

stop_fsm(ServFsmPoolSize, Pid, ServFsmPool) ->
    _Ignore = serv_fsm_sup:stop_fsm(Pid),
    case queue:out(ServFsmPool) of
	{empty, _ServFsmPool} ->
	    done;
	{{value, {serv_fsm_pid, Pid}}, NewServFsmPool} ->
	    stop_fsm(ServFsmPoolSize - 1, Pid, NewServFsmPool)
    end.

terminate(_Reason, _StateName, 
	  #state{serv_fsm_pool_size = ServFsmPoolSize,
		 serv_fsm_pool = ServFsmPool}) ->
    _ignore = stop_fsm(ServFsmPoolSize, ServFsmPool),
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
    gen_fsm:sync_send_event(?SERVER, {sync_send, ToWho, Message, N}).

send_one() ->
    sync_send(<<"lee">>, <<"hello">>, 1).
