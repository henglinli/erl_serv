%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_mc_binary_server).
-author('HenryLee<henglinli@gmail.com>').

-include("serv.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([ready/2, ready/3,
	 wait_for_socket/2, wait_for_socket/3]).

-export([set_socket/2, send/2, sync_send/2]).

-define(SERVER, ?MODULE).

-record(state, {transport={gen_tcp, inet} :: {gen_tcp, inet} | {ssl, ssl},
		%% socket
		socket=undefined :: inet:socket() | ssl:sslsocket(),
		states=undefined :: term() % per-service connection state		
	       }).
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
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    case sidejob_supervisor:start_child(serv_mc_binary_server_sj,
					gen_fsm, start_link,
					[?MODULE, [], []]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Pid} ->
	    {ok, Pid}
    end.
%% @doc Sets the socket to service for this server.
-spec set_socket(pid(), port()) -> ok.
set_socket(Pid, Socket) ->
    gen_fsm:sync_send_event(Pid, {set_socket, Socket}, infinity).

%% @doc sync send to client
-spec sync_send({pid, Pid :: pid()} | {name, Name :: binary()},
		Message :: iodata()) ->
		       ok | {error, Reason :: term()}.
sync_send(_Name, _Message) ->
    {error, <<"not impl">>}.

%% @doc send to client
-spec send({pid, Pid :: pid()} | {name, Name :: binary()},
	   Message :: iodata()) ->
		  ok | {error, Reason :: term()}.

send(_Name, _Message) ->
    {error, <<"not impl">>}.

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
-spec init(list()) -> {ok, wait, #state{}}.
init([]) ->
    serv_pb_stat:update(pbc_connect),
    {ok, wait_for_socket, #state{}}.
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
wait_for_socket(_Event, State) ->
    {next_state, wait_for_socket, State}.

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
wait_for_socket({set_socket, Socket}, _From,
		#state{transport= {_Transport, Control}}=State) ->
    case Control:peername(Socket) of
	{ok, _PeerInfo} ->
	    Control:setopts(Socket, [{active, once}]),
	    %% check if security is enabled, if it is wait for TLS, otherwise go
	    %% straight into connected state
	    case app_helper:get_env(serv_pb, public, false) of
		true ->
		    {reply, ok, ready,
		     State#state{socket=Socket}};
		_Else ->
		    {reply, ok, wait_for_auth,
		     State#state{socket=Socket}}
	    end;
	{error, Reason} ->
	    lager:error("Could not get PB socket peername: ~p", [Reason]),
	    %% It's not really "ok", but there's no reason for the
	    %% listener to crash just because this socket had an
	    %% error. See riak_api#54.
	    {stop, normal, State}
    end;

wait_for_socket(_Event, _From, State) ->
    {reply, {error, <<"not impl">>}, wait_for_socket, State}.

%% ready
ready(_Event, State) ->
    {next_state, ready, State}.

ready(_Event, _From, State) ->
    {reply, {error, <<"not imp">>}, ready, State}.

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
    Reply={error, <<"not impl">>},
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
%% @doc The handle_info/3 gen_fsm callback.
handle_info({tcp_closed, _Socket}, _StateName, State) ->
    {stop, normal, State#state{socket=undefined}};

handle_info({tcp_error, _Socket, _Reason}, _StateName, State) ->
    {stop, normal, State#state{socket=undefined}};

handle_info({tcp, Socket, Packet}, ready,
	    #state{socket=Socket,
		   states=_HandlerStates}=State) ->
    case parse_packat(Packet) of
	undefined ->
	    {next_state, ready,
	     State#state{}, 0};
	{_MsgCode, _MsgData} ->
	    {next_state, ready,
	     State#state{}, 0}
    end;

%% unknown mesasge
handle_info(Message, StateName, State) ->
    %% Throw out messages we don't care about, but log them
    lager:error("Unrecognized message ~p", [Message]),
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
terminate(_Reason, _StateName,
	  #state{socket=Socket,
		 transport={Transport, _Control}}) ->
    case Socket of
	undefined ->
	    continue;
	_Socket ->
	    Transport:close(Socket)
    end.

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
    {ok, StateName, State, 0}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec parse_packat(Packet::binary()) ->
			  undefined | {MsgCode::integer(), MsgData::binary()}.
parse_packat(<<MsgCode:8/big-unsigned-integer,
	       MsgData/binary>>) ->
    {MsgCode, MsgData};
parse_packat(_) ->
    undefined.
