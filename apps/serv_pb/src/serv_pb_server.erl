%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_server).
-author('HenryLee<henglinli@gmail.com>').

%-include("serv_pb.hrl").
-include("serv_pb_base_pb.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([ready/2, ready/3,
	 reply/2, reply/3,
	 reply_then_stop/2, reply_then_stop/3,
	 wait/2, wait/3]).

-export([set_socket/2, send/2, sync_send/2]).

-define(SERVER, ?MODULE).

-record(state, {transport = {gen_tcp, inet} :: {gen_tcp, inet} | {ssl, ssl},
		socket = undefined :: undefined | inet:socket() | ssl:sslsocket(),   % socket
		request = undefined :: term(),                % current request
		peername = undefined :: undefined | {inet:ip_address(), inet:port_number()},
		common_name :: undefined | binary(),
		security = undefined :: term(),
		retries = 3,
		response = <<>> :: binary(),
		session = undefined :: term()
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
    gen_fsm:start_link(?MODULE, [], []).

%% @doc Sets the socket to service for this server.
-spec set_socket(pid(), port()) -> ok.
set_socket(Pid, Socket) ->
    gen_fsm:sync_send_event(Pid, {set_socket, Socket}, infinity).

%% @doc sync send to client
-spec sync_send(Pid :: pid(), Message :: binary() | iolist()) ->
		       ok | {error, Reason :: term()}.
sync_send(Pid, Message) ->
    gen_fsm:sync_send_event(Pid, {message, Message}).

%% @doc send to client
-spec send(Pid :: pid(), Message :: binary() | iolist()) -> ok.
send(Pid, Message) ->
    gen_fsm:send_event(Pid, {message, Message}).

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
    {ok, wait, #state{}, 0}.
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
wait(timeout, State) ->
    {next_state, wait, State};

wait(_Event, State) ->
    {next_state, wait, State}.
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
wait({set_socket, Socket}, _From,
     State = #state{transport= {_Transport, Control}}) ->
    case Control:peername(Socket) of
	{ok, PeerInfo} ->
	    Control:setopts(Socket, [{active, once}]),
	    %% check if security is enabled, if it is wait for TLS, otherwise go
	    %% straight into connected state
	    {reply, ok, ready, State#state{request=undefined,
					   socket=Socket,
					   peername=PeerInfo}};
	{error, Reason} ->
	    lager:debug("Could not get PB socket peername: ~p", [Reason]),
	    %% It's not really "ok", but there's no reason for the
	    %% listener to crash just because this socket had an
	    %% error. See riak_api#54.
	    {stop, normal, Reason, State}
    end;

wait(_Event, _From, State) ->
    {reply, unknown_message, wait, State}.

%% ready
ready(timeout, State) ->
    {next_state, ready, State};

ready({message, Message}, State) ->
    {next_state, reply, State#state{response = Message}, 0};

ready(_Event, State) ->
    {next_state, ready, State}.

ready({message, Message}, _From, State) ->
    {reply, ok, reply, State#state{response = Message}, 0};

ready(_Event, _From, State) ->
    {reply, unknown_message, ready, State}.

%% reply
reply(timeout, State=#state{socket = Socket,
			    transport = {Transport, Control},
			    response = Response}) ->
    case Transport:send(Socket, Response) of
	ok ->
	    Control:setopts(Socket, [{active, once}]),
	    {next_state, ready, State};
	{error, Reason} ->
	    lager:debug("send error: ~p", [Reason]),
	    {stop, Reason, State}
    end;

reply(_Event, State) ->
    {next_state, ready, State}.

reply(_Event, _From, State) ->
    {reply, unknown_message, ready, State}.

% reply then stop
reply_then_stop(_Event, State=#state{response=Response,
				     socket = Socket,
				     transport = {Transport, _Control}
				    }) ->
    Transport:send(Socket, Response),
    {stop, normal, State}.

reply_then_stop(_Event, _From, State) ->
    {stop, normal, unknown_message, State}.
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
    Reply = unknown_message,
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
    {stop, normal, State#state{socket = undefined}};

handle_info({tcp_error, _Socket, _Reason}, _StateName, State) ->
    {stop, normal, State#state{socket = undefined}};

handle_info({tcp, Socket, _Data}, handle_info, State) ->
    %% req =/= undefined: received a new request while another was in
    %% progress -> Error
    lager:debug("Received a new PB socket request"
		" while another was in progress"),
    Response = encode(#response{errmsg = <<"last request not done">>,
				errcode = 4}),
    {next_state, reply_then_stop, State#state{socket = Socket,
					      response = Response}};

handle_info({tcp, Socket, Packet}, _StateName,
	    State = #state{request=undefined,
			   socket=Socket,
			   session=Session}) ->
    case parse_packat(Packet) of
	undefined ->
	    lager:debug("recved: [~p]", [Packet]),
	    BadPkg = encode(#response{errmsg = <<"bad packet">>,
				      errcode = 1}),
	    {next_state, reply, State#state{response = BadPkg}};
	{MsgCode, MsgData} ->
	    lager:debug("recved: {~p, ~p}", [MsgCode, MsgData]),
	    NotImpl = encode(#response{errmsg = <<"not implement">>,
				       errcode = 2}),
	    InternalErr = encode(#response{errmsg = <<"internal error">>,
					   errcode = 3}),
	    case serv_pb_handler:lookup(MsgCode) of
		undefined ->
		    lager:warning("unregistered message: ~p", [MsgCode]),
		    {next_state, reply, State#state{response = NotImpl}};
		Handler when is_atom(Handler) ->
		    case Handler:handle(MsgData, Session) of
			{noreply, nochange} ->
			    {next_state, ready, State, 0};
			{noreply, NewSession} ->
			    {next_state, ready,
			     State#state{session = NewSession}, 0};
			{Response, nochange} ->
			    {next_state, reply,
			     State#state{response = Response}, 0};
			{Response, NewSession} ->
			    {next_state, reply,
			     State#state{response = Response,
					 session = NewSession}, 0};
			_ ->
			    {next_state, reply_then_stop,
			     State#state{response = InternalErr}, 0}
		    end
	    end
    end;

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
	  #state{socket = undefined}) ->
    ok;

terminate(_Reason, _StateName,
	 #state{socket = Socket, transport = {Transport, _Control}}) ->
    Transport:close(Socket),
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
-spec parse_packat(Packet::binary()) ->
			  undefined | {MsgCode::integer(), MsgData::binary()}.
parse_packat(<<MsgCode:8/big-unsigned-integer,
	       MsgData/binary>>) ->
    {MsgCode, MsgData};
parse_packat(_) ->
    undefined.

-spec encode(Error :: #response{}) -> Response::iolist().
encode(Error) ->
    [0 | serv_pb_base_pb:encode(Error)].
