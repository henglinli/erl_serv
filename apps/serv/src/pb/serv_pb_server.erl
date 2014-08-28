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

-include("serv.hrl").
-include("serv_pb_base_pb.hrl").
-include("serv_pb_chat_pb.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([wait_for_auth/2, wait_for_auth/3,
	 ready/2, ready/3,
	 reply_then_stop/2, reply_then_stop/3,
	 wait_for_socket/2, wait_for_socket/3]).

-export([set_socket/2, send/2, sync_send/2]).

-define(SERVER, ?MODULE).

-record(state, {transport={gen_tcp, inet} :: {gen_tcp, inet} | {ssl, ssl},
		%% socket
		socket=undefined :: inet:socket() | ssl:sslsocket(),
		request=undefined :: term(),  % current request
		states=undefined :: term(), % per-service connection state
		response = <<>> :: binary(),
		session=undefined :: term(),
		async=undefined :: module(),
		replys=undefined :: gb_trees:tree()
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
    case sidejob_supervisor:start_child(serv_pb_server_sj,
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
%% login or register
send({pid, Pid}, ok) ->
    Response = #response{errcode=0,
			 errmsg= <<"OK">>},
    gen_fsm:send_event(Pid, Response);

%% select
send({pid, Pid}, {server, Server}) ->
    Response = #server{errcode=0,
		       errmsg= <<"OK">>,
		       ip=Server},
    gen_fsm:send_event(Pid, Response);

%% reply
send({pid, Pid}, {reply, _Id, _Result}=Reply) ->
    gen_fsm:send_event(Pid, Reply);

%% chat
send({pid, Pid}, {chat, Chat}) ->
    gen_fsm:send_event(Pid, {chat, Chat});

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
    {ok, wait_for_socket, #state{replys=gb_trees:empty()}}.
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
		     State#state{request=undefined,
				 socket=Socket}};
		_Else ->
		    {reply, ok, wait_for_auth,
		     State#state{request=undefined,
				 socket=Socket}}
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

%% auth, only for ping
wait_for_auth(timeout, #state{}=State) ->
    timeout_reply(State, wait_for_auth);

%% login or register
wait_for_auth(#response{}=Response, State) ->
    encode_then_reply(State, Response, ready);

%%  select server
wait_for_auth(Response, #state{}=State) ->
    encode_then_reply(State, Response, wait_for_auth);

%% other unknown event
wait_for_auth(_Event, State) ->
    {next_state, wait_for_auth, State}.

wait_for_auth(_Event, _From, State) ->
    {reply, {error, <<"not impl">>}, wait_for_auth, State}.

%% ready
ready(timeout, #state{}=State) ->
    timeout_reply(State, ready);

%% for chat
ready({chat, Chat},
      #state{}=State) ->
    Encoded = [?SERVER_CHAT_CODE, Chat],
    reply_encoded(State, Encoded, ready);

%% for reply
%% value = {N :: error, M :: not_found, L :: forward}
ready({reply, Id, error},
      #state{replys=Replys}=State) ->
    Response=#reply{id=Id,
		    errcode=2,
		    errmsg= <<"send to self">>},
    case ?N of
	1 ->
	    encode_then_reply(State, Response, ready);
	_Else ->
	    case gb_trees:lookup(Id, Replys) of
		none ->
		    Replys1 = gb_trees:insert(Id, {1, 0, 0},  Replys),
		    {next_state, ready, State#state{replys=Replys1}};
		{value, {Es, Ns, Fs}} ->
		      %% N == Value
		    case Es+Ns+Fs+1 of
			?N ->
			    Replys1 = gb_trees:delete(Id, Replys),
			    encode_then_reply(State#state{replys=Replys1},
					      Response, ready);
			_Else ->
			    Replys1 = gb_trees:update(Id,
						      {Es+1, Ns, Fs}, Replys),
			    {next_state, ready, State#state{replys=Replys1}}
		    end
	    end
    end;

ready({reply, Id, not_found},
      #state{replys=Replys}=State) ->
    Saved=#reply{id=Id,
		 errcode=1,
		 errmsg= <<"saved">>},
    Forward=#reply{id=Id,
		   errcode=0,
		   errmsg= <<"forward">>},
    case ?N of
	1 ->
	    encode_then_reply(State, Saved, ready);
	_Else ->
	    case gb_trees:lookup(Id, Replys) of
		none ->
		    Replys1 = gb_trees:insert(Id, {0,1,0}, Replys),
		    {next_state, ready, State#state{replys=Replys1}};
		{value, {Es, Ns, Fs}} ->
		    %% N == Value
		    case Es+Ns+Fs+1 of
			%% last msg
			?N ->
			    %% TODO: save message here
			    Replys1 = gb_trees:delete(Id, Replys),
			    case Fs of
				%% 0 forward
				0 ->
				    encode_then_reply(State#state{replys=Replys1},
						      Saved, ready);
				_Else ->
				    encode_then_reply(State#state{replys=Replys1},
						      Forward, ready)
			    end;
			_Else ->
			    Replys1 =  gb_trees:update(Id,
						       {Es, Ns+1, Fs},
						       Replys),
			    {next_state, ready,
			     State#state{replys=Replys1}}
		    end
	    end
    end;

ready({reply, Id, forward},
      #state{replys=Replys}=State) ->
    Forward=#reply{id=Id,
		   errcode=0,
		   errmsg= <<"forward">>},
    case ?N of
	1 ->
	    encode_then_reply(State, Forward, ready);
	_Else ->
	    case gb_trees:lookup(Id, Replys) of
		none ->
		    Replys1 = gb_trees:insert(Id, {0,1,0}, Replys),
		    {next_state, ready, State#state{replys=Replys1}};
		{value, {Es, Ns, Fs}} ->
		    %% N == Value
		    case Es+Ns+Fs+1 of
			%% last msg
			?N ->
			    %% TODO: save message here
			    Replys1 = gb_trees:delete(Id, Replys),
			    encode_then_reply(State#state{replys=Replys1},
					      Forward, ready);
			_Else ->
			    Replys1 =  gb_trees:update(Id,
						       {Es, Ns+1, Fs},
						       Replys),
			    {next_state, ready,
			     State#state{replys=Replys1}}
		    end
	    end
    end;

ready(_Event, State) ->
    {next_state, ready, State}.

ready(_Event, _From, State) ->
    {reply, {error, <<"not imp">>}, ready, State}.

%% reply then stop
reply_then_stop(timeout, #state{}=State) ->
    reply_then_stop(State);

reply_then_stop(_Event, State) ->
    {stop, normal, State}.

reply_then_stop(_Event, _From, State) ->
    {stop, normal, {error, <<"not impl">>}, State}.
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

handle_info({tcp, Socket, Packet}, wait_for_auth,
	    #state{request=undefined,
		   socket=Socket,
		   session=Session}=State) ->
    BadPacket=serv_pb_error:get(1),
    case parse_packat(Packet) of
	%% unkown packat
	undefined ->
	    {next_state, reply_then_stop,
	     State#state{response=BadPacket}, 0};
	%% auth packet, register or login
	{MsgCode, MsgData} ->
	    Handler = wait_handler(MsgCode),
	    handle_wait(Handler, MsgCode, MsgData, Session, State)
    end;

handle_info({tcp, Socket, Packet}, ready,
	    #state{request=undefined,
		   socket=Socket,
		   states=HandlerStates}=State) ->
    BadPacket=serv_pb_error:get(1),
    case parse_packat(Packet) of
	undefined ->
	    {next_state, reply_then_stop,
	     State#state{response=BadPacket}, 0};
	{MsgCode, MsgData} ->
	    Handler = ready_handler(MsgCode),
	    handle_ready(Handler, MsgData, HandlerStates, State)
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
		 transport={Transport, _Control},
		 session=User}) ->
    case Socket of
	undefined ->
	    continue;
	_Socket ->
	    Transport:close(Socket)
    end,
    case User of
	undefined ->
	    done;
	_User ->
	    case app_helper:get_env(serv, session_type, riak_core) of
		riak_core ->
		    Session=#session{pid=erlang:self(), user=User},
		    _Result=serv:deregister(Session);
		_Else ->
		    _Result=serv_pb_session:unregister(User),
		    done
	    end
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

%% doc wait_for_auth state's handler
-spec wait_handler(non_neg_integer()) -> atom().
wait_handler(MsgCode) ->
    case MsgCode of
	?PING_CODE ->
	    serv_pb_handler_ping;
	?SELECT_CODE ->
	    %% select server
	    serv_pb_handler_select;
	?AUTH_CODE ->
	    %% auth
	    serv_pb_handler_auth;
	_Else ->
	    %% do nothing
	    serv_pb_handler_empty
    end.

%% @doc get state ready's handler
-spec ready_handler(non_neg_integer()) -> atom().
ready_handler(MsgCode) ->
    case MsgCode of
	?PING_CODE ->
	    serv_pb_handler_ping;
	?AUTH_CODE ->
	    %% already login
	    serv_pb_handler_ping;
	_Else ->
	    case serv_pb_handler:lookup(MsgCode) of
		undefined ->
		    serv_pb_handler_empty;
		Found when is_atom(Found) ->
		    Found
	    end
    end.

%% @doc handle state wait_for_auth's message
-spec handle_wait(atom(), non_neg_integer(),
		  binary(), term(), #state{}) -> tuple().
handle_wait(Handler, MsgCode, MsgData, Session, State) ->
    case Handler:decode(MsgData) of
	{ok, Message} ->
	    case Handler:process(Message, Session) of
		%% async
		{async, AsyncHandler, NewSession} ->
		    {next_state, wait_for_auth,
		     State#state{async=AsyncHandler,
				 session=NewSession}};
		%% reply
		{reply, ReplyMessage, NewSession} ->
		    Response=Handler:encode(ReplyMessage),
		    %% check if login done
		    NextState = case MsgCode of
				    ?AUTH_CODE ->
					ready;
				    _Other ->
					wait_for_auth
				end,
		    {next_state, NextState,
		     State#state{response=Response,
				 session=NewSession}, 0};
		%% error
		{error, Reason, NewSession} ->
		    {next_state, wait_for_auth,
		     State#state{response=Reason,
				 session=NewSession}}
	    end;
	{error, Reason} ->
	    {next_state, wait_for_auth,
	     State#state{response=Reason,
			 session=Session}}
    end.
%% @doc handle state ready's message
-spec handle_ready(atom(), binary(), term(), #state{}) -> tuple().
handle_ready(Handler, MsgData, HandlerStates, State) ->
    case Handler:decode(MsgData) of
	{ok, Message} ->
	    case Handler:process(Message, HandlerStates) of
		%% async
		{async, AsyncHandler, HandlerStates} ->
		    {next_state, ready,
		     State#state{async=AsyncHandler,
				 states=HandlerStates}};
		%% reply
		{reply, ReplyMessage, NewHandlerStates} ->
		    Response=Handler:encode(ReplyMessage),
		    {next_state, ready,
		     State#state{response=Response,
				 states=NewHandlerStates}, 0};
		%% error
		{error, Reason, NewHandlerStates} ->
		    {next_state, ready,
		     State#state{response=Reason,
				 states=NewHandlerStates}}
	    end;
	{error, Reason} ->
	    {next_state, ready,
	     State#state{response=Reason,
			 states=HandlerStates}}
    end.

%% reply
-spec reply(term(), term(), term(), iodata(), #state{}, atom()) ->
		   {next_state, atom(), #state{}} |
		   {stop, term(), #state{}}.
reply(Transport, Socket, Control, Response, State, NextStateName) ->
    case Transport:send(Socket, Response) of
	ok ->
	    Control:setopts(Socket, [{active, once}]),
	    {next_state, NextStateName, State};
	{error, Reason} ->
	    lager:warning("send error: ~p", [Reason]),
	    {stop, Reason, State}
    end.

%% timeout reply
-spec timeout_reply(#state{}, atom()) ->
			   {next_state, atom(), #state{}} |
			   {stop, term(), #state{}}.
timeout_reply(#state{socket=Socket,
		     transport={Transport, Control},
		     response=Response}=State,
	      NextStateName) ->
    reply(Transport, Socket, Control, Response, State, NextStateName).

%% encode then reply
-spec encode_then_reply(#state{}, term(), atom()) ->
			       {next_state, atom(), #state{}} |
			       {stop, term(), #state{}}.
encode_then_reply(#state{async=AsyncHandler,
			 socket=Socket,
			 transport={Transport, Control}}=State,
		  Response,
		  NextStateName) ->
    Encoded=AsyncHandler:encode(Response),
    reply(Transport, Socket, Control, Encoded, State, NextStateName).

%% reply encoded
-spec reply_encoded(#state{}, term(), atom()) ->
			   {next_state, atom(), #state{}} |
			   {stop, term(), #state{}}.

reply_encoded(#state{socket=Socket,
		     transport={Transport, Control}}=State,
	      Response,
	      NextStateName) ->
    reply(Transport, Socket, Control, Response, State, NextStateName).

%% reply then stop
-spec reply_then_stop(#state{}) ->
			     {stop, term(), #state{}}.
reply_then_stop(#state{response=Response,
		       socket=Socket,
		       transport={Transport, _Control}
		      }=State) ->
    case Transport:send(Socket, Response) of
	ok ->
	    {stop, normal, State};
	{error, Reason} ->
	    {stop, Reason, State}
    end.
