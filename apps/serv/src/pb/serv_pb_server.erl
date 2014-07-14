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
         reply/2, reply/3,
         reply_then_stop/2, reply_then_stop/3,
         wait_for_socket/2, wait_for_socket/3]).

-export([set_socket/2, send/2, sync_send/2]).

-define(SERVER, ?MODULE).

-record(state, {transport = {gen_tcp, inet} :: {gen_tcp, inet} | {ssl, ssl},
                                                % socket
                socket = undefined :: undefined | inet:socket() | ssl:sslsocket(),
                request = undefined :: undefined | term(),  % current request
                peername = undefined :: undefined | {inet:ip_address(), inet:port_number()},
                states = undefined :: term(), % per-service connection state
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
    case erlang:whereis(serv_pb_server_sj) of
        undefined ->
            gen_fsm:start_link(?MODULE, [], []);
        _SideJob ->
            case sidejob_supervisor:start_child(serv_pb_server_sj,
                                                gen_fsm, start_link,
                                                [?MODULE, [], []]) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Pid} ->
                    {ok, Pid}
            end
    end.
%% @doc Sets the socket to service for this server.
-spec set_socket(pid(), port()) -> ok.
set_socket(Pid, Socket) ->
    gen_fsm:sync_send_event(Pid, {set_socket, Socket}, infinity).

%% @doc sync send to client
-spec sync_send({pid, Pid :: pid()} | {name, Name :: binary()},
                Message :: binary() | iolist()) ->
                       ok | {error, Reason :: term()}.
sync_send({name, Who}, Message)
  when erlang:is_binary(Who) ->
    case serv_pb_session:lookup(Who) of
        undefined ->
            {error, not_found};
        Sessions ->
            lists:map(fun({_Who, {pid, Pid}}) ->
                              gen_fsm:sync_send_event(Pid, {message, Message})
                      end,
                      Sessions)
    end;

%% send server
sync_send({pid, Pid}, {server, {0, Server}})
  when erlang:is_pid(Pid) ->
    EncodedServer = encode_server(0, <<>>, Server),
    gen_fsm:sync_send_event(Pid, {server, EncodedServer});
%% send server errror
sync_send({pid, Pid}, {server, {ErrCode, ErrMsg}})
  when erlang:is_pid(Pid) ->
    EncodedServer = encode_server(ErrCode, ErrMsg, <<>>),
    gen_fsm:sync_send_event(Pid, {server, EncodedServer});
%% send reply
sync_send({pid, Pid}, {reply, {Id, ErrCode, ErrMsg}})
    when erlang:is_pid(Pid) ->
    EncodedReply = encode_reply(Id, ErrCode, ErrMsg),
    gen_fsm:sync_send_event(Pid, {reply, EncodedReply}).

%% @doc send to client
-spec send(Who :: binary(), Message :: binary() | iolist()) ->
                  ok | {error, Reason :: term()}.
send(Name, _Message) when erlang:is_binary(Name) ->
    {error, not_impl}.

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
                #state{transport= {_Transport, Control}} = State) ->
    case Control:peername(Socket) of
        {ok, PeerInfo} ->
            Control:setopts(Socket, [{active, once}]),
            %% check if security is enabled, if it is wait for TLS, otherwise go
            %% straight into connected state
            case app_helper:get_env(serv_pb, public, false) of
                true ->
                    {reply, ok, ready,
                     State#state{request=undefined,
                                 socket=Socket,
                                 peername=PeerInfo}};
                _Else ->
                    {reply, ok, wait_for_auth,
                     State#state{request=undefined,
                                 socket=Socket,
                                 peername=PeerInfo}}
            end;
        {error, Reason} ->
            lager:debug("Could not get PB socket peername: ~p", [Reason]),
            %% It's not really "ok", but there's no reason for the
            %% listener to crash just because this socket had an
            %% error. See riak_api#54.
            {stop, normal, Reason, State}
    end;

wait_for_socket(_Event, _From, State) ->
    {reply, unknown_message, wait_for_socket, State}.

%% auth, only for ping
wait_for_auth(timeout, #state{socket = Socket,
                              transport = {Transport, Control},
                              response = Response} = State) ->
    case Transport:send(Socket, Response) of
        ok ->
            Control:setopts(Socket, [{active, once}]),
            {next_state, wait_for_auth, State};
        {error, Reason} ->
            lager:debug("send error: ~p", [Reason]),
            {stop, Reason, State}
    end;

wait_for_auth(_Event, State) ->
    {next_state, wait_for_auth, State}.
%% for select
wait_for_auth({server, Server}, _From,
              #state{socket = Socket,
                     transport = {Transport, Control}} = State) ->
    case Transport:send(Socket, Server) of
        ok ->
            Control:setopts(Socket, [{active, once}]),
            {reply, ok, wait_for_auth, State};
        {error, Reason} ->
            lager:debug("send error: ~p", [Reason]),
            {stop, Reason, State}
    end;

wait_for_auth(_Event, _From, State) ->
    {reply, unknown_message, wait_for_auth, State}.

%% ready
ready(_Event, State) ->
    {next_state, ready, State}.

ready({reply, Reply}, _From, State) ->
    {reply, ok, reply, State#state{response=Reply}, 0};

ready(_Event, _From, State) ->
    {reply, unknown_message, ready, State}.

%% reply
reply(timeout, #state{socket = Socket,
                      transport = {Transport, Control},
                      response = Response} = State) ->
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

%% reply then stop
reply_then_stop(timeout, #state{response=Response,
                                socket = Socket,
                                transport = {Transport, _Control}
                               } = State) ->
    case Transport:send(Socket, Response) of
        ok ->
            {stop, normal, State};
        {error, Reason} ->
            {stop, Reason, State}
    end;

reply_then_stop(_Event, State) ->
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

%% maybe not needed
handle_info({tcp, Socket, _Data}, handle_info, State) ->
    %% req =/= undefined: received a new request while another was in
    %% progress -> Error
    lager:debug("Received a new PB socket request"
                " while another was in progress"),
    Response = encode(#response{errmsg = <<"last request not done">>,
                                errcode = 4}),
    {next_state, reply_then_stop,
     State#state{socket = Socket,
                 response = Response}, 0};

handle_info({tcp, Socket, Packet}, wait_for_auth,
            #state{request=undefined,
                   socket=Socket,
                   peername = _PeerInfo} = State) ->
    Ok = encode(#response{errmsg = <<"Ok">>,
                          errcode = 0}),
    BadPacket = encode(#response{errmsg = <<"bad packet">>,
                              errcode = 1}),
    InternalErr = encode(#response{errmsg = <<"internal error">>,
                                   errcode = 2}),
    NotLogin = encode(#response{errmsg = <<"not login">>,
                                errcode = 3}),
    NotImpl = encode(#response{errmsg = <<"not implement">>,
                               errcode = 4}),
    case parse_packat(Packet) of
        %% unkown packat
        undefined ->
            {next_state, reply_then_stop,
             State#state{response = BadPacket}, 0};
        %% ping packet
        {?PING_CODE, _MsgData} ->
            {next_state, wait_for_auth, State#state{response = Ok}, 0};
        %% select packet
        {?SELECT_CODE, MsgData} ->
            case serv_pb_base_pb:decode(select, MsgData) of
                #select{user = User} ->
                    ok = serv:send({pid, erlang:self()}, {select, User, 1}),
                    {next_state, wait_for_auth, State};
                _Other ->
                    {next_state, reply_then_stop,
                     State#state{response = InternalErr}, 0}
            end;
        %% auth packet, register or login
        {?AUTH_CODE, MsgData} ->
            case serv_pb_base_pb:decode(auth, MsgData) of
                #auth{user = User, password = _Password, how = How} ->
                    case How of
                        1 -> % register
                            %{do_register, User, Password},
                            {next_state, reply_then_stop,
                             State#state{response = Ok}, 0};
                        2 -> % login
                            case serv_pb_session:register(User) of
                                true ->
                                    %% todo: check password and store
                                    {next_state, reply,
                                     State#state{response = Ok,
                                                 session = User}, 0};
                                _Other ->
                                    lager:error("serv_pb_session:register/2", []),
                                    {next_state, reply_then_stop,
                                     State#state{response = InternalErr}, 0}
                            end;
                        _Else ->
                            {next_state, reply_then_stop,
                             State#state{response = NotImpl}, 0}
                    end;
                _Other ->
                    {next_state, reply_then_stop,
                     State#state{response = InternalErr}, 0}
            end;
        % other	packet
        {_MsgCode, _MsgData} ->
            {next_state, reply_then_stop,
             State#state{response = NotLogin}, 0}
    end;

handle_info({tcp, Socket, Packet}, _StateName,
            #state{request=undefined,
                   socket=Socket,
                   states = HandlerStates} = State) ->
    Ok = encode(#response{errmsg = <<"Ok">>,
                          errcode = 0}),
    BadPacket = encode(#response{errmsg = <<"bad packet">>,
                              errcode = 1}),
    NotImpl = encode(#response{errmsg = <<"not implement">>,
                               errcode = 4}),
    InternalErr = encode(#response{errmsg = <<"internal error">>,
                                   errcode = 2}),
    AlreadyLogin = encode(#response{errmsg = <<"already login">>,
                                    errcode = 5}),
    case parse_packat(Packet) of
        undefined ->
            {next_state, reply_then_stop, State#state{response = BadPacket}, 0};
        {?PING_CODE, _MsgData} ->
            {next_state, reply, State#state{response = Ok}, 0};
        {?AUTH_CODE, _MsgData} ->
            {next_state, reply, State#state{response = AlreadyLogin}, 0};
        {MsgCode, MsgData} ->
            case serv_pb_handler:lookup(MsgCode) of
                undefined ->
                    lager:warning("unregistered message: ~p", [MsgCode]),
                    {next_state, reply, State#state{response = NotImpl}, 0};
                Handler when is_atom(Handler) ->
                    case Handler:handle(MsgData, HandlerStates) of
                        {noreply, NewHandlerStates} ->
                            {next_state, ready,
                             State#state{states = NewHandlerStates}};
                        {reply, Response, NewHandlerStates} ->
                            {next_state, reply,
                             State#state{response = Response,
                                         states = NewHandlerStates}, 0};
                        {error, _Reason} ->
                            {next_state, reply_then_stop,
                             State#state{response = InternalErr}, 0}
                    end
            end
    end;

%% other process message to reply
handle_info({message, Message}, ready, State) ->
    {next_state, reply,
     State#state{response = Message}, 0};
%% handle_info({'DOWN', MonitorRef, _Type, Object, _Info}, StateName,
%%          State = #state{}) ->
%%     case erlang:get(Object) of
%%	undefined ->
%%          continue;
%%	{user, User} ->
%%          _Ignore = erlang:erase(User),
%%          _Ignore = erlang:erase(Object)
%%     end,
%%     _Result = erlang:demonitor(MonitorRef, [flush]),
%%     {next_state, StateName, State};
% unknown mesasge
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
         #state{socket = Socket,
                transport = {Transport, _Control},
                session = User}) ->
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
            _Result = serv_pb_session:unregister(User),
            done
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

-spec encode(Error :: #response{}) -> Response::iolist().
encode(Error) ->
    [?RESPONSE_CODE, serv_pb_base_pb:encode(Error)].

-spec encode_reply(integer(), integer(), binary()) -> iolist().
encode_reply(Id, ErrCode, ErrMsg) ->
    Reply = serv_pb_chat_pb:encode(#reply{id=Id,
                                          errcode=ErrCode,
                                          errmsg=ErrMsg}),
    [?REPLY_CODE, Reply].

-spec encode_server(integer(), binary(), binary()) -> iolist().
encode_server(ErrCode, ErrMsg, Ip) ->
    Server = serv_pb_base_pb:encode(#server{errcode=ErrCode,
                                            ip = Ip,
                                            errmsg=ErrMsg}),
    [?SERVER_CODE, Server].
