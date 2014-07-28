%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 26 Jun 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_client).

-author('HenryLee<henglinli@gmail.com>').

-behaviour(gen_fsm).

-include_lib("serv/include/serv.hrl").
-include_lib("serv/include/serv_pb_base_pb.hrl").
-include_lib("serv/include/serv_pb_chat_pb.hrl").

%% API
-export([start_link/0, stop/1]).
-export([connect/3, connect/1, connect/2,
         select/2,
         login/3, login/1, login/2,
         ping/1,
         chat/3, chat/2
        ]).
%% test
-export([test/1,
	 chat_one/0,
         login_one/0,
         chat_test/0
        ]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([do_connect/2, do_connect/3,
         do_login/2, do_login/3,
         do_request/2, do_request/3]).

-record(state, {socket = undefined :: undefined | gen_tcp:socket(),
                self = <<>> :: binary(),
                password = <<>> :: binary()}).

-record(request, {command = undefined ::atom(),
                  data = <<>> ::binary()}).

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
    gen_fsm:start_link(?MODULE, [], []).

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
    {ok, do_connect, #state{}}.

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
do_connect(_Event, State) ->
    {next_state, do_connect, State}.

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
do_connect({connect, Host, Port}, _From, State) ->
    case gen_tcp:connect(Host, Port,
                         [{send_timeout, ?TIMEOUT},
                          {mode, binary},
                          {active, true},
                          {packet, 2}
                         ]) of
        {error, Reason} ->
            {stop, Reason, {error, Reason}, State};
        {ok, Socket} ->
            {reply, ok, do_login, State#state{socket = Socket}}
    end;

do_connect(_Event, _From, State) ->
    {reply, not_connected, do_connect, State}.

%% do login
do_login(_Event, State) ->
    {next_state, do_login, State}.

do_login({select, User}, _From, #state{socket = Socket} = State) ->
    ProtobufSelect = #select{user=User},
    Select = serv_pb_base_pb:encode(ProtobufSelect),
    case gen_tcp:send(Socket, [?SELECT_CODE, Select]) of
        {error, Reason} ->
            {stop, Reason, {error, Reason}, State};
        ok ->
            {reply, ok, do_login, State#state{self = User}}
    end;

do_login({login, NewSelf, Password}, _From,
         #state{socket = Socket} = State) ->
    ProtobufAuth = #auth{user = NewSelf, password = Password, how = 2},
    Auth = serv_pb_base_pb:encode(ProtobufAuth),
    case gen_tcp:send(Socket, [?AUTH_CODE, Auth]) of
        {error, Reason} ->
            {stop, Reason, {error, Reason}, State};
        ok ->
            {reply, ok, do_request, State#state{self = NewSelf}}
    end;

do_login(_Event, _From, State) ->
    {reply, unknown_message, do_login, State}.

%% do request
do_request(_Event, State) ->
    {next_state, do_request, State}.

do_request(#request{command = chat, data = {To, Msg}}, _From,
           #state{socket = Socket, self = Self} = State) ->
    ProtobufChat = #chat{from = Self, to = To,
                         time = s(os:timestamp()),
                         msg = Msg},
    Chat = serv_pb_chat_pb:encode(ProtobufChat),
    case gen_tcp:send(Socket, [?CHAT_CODE, Chat]) of
        {error, Reason} ->
            {stop, Reason, {error, Reason}, State};
        ok ->
            {reply, ok, do_request, State}
    end;

do_request(_Event, _From, State) ->
    {reply, not_impl, do_request, State}.

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

%% handle_event(_Event, StateName, State = #state{socket = Socket}) ->
%%     case Socket of
%%	undefined ->
%%          {stop, shutdown, State};
%%	_Socket ->
%%          gen_tcp:close(Socket),
%%          {stop, shutdown, State}
%%     end.

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
handle_sync_event(ping, _From, do_connect, State) ->
    {reply, not_connected, do_connect, State};

handle_sync_event(ping, _From, StateName,
                  State = #state{socket = Socket}) ->
    Ping = [?PING_CODE, <<"ping">>], %serv_pb_codec:encode(ping),
    case gen_tcp:send(Socket, Ping) of
        {error, Reason} ->
            {stop, Reason, {error, Reason}, State};
        ok ->
            {reply, ok, StateName, State}
    end;

handle_sync_event(stop, _From, _StateName, State) ->
    {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, not_support, StateName, State}.

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
handle_info({tcp_closed, _Socket}, _StateName, State) ->
    lager:info("connection closed", []),
    {stop, normal, State#state{socket = undefined}};

handle_info({tcp_error, _Socket, Reason}, _StateName, State) ->
    lager:info("connection error: ~p", [Reason]),
    {stop, Reason, State#state{socket = undefined}};

handle_info({tcp, Socket, Data}, StateName, State) ->
    case handle_packet(Data) of
        noreply ->
            {next_state, StateName, State};
        {address, Ip} ->
            Address = erlang:binary_to_list(Ip),
            case inet:parse_address(Address) of
                {ok, IPAddress} ->
                    case inet:peername(Socket) of
                        {ok, {IPAddress, _Port}} ->
                            {next_state, StateName, State};
                        {ok, _} ->
                            lager:info("please reconnect to ~p", [Ip]),
                            {stop, normal, State};
                        {error, Reason} ->
                            {stop, Reason, State}
                    end;
                {error, einval} ->
                    {stop, einval, State}
            end;
        {stop, ErrMsg} ->
            {stop, ErrMsg, State};
        {reply, Reply} ->
            case gen_tcp:send(Socket, Reply) of
                {error, Reason} ->
                    {stop, Reason, State};
                ok ->
                    {next_state, StateName, State}
            end
    end;

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
terminate(_Reason, _StateName,
          #state{socket = undefined}) ->
    lager:info("Bye!", []),
    ok;

terminate(_Reason, _StateName,
          #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    lager:info("Bye!", []),
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
-spec handle_packet(Packet::binary()) ->
                           noreply | {address | stop | reply, binary()}.
handle_packet(Packet) ->
    case parse_packat(Packet) of
        undefined ->
            lager:info("recved: [~p]", [Packet]),
            noreply;
        {?RESPONSE_CODE, MsgData} ->
            #response{errcode = ErrCode, errmsg = ErrMsg}
                = serv_pb_base_pb:decode(response, MsgData),
            lager:info("recved: {~p, ~p}", [ErrCode, ErrMsg]),
            noreply;
        {?SERVER_CODE, MsgData} ->
            #server{errcode=ErrCode, errmsg=ErrMsg, ip=Ip}
                = serv_pb_base_pb:decode(server, MsgData),
            case ErrCode of
                0 ->
                    {address, Ip};
                _Else ->
                    lager:info("select server error: {~p, ~p}", [ErrCode, ErrMsg]),
                    {stop, ErrMsg}
            end;
        {?CHAT_ID_CODE, MsgData} ->
            #chat_id{id = Id}
                = serv_pb_chat_pb:decode(chat_id, MsgData),
            lager:info("chat_id: ~p", [Id]),
            noreply;
        {?REPLY_CODE, MsgData} ->
            #reply{id = Id, errcode = ErrCode, errmsg = ErrMsg}
                = serv_pb_chat_pb:decode(reply, MsgData),
            lager:info("chat_id: ~p -> {~p, ~p}", [Id, ErrCode, ErrMsg]),
            noreply;
        {?SERVER_CHAT_CODE, MsgData} ->
            #chat{from = From, to = To, msg = Msg, time = Time}
                = serv_pb_chat_pb:decode(chat, MsgData),
            lager:info("{~p, ~p, ~p, ~p}", [Time, From, To, Msg]),
            noreply;
        {MsgCode, MsgData} ->
            %Response = serv_pb_codec:decode(MsgCode, MsgData),
            lager:info("recved: [~p: ~p]", [MsgCode, MsgData]),
            noreply
    end.

-spec parse_packat(Packet::binary()) ->
                         undefined | {MsgCode::integer(), MsgData::binary()}.
parse_packat(<<MsgCode:8/big-unsigned-integer,
                       MsgData/binary>>) ->
    {MsgCode, MsgData};

parse_packat(_) ->
    undefined.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stop
stop(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, stop).

%% connect
-spec connect(pid(), inet:ip_address() | inet:hostname(), inet:port_number()) ->
                     ok | {error, inet:posix()}.
connect(Pid, Host, Port) ->
    gen_fsm:sync_send_event(Pid, {connect, Host, Port}).

connect(Pid, Port) ->
    connect(Pid, "localhost", Port).

connect(Pid) ->
    connect(Pid, "localhost", 8087).

%%
-spec select(Pid :: pid(), Self :: binary()) ->
                    ok | {error, term()}.
select(Pid, Self) ->
    gen_fsm:sync_send_event(Pid, {select, Self}).

%% login
-spec login(Pid :: pid(), Self :: binary(), Pasword :: binary()) ->
                   ok | {error, term()}.
login(Pid, Self, Pasword) ->
    gen_fsm:sync_send_event(Pid, {login, Self, Pasword}).

login(Pid, Self) ->
    login(Pid, Self, Self).

login(Pid) ->
    login(Pid, <<"lee">>, <<"lee">>).

%% ping
-spec ping(pid()) -> ok | {error, term()}.
ping(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, ping).

%% chat
-spec chat(Pid :: pid(), To :: binary(), Msg :: binary()) -> undefined | ok.
chat(Pid, To, Msg) ->
    Chat = {To, Msg},
    gen_fsm:sync_send_event(Pid, #request{command = chat,
                                          data = Chat}).

chat(Pid, To) ->
    chat(Pid, To, <<"hello!">>).

%% -spec ms(erlang:timestamp()) -> pos_integer().
%% ms({MegaSecs, Secs, MicroSecs}) ->
%%     MegaSecs*100000000 + Secs*1000 + MicroSecs.

-spec s(erlang:timestamp()) -> pos_integer().
s({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs*100000 + Secs.

%%
-spec test(Clients :: pos_integer()) -> term().
test(Clients) ->
    test_helper(Clients, ok).

test_helper(0, ok) ->
    ok;

test_helper(_N, error) ->
    error;

test_helper(N, ok) ->
    Result = do_test(N),
    test_helper(N - 1, Result).

do_test(N) ->
    Id = erlang:integer_to_binary(N),
    User = <<"lee@", Id/binary>>,
    case serv_client_sup:start_child() of
        {ok , Pid} ->
            serv_client:connect(Pid),
            serv_client:login(Pid, User),
            ok;
        {error, _Reason} ->
            error
    end.

login_one() ->
    application:start(serv_client),
    case serv_client_sup:start_child() of
        {ok , Pid} ->
            ok = serv_client:connect(Pid, "localhost", 8087),
	    ok = serv_client:login(Pid, <<"lee">>),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

chat_one() ->
    application:start(serv_client),
    case serv_client_sup:start_child() of
        {ok , Pid} ->
            ok = serv_client:connect(Pid, "localhost", 8087),
	    ok = serv_client:login(Pid, <<"lee">>),
	    ok = serv_client:chat(Pid, <<"google">>),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

chat_test() ->
    application:start(serv_client),
    case serv_client_sup:start_child() of
        {ok, Pid} ->
            ok = serv_client:connect(Pid, "localhost", 8087),
	    ok = serv_client:login(Pid, <<"lee">>),
            case serv_client_sup:start_child() of
                {ok, Pid1} ->
                    ok = serv_client:connect(Pid1, "localhost"),
		    ok = serv_client:login(Pid, <<"google">>),
                    ok = serv_client:chat(Pid1, <<"lee">>),
                    ok = serv_client:chat(Pid, <<"google">>),
                    ok;
                {error, _Reason} ->
                    error
            end;
        {error, _Reason} ->
            error
    end.
