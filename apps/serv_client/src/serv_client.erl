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

-include_lib("serv_pb/include/serv_pb_base_pb.hrl").
-include_lib("serv/include/serv_pb_chat_pb.hrl").

%% API
-export([start/0]).
-export([stop/0,
	 connect/0, connect/1, connect/2,
	 login/0, login/1, login/2,
	 ping/0,
	 chat/1, chat/2
	]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4]).

-export([do_connect/2, do_connect/3,
	 do_login/2, do_login/3,
	 do_request/2, do_request/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 3600).

-record(state, {socket = undefined :: undefined | gen_tcp:socket(),
		self = <<>> :: binary()}).

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
    {reply, not_impl, do_connect, State}.

%% do login
do_login(_Event, State) ->
    {next_state, do_login, State}.

do_login({login, NewSelf, Password}, _From,
	 State = #state{socket = Socket}) ->
    ProtobufAuth = #auth{user = NewSelf, password = Password},
    Auth = serv_pb_chat_pb:encode(ProtobufAuth),
    case gen_tcp:send(Socket, [3, Auth]) of
	{error, Reason} ->
	    {stop, Reason, {error, Reason}, State};
	ok ->
	    {reply, ok, do_request, State#state{self = NewSelf}}
    end;

do_login(_Event, _From, State) ->
    {reply, not_impl, do_login, State}.

%% do request
do_request(_Event, State) ->
    {next_state, do_request, State}.

do_request(#request{command = chat, data = {To, Msg}}, _From,
	   State = #state{socket = Socket, self = Self}) ->
    ProtobufChat = #chat{from = Self, to = To,
			 time = s(os:timestamp()),
			 msg = Msg},
    Chat = serv_pb_chat_pb:encode(ProtobufChat),
    case gen_tcp:send(Socket, [5, Chat]) of
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
%%	    {stop, shutdown, State};
%%	_Socket ->
%%	    gen_tcp:close(Socket),
%%	    {stop, shutdown, State}
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
handle_sync_event(ping, _From, StateName,
		  State = #state{socket = Socket}) ->
    case StateName of
	do_connect ->
	    {reply, not_connected, StateName, State};
	_OtherStae ->
	    Ping = [1, <<"ping">>],%serv_pb_codec:encode(ping),
	    case gen_tcp:send(Socket, Ping) of
		{error, Reason} ->
		    {stop, Reason, {error, Reason}, State};
		ok ->
		    {reply, ok, StateName, State}
	    end
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
    {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, _StateName, State) ->
    lager:info("connection error: ~p", [Reason]),
    {stop, Reason, State};

handle_info({tcp, Socket, Data}, StateName, State) ->
    case handle_packet(Data) of
	noreply ->
	    {next_state, StateName, State};
	Reply ->
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
    ok;

terminate(_Reason, _StateName,
	  #state{socket = Socket}) ->
    gen_tcp:close(Socket),
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
			   Reply::noreply | binary().
handle_packet(Packet) ->
    case parse_packat(Packet) of
	undefined ->
	    lager:info("recved: [~p]", [Packet]),
	    noreply;
	{0, MsgData} ->
	    #response{errcode = ErrCode, errmsg = ErrMsg}
		= serv_pb_base_pb:decode(response, MsgData),
	    lager:info("recved: {~p, ~p}", [ErrCode, ErrMsg]),
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
%% utils
%%
start() ->
    lager:start(),
    start_link().

%% stop
stop() ->
    gen_fsm:sync_send_all_state_event(?SERVER, stop).

%% connect
-spec connect(inet:ip_address() | inet:hostname(), inet:port_number()) ->
		     ok | {error, inet:posix()}.
connect(Host, Port) ->
    gen_fsm:sync_send_event(?SERVER, {connect, Host, Port}).

connect(Port) ->
    connect("localhost", Port).

connect() ->
    connect("localhost", 8087).

%% login
-spec login(Self :: binary(), Pasword :: binary()) ->
		   ok | {error, term()}.
login(Self, Pasword) ->
    gen_fsm:sync_send_event(?SERVER, {login, Self, Pasword}).

login(Self) ->
    login(Self, Self).

login() ->
    login(<<"lee">>, <<"lee">>).

%% ping
-spec ping() -> ok | {error, term()}.
ping() ->
    gen_fsm:sync_send_all_state_event(?SERVER, ping).

%% chat
-spec chat(To :: binary(), Msg :: binary()) -> undefined | ok.
chat(To, Msg) ->
    Chat = {To, Msg},
    gen_fsm:sync_send_event(?SERVER,
			    #request{command = chat,
				     data = Chat}).

chat(To) ->
    chat(To, <<"hello!">>).

%% -spec ms(erlang:timestamp()) -> pos_integer().
%% ms({MegaSecs, Secs, MicroSecs}) ->
%%     MegaSecs*100000000 + Secs*1000 + MicroSecs.

-spec s(erlang:timestamp()) -> pos_integer().
s({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs*100000 + Secs.
