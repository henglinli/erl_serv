%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_session).

-author('HenryLee<henglinli@gmail.com>').

-include("serv_pb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 5000).

-record(session, {pid::pid(),
		  user::[byte() | bitstring()],
		  token::non_neg_integer()
		 }).

-record(state, {socket::any(),
		transport::module(),
		session::#session{}
	       }).

-ifndef(NDEBUG).
-define(DEBUG(Data), lager:info(Data)).
-define(DEBUG(Format, Data), lager:info(Format, Data)).
-else.
-define(DEBUG(_Data), true).
-define(DEBUG(_Format, _Data), true).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(Ref :: ranch:ref(),
		 Socket :: any(),
		 Transport :: module(),
		 Options :: any()) ->
			{ok, ConnectionPid :: pid()}
			    | {error, Reason :: any()}.
start_link(Ref, Socket, Transport, Options) ->
    proc_lib:start_link(?MODULE, init,
			[Ref, Socket, Transport, Options, erlang:self()]).


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
-spec init(Options::any()) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.

-spec init(Ref::ranch:ref(),
	   Socket::any(),
	   Transport::module(),
	   Options::any(),
	   Parent::pid()) -> any().
init(Ref, Socket, Transport, _Options, Parent) ->
    SelfPid = erlang:self(),
    ok = proc_lib:init_ack(Parent, {ok, SelfPid}),
    Session = #session{},
    true = ets:insert(serv_session_map:tid(), {SelfPid, Session}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
			  #state{socket = Socket,
				 transport = Transport,
				 session = Session
				},
			  ?TIMEOUT).

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({send, Chat = #chat{}},
	    State = #state{socket = Socket,
			   transport = Transport
			  }) ->
    Reply = serv_pb_codec:encode(Chat),
    ok = Transport:setopts(Socket, [{active, once}]),
    case Transport:send(Socket, Reply) of
	ok ->
	    {noreply, State};
	{error, Reason} ->
	    {stop, Reason, State}
    end;

handle_cast(_Request, State) ->
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
handle_info(timeout, State) ->
    ?DEBUG("recv timeout, hibernate."),
    {noreply, State, hibernate};

handle_info(Info, State = #state {
			     socket = Socket,
			     transport = Transport,
			     session = Session
			    }) ->
    {OK, Closed, Error} = Transport:messages(),
    case Info of
	{OK, _Socket, Data} ->
	    case handle_packet(Data, Session) of
		{noreply, nochange} ->
		    {noreply, State};
		{noreply, NewSession} ->
		    {noreply,
		     #state{socket = Socket,
			    transport = Transport,
			    session = NewSession}};
		{Reply, NewSession} ->
		    ok = Transport:setopts(Socket, [{active, once}]),
		    case gen_tcp:send(Socket, Reply) of
			{error, Reason} ->
			    {stop, Reason, State};
			ok ->
			    case NewSession of
				nochange ->
				    {noreply, State};
				    _ ->
				    {noreply,
				     #state{socket = Socket,
					    transport = Transport,
					    session = NewSession}}
			    end
		    end
	    end;
	{Closed, _Socket} ->
	    {stop, normal, State};
	{Error, _Socket, Reason} ->
	    {stop, Reason, State}
    end;

handle_info(_Info, State) ->
    {stop, normal, State}.

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
terminate(_Reason, #state{socket = Socket,
			  transport = Transport
			 }) ->
    true = ets:delete(serv_session_map:tid(), erlang:self()),
    Transport:close(Socket).

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

%% API.

-spec handle_packet(Packet::binary(), Session::#session{}) ->
			   {Reply::noreply | binary(),
			    NewSession::nochange | #session{}}.
handle_packet(Packet, Session) ->
    case serv_pb_codec:parse_packat(Packet) of
	undefined ->
	    ?DEBUG("recved: [~p]", [Packet]),
	    Reply = serv_pb_codec:encode(
		      #error_response{errmsg = <<"bad packet">>,
				      errcode = 1}),
	    {Reply, nochange};
	{MsgCode, MsgData} ->
	    Request = serv_pb_codec:decode(MsgCode, MsgData),
	    ?DEBUG("recved: [~p:~p]", [MsgCode, Request]),
	    handle_request(Request, Session)
    end.

-spec handle_request(Request::undefined
			    | info_request
			    | #auth_request{}
			    | #chat{},
		    Session::#session{}) ->
			    {Response::binary(), NewSession::#session{}}.
handle_request(undefined, _Session) ->
    Response = serv_pb_codec:encode(
		 #error_response{errmsg = <<"bad packet">>,
				 errcode = 1}),
    {Response, nochange};

handle_request(info_request, _Session) ->
    Response = serv_pb_codec:encode(
		 #info_response{node = erlang:atom_to_binary(erlang:node(), utf8),
				server_version = <<"1">>}),
    {Response, nochange};

handle_request(#auth_request{user = User,
			    password = _Password}, Session) ->
    true = ets:insert(serv_session_map:tid(), {User, self()}),
    Response = serv_pb_codec:encode(
		 #error_response{errmsg = <<"OK">>,
				 errcode = 1}),
    NewSession = Session#session{user = User},
    {Response, NewSession};

handle_request(#chat{from = From,
		     to = To,
		     msg = Msg
		 }, Session) ->
    case From == Session#session.user of
	true ->
	    gen_server:cast(#chat{from = From,
				  to = To,
				  time = utils:now(),
				  msg = Msg}),
	    {noreply, nochange};
	false ->
	    Response = serv_pb_codec:encode(
			 #error_response{errmsg = <<"not from you">>,
					 errcode = 3}),
	    {Response, nochange}
    end;

handle_request(_, _) ->
    Response = serv_pb_codec:encode(
		 #error_response{errmsg = <<"not implement">>,
				 errcode = 2}),
    {Response, nochange}.
