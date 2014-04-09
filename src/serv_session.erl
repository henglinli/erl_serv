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

-include("serv_spdy.hrl").

-behaviour(gen_server).

%% API
-export([start_link/4]).

-export([send/2]).

%% gen_server callbacks
-export([init/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 5000).

-record(session, {streams :: [integer()],
		  last_good_id = 0 :: integer(),
		  ping_id = 0 :: integer()
		 }).

-record(state, {socket :: any(),
		transport :: module(),
		session :: #session{}
	       }).


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
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init,
			[Ref, Socket, Transport, Opts, self()]).

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
-spec init(Options :: any()) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.

-spec init(Ref :: ranch:ref(),
	   Socket :: any(),
	   Transport :: module(),
	   Options :: any(),
	   Parent :: pid()) ->
		  {ok, ConnectionPid :: pid()} | {error, Reason :: any()}.
init(Ref, Socket, Transport, Opts, Parent) ->
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    MaybeServSession = lists:keyfind(serv_session_map, 1, Opts),
    case Transport:peername(Socket) of
	{ok, Key} ->
	    Tid = ets:new(?SERVER, []),
	    Session = #session{streams = [],
			       last_good_id = 0,
			       ping_id = 0},
	    true = ets:insert(Tid, {session, Session}),
	    case MaybeServSession of
		false ->
		    not_happend_here;
		{serv_session_map, SessionMap} ->
		    ets:insert(SessionMap, {Key, Tid})
	    end,
	    ok = ranch:accept_ack(Ref),
	    ok = Transport:setopts(Socket, [{active, once}]),
	    gen_server:enter_loop(?MODULE, [],
				  #state{socket = Socket,
					 transport = Transport,
					 session = Session
					},
				  ?TIMEOUT);
	{error, Reason} ->
	    {error, Reason}
    end.

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
handle_cast({send, Frame},
	    #state{socket = Socket,
		   transport = Transport,
		   session = #session {
				streams = _Streams,
				last_good_id = _LastGoodID,
				ping_id = _ServerPingId
			       }
		  }) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, Frame);

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
    %% Transport:send(Socket, <<"noreply">>),
    %% {noreply, State, ?TIMEOUT};
    %% {stop, normal, State};
    {noreply, State, hibernate};

handle_info(Info, State = #state {
			     transport = Transport
			    }) ->
    {OK, Closed, Error} = Transport:messages(),
    case Info of
	{OK, _Socket, Data} ->
	    handle_data(Data, State),
	    {noreply, State, ?TIMEOUT};
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

%% utils
debug(Format, Data) ->
    lager:info(Format, Data).

%% API.

-spec send(pid(), binary()) -> ok.
send(Pid, Frame) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Frame}).

%% internal
-spec handle_data(binary(), #state{}) ->
			 ok | {error, closed | inet:posix()}.
handle_data(<<>>, _State) ->
    ok;

handle_data(Data, State) ->
    case serv_spdy:split_data(Data) of
	false ->
	    debug("data: ~p", [Data]),
	    send_goaway(State);
	{true, Frame, Rest} ->
	    handle_frame(Frame, Rest, State)
    end.

-spec handle_frame(binary(), binary(), #state{}) ->
			  ok | {error, closed | inet:posix()}.
handle_frame(Frame, <<>>, State) ->
    handle_frame_helper(Frame, State);

handle_frame(Frame, Rest, State) ->
    handle_frame_helper(Frame, State),
    handle_data(Rest, State).

-spec handle_frame_helper(binary(), #state{}) ->
				 ok | {error, closed | inet:posix()}.
handle_frame_helper(Frame, State = #state {
				      socket = Socket,
				      transport = Transport
				     }) ->
    debug("frame: ~p", [Frame]),
    case serv_spdy:parse_frame(Frame) of
	#spdy_data{} ->
	    send_server_ping(State);
	#spdy_syn_stream{} ->
	    send_server_ping(State);
	#spdy_syn_reply{} ->
	    send_server_ping(State);
	#spdy_rst_stream{} ->
	    send_server_ping(State);
	#spdy_ping{version = $l,
		   id = Id} ->
	    case Id band 1 of
		1 ->
		   send_goaway(State);
		_ ->
		    send_server_ping(State)
	    end;
	#spdy_goaway{} ->
	    send_server_ping(State);
	_ ->
	    send_goaway(State)
    end.

-spec send_reply(Socket :: any(),
		 Transport :: module(),
		 #spdy_data{}
		 | #spdy_syn_stream{}
		 | #spdy_syn_reply{}
		 | #spdy_rst_stream{}
		 | #spdy_ping{}
		 | #spdy_goaway{}) ->
			ok | {error, closed | inet:posix()}.
send_reply(Socket, Transport, Frame) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, Frame).

%% send goway frame
-spec send_goaway(#state{}) ->
			 ok | {error, closed | inet:posix()}.
send_goaway(#state{socket = Socket,
		   transport = Transport,
		   session = #session {
				last_good_id = LastGoodID
			       }
		  }) ->
    Reply = serv_spdy:build_frame(
	      #spdy_goaway{
		 version = $l,
		 last_good_id = LastGoodID,
		 status_code =
		     serv_spdy:goaway_status_code(
		       goaway_protocol_error)}),
    send_reply(Socket, Transport, Reply).

-spec send_server_ping(#state{}) ->
			      ok | {error, closed | inet:posix()}.
send_server_ping(#state{socket = Socket,
			transport = Transport,
			session = #session {
				     streams = _Streams,
				     last_good_id = _LastGoodID,
				     ping_id = ServerPingId
				    }
		       }) ->

    Reply = serv_spdy:build_frame(
	      #spdy_ping{version = $l,
			 id = ServerPingId}),
    send_reply(Socket, Transport, Reply).

%% %% create session, not used yet
%% -spec create_session(Tid :: ets:tid() | atom()) -> true.
%% create_session(Tid) ->
%%     ets:insert(Tid, {session, #session{
%%				 streams = [],
%%				 last_good_id = 0,
%%				 ping_id = 0}}).
%% -spec create_ping_counter(Tid :: ets:tid() | atom()) -> true.
%% create_ping_counter(Tid) ->
%%     ets:insert(Tid, {ping_id, 0}).
