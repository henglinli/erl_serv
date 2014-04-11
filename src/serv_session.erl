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
-spec init(Options :: any()) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.

-spec init(Ref :: ranch:ref(),
	   Socket :: any(),
	   Transport :: module(),
	   Options :: term(),
	   Parent :: pid()) -> any().
init(Ref, Socket, Transport, _Options, Parent) ->
    SelfPid = erlang:self(),
    ok = proc_lib:init_ack(Parent, {ok, SelfPid}),
    Session = #session{streams = [],
		       last_good_id = 0,
		       ping_id = 0},
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
    debug("recv timeout, hibernate."),
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

%% utils
debug(Data) ->
    lager:info(Data).

debug(Format, Data) ->
    lager:info(Format, Data).

%% API.

-spec send(pid(), binary()) -> ok.
send(Pid, Frame) when erlang:is_pid(Pid) ->
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
	    send_goaway(goaway_protocol_error, State);
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
handle_frame_helper(Frame, State) ->
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
	    debug("ping, id: ~p", [Id]),
	    case Id band 1 of
		1 ->
		    send_server_ping(State);
		_ ->
		    send_goaway(goaway_ok, State)
	    end;
	#spdy_goaway{} ->
	    %% send_server_ping(State);
	    ok;
	_ ->
	    send_goaway(goaway_protocol_error, State)
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
-spec send_goaway(Status :: atom(), State :: #state{}) ->
			 ok | {error, closed | inet:posix()}.
send_goaway(Status,
	    #state{socket = Socket,
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
		     serv_spdy:goaway_status_code(Status)
		}),
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
