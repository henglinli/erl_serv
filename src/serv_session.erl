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
	    State = #state{socket = Socket,
		   transport = Transport
		  }) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    case Transport:send(Socket, Frame) of
	 ok ->
	    {noreply, State, ?TIMEOUT};
	{error, Reason} ->
	    {stop, Reason, State}
    end;

handle_cast(_Request, State) ->
    {noreply, State, ?TIMEOUT}.

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
	    case handle_data(Data, Session) of
		{Reply = #spdy_goaway{}, _Session} ->
		    ok = Transport:setopts(Socket, [{active, once}]),
		    case Transport:send(Socket, Reply) of
			ok ->
			    {stop, normal, State};
			{error, Reason} ->
			    {stop, Reason, State}
		    end;
		{Reply, NewSession} ->
		    ok = Transport:setopts(Socket, [{active, once}]),
		    case Transport:send(Socket, Reply) of
			ok ->
			    {noreply, #state {
					 socket = Socket,
					 transport = Transport,
					 session = NewSession
					}, ?TIMEOUT};
			{error, Reason} ->
			    {stop, Reason, State}
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
-spec send(pid(), binary()) -> ok.
send(Pid, Frame) when erlang:is_pid(Pid) ->
    gen_server:cast(Pid, {send, Frame}).

%% internal
-spec handle_data(Data :: binary(), Session :: #session{}) ->
			 {Replay :: #spdy_data{}
				  | #spdy_syn_stream{}
				  | #spdy_syn_reply{}
				  | #spdy_rst_stream{}
				  | #spdy_ping{}
				  | #spdy_goaway{},
			  NewSession :: #session{}}.
handle_data(Data, Session = #session{
			       last_good_id = LastGoodID
			      }) ->
    case serv_spdy:split_data(Data) of
	false ->
	    ?DEBUG("data: ~p", [Data]),
	    Reply = serv_spdy:build_frame(
		      #spdy_goaway{
			 version = $l,
			 last_good_id = LastGoodID,
			 status_code =
			     serv_spdy:goaway_status_code(goaway_protocol_error)
			}),
	    {Reply, Session};
	{true, Frame, Rest} ->
	    handle_frame(Frame, Rest, Session)
    end.

-spec handle_frame(Frame :: binary(), Rest :: binary(), Session :: #session{}) ->
			  {Replay :: #spdy_data{}
				   | #spdy_syn_stream{}
				   | #spdy_syn_reply{}
				   | #spdy_rst_stream{}
				   | #spdy_ping{}
				   | #spdy_goaway{},
			   NewSession :: #session{}}.
handle_frame(Frame, <<>>, Session) ->
    handle_frame_helper(Frame, Session);

handle_frame(Frame, Rest, Session) ->
    handle_frame_helper(Frame, Session),
    handle_data(Rest, Session).

-spec handle_frame_helper(Frame :: binary(), Session :: #session{}) ->
				 {Replay :: #spdy_data{}
					  | #spdy_syn_stream{}
					  | #spdy_syn_reply{}
					  | #spdy_rst_stream{}
					  | #spdy_ping{}
					  | #spdy_goaway{},
				  NewSession :: #session{}}.
handle_frame_helper(Frame, Session) ->
    case serv_spdy:parse_frame(Frame) of
	#spdy_data{} ->
	    handle_undefined(Session);
	#spdy_syn_stream{} ->
	    handle_undefined(Session);
	#spdy_syn_reply{} ->
	    handle_undefined(Session);
	#spdy_rst_stream{} ->
	    handle_undefined(Session);
	#spdy_ping{version = $l,
		   id = PingId} ->
	    ?DEBUG("ping, id: ~p", [PingId]),
	    handle_client_ping(PingId, Session);
	#spdy_goaway{} ->
	    handle_undefined(Session);
	_ ->
	    handle_undefined(Session)
    end.

-spec goaway(Status :: atom(), Session :: #session{}) ->
		   {Reply :: #spdy_goaway{},
		    NewSession :: #session{}}.
goaway(Status, Session = #session {
		 last_good_id = LastGoodID
		}) ->
    ?DEBUG("goway, last good id: ", [LastGoodID]),
    Reply = serv_spdy:build_frame(
	      #spdy_goaway{
		 version = $l,
		 last_good_id = LastGoodID,
		 status_code =
		     serv_spdy:goaway_status_code(Status)
		}),
    {Reply, Session}.

-spec handle_undefined(Session :: #session{}) ->
			      {Reply :: #spdy_goaway{},
			       NewSession :: #session{}}.
handle_undefined(Session) ->
    goaway(goaway_protocol_error, Session).


-spec handle_client_ping(PingId :: integer(), Session :: #session{}) ->
				{Reply :: #spdy_ping{}
					| #spdy_goaway{},
				 NewSession :: #session{}}.
handle_client_ping(PingId, Session = #session {
					streams = Streams,
					last_good_id = LastGoodID,
					ping_id = ServerPingId
				       }) ->
    case PingId band 1 of
	1 ->
	    Reply = serv_spdy:build_frame(
		      #spdy_ping{version = $l,
				 id = ServerPingId}),
	    {Reply, #session {
		       streams = Streams,
		       last_good_id = LastGoodID,
		       ping_id = ServerPingId + 2}};
	_ ->
	    goaway(goaway_protocol_error, Session)
    end.
