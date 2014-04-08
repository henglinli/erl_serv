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
-export([start_link/0, start_link/4]).

-export([send/2]).

%% gen_server callbacks
-export([init/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 5000).

-record(state, {socket::any(),
		transport::module(),
		streams::[integer()],
		last_good_id = 0 ::integer()
	       }).

-record(session, {streams::[integer()],
		  last_good_id = 0 ::integer()
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link({local, ?SERVER}, ?MODULE, init,
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
init([]) ->
   {ok, undefined}.

init(Ref, Socket, Transport, _Opts = [SessionMap], Parent) ->
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    case Transport:peername() of
	{ok, Key} ->
	    Tid = ets:new(?SERVER, []),
	    ets:insert(SessionMap, {Key, Tid}),
	    ok = ranch:accept_ack(Ref),
	    ok = Transport:setopts(Socket, [{active, once}]),
	    gen_server:enter_loop(?MODULE, [],
				  #state{socket = Socket,
					 transport = Transport,
					 streams = [],
					 last_good_id = 0
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
		   streams = _Streams,
		   last_good_id = _LastGoodID
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

handle_info(Info,
	    State=#state{socket = _Socket,
			 transport = Transport,
			 streams = _Streams,
			 last_good_id = _LastGoodID
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

send(Pid, Frame) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Frame}).

%% internal
handle_data(<<>>, _State) ->
    ok;

handle_data(Data, State) ->
    case serv_spdy:split_data(Data) of
	false ->
	    debug("data: ~p", [Data]),
	    send_goaway(State);
	{true, Frame, Rest} ->
	    debug("frame: ~p", [Frame]),
	    handle_frame(Frame, Rest, State)
    end.

handle_frame(Frame, <<>>, #state{socket = Socket,
				 transport = Transport,
				 streams = _Streams,
				 last_good_id = _LastGoodID
				}) ->
    debug("frame: ~p", [Frame]),
    ok = Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, <<"OK">>);

handle_frame(Frame, Rest, State=#state{socket = Socket,
				       transport = Transport,
				       streams = _Streams,
				       last_good_id = _LastGoodID
				      }) ->
    debug("frame: ~p", [Frame]),
    ok = Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, <<"OK">>),
    handle_data(Rest, State).

%% send goway frame
send_goaway(#state{socket = Socket,
		   transport = Transport,
		   streams = _Streams,
		   last_good_id = LastGoodID
		  }) ->
    Reply = serv_spdy:build_frame(
	      #spdy_goaway{
		 version = $l,
		 last_good_id = LastGoodID,
		 status_code =
		     serv_spdy:goaway_status_code(
		       goaway_protocol_error)}),
    ok = Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, Reply).
