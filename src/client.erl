%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 10 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(client).

-behaviour(gen_server).

-include("serv_spdy.hrl").

%% API
-export([start_link/0]).
-export([start/0]).
-export([ping/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 50000).

-record(state, {last_good_id :: integer(),
		client_ping_id :: integer(),
		socket :: gen_tcp:socket()
	       }).

-record(request, {command::atom(), data::binary()}).

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
init(_Args) ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    case gen_tcp:connect(SomeHostInNet, 9999,
			 [{active, true},
			  {send_timeout, 5000}
			 ]) of
	{error, Reason} ->
	    {stop, Reason};
	{ok, Socket} ->
	    %% gen_tcp:controlling_process(Socket, self()),
	    {ok, #state{last_good_id = 0,
			client_ping_id = 1,
			socket = Socket
		       }, ?TIMEOUT}
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
handle_call(#request{command = Command,
		     data = _Data},
	    _From,
	    #state{last_good_id = _LastGoodID,
		   client_ping_id = _ClientPingId,
		   socket = Socket
		  } = State
	   ) ->
    case Command of
	ping ->
	    case send_client_ping(Socket) of
		{error, Reason} ->
		    {stop, Reason, State};
		ok ->
		    {reply, ok, State, ?TIMEOUT}
	    end;
	_ ->
	    {noreply, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?TIMEOUT}.

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
handle_cast(stop, _State) ->
    {stop, normal, _State};

handle_cast(_Msg, _State) ->
    {stop, normal, _State}.

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
    {noreply, State, hibernate};

handle_info(Info, #state{} = State) ->
    case Info of
	{tcp, Socket, Data} ->
	    handle_data(Socket, Data),
	    {noreply, State, ?TIMEOUT};
	{tcp_closed, _Socket} ->
	    {stop, normal, State};
	{tcp_error, _Socket, Reason} ->
	    {stop, Reason, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

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
    error_logger:info_msg(Format, Data).

start() ->
    start_link().

stop() ->
    gen_server:cast(?SERVER, stop).

ping() ->
    gen_server:call(?SERVER,
		    #request{command = ping
			    }).

-spec handle_data(Socket :: gen_tcp:socket(), Packet :: binary()) ->
			 ok | {error, closed | inet:posix()}.
handle_data(_, <<>>) ->
    ok;

handle_data(Socket, Data) ->
    case serv_spdy:split_data(Data) of
	false ->
	    debug("illegal frame: ~p", [Data]),
	    send_goaway(Socket);
	{true, Frame, Rest} ->
	    handle_frame(Socket, Frame, Rest)
    end.

-spec handle_frame(Socket :: gen_tcp:socket(),
		   Frame :: binary(),
		   Rest :: binary()) ->
			  ok | {error, closed | inet:posix()}.
handle_frame(Socket, Frame, <<>>) ->
    handle_frame_helper(Socket, Frame);

handle_frame(Socket, Frame, Rest) ->
    handle_frame_helper(Socket, Frame),
    handle_data(Socket, Rest).

-spec handle_frame_helper(Socket :: gen_tcp:socket(),
			  Frame :: binary()) ->
				 ok | {error, closed | inet:posix()}.
handle_frame_helper(Socket, Frame) ->
    case serv_spdy:parse_frame(Frame) of
	#spdy_data{} ->
	    send_client_ping(Socket);
	#spdy_syn_stream{} ->
	    send_client_ping(Socket);
	#spdy_syn_reply{} ->
	    send_client_ping(Socket);
	#spdy_rst_stream{} ->
	    send_client_ping(Socket);
	#spdy_ping{version = $l,
		   id = Id} ->
	    debug("ping: ~p", [Id]),
	    case Id band 1 of
		1 ->
		    send_goaway(Socket);
		_ ->
		    send_goaway(Socket)
	    end;
	#spdy_goaway{} ->
	    send_client_ping(Socket);
	_ ->
	    send_goaway(Socket)
    end.

%% send goway frame
-spec send_goaway(Socket :: gen_tcp:socket()) ->
			 ok | {error, closed | inet:posix()}.
send_goaway(Socket) ->
    LastGoodID = 0,
    Reply = serv_spdy:build_frame(
	      #spdy_goaway{
		 version = $l,
		 last_good_id = LastGoodID,
		 status_code =
		     serv_spdy:goaway_status_code(
		       goaway_protocol_error)}),
    gen_tcp:send(Socket, Reply).


-spec send_client_ping(Socket :: gen_tcp:socket()) ->
			      ok | {error, closed | inet:posix()}.
send_client_ping(Socket) ->
    ClientPingId = client_ping_id(),
    Reply = serv_spdy:build_frame(
	      #spdy_ping{version = $l,
			 id = ClientPingId}),
    gen_tcp:send(Socket, Reply).

-spec client_ping_id() -> integer().
client_ping_id() ->
    case erlang:get(last_good_id) of
	undefined ->
	    erlang:put(last_good_id, 1),
	    1;
	Value ->
	    erlang:put(last_good_id, Value + 2),
	    Value
    end.
