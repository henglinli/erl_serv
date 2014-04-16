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

-include("serv_pb.hrl").

%% API
-export([start_link/0]).
-export([start/0]).
-export([ping/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 5000).

-record(state, {socket :: gen_tcp:socket()
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
			 [{send_timeout, ?TIMEOUT},
			  {mode, binary},
			  {packet, 2}
			 ]) of
	{error, Reason} ->
	    {stop, Reason};
	{ok, Socket} ->
	    %% gen_tcp:controlling_process(Socket, self()),
	    {ok, #state{socket = Socket
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
	    #state{socket = Socket
		  } = State
	   ) ->
    case Command of
	ping ->
	    Ping = #info_response{node = erlang:node()},
	    %BPing = erlang:term_to_binary(Ping),
	    case gen_tcp:send(Socket, Ping) of
		{error, Reason} ->
		    {stop, Reason, State};
		ok ->
		    {reply, ok, State, ?TIMEOUT}
	    end;
	_ ->
	    {noreply, State}
    end;

handle_call(_Request, _From, State) ->
    {noreply, State, ?TIMEOUT}.

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
handle_cast(stop, State = #state{socket = Socket
		    }) ->
    ok = gen_tcp:close(Socket),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {stop, normal, State}.

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
    debug("recv timeout"),
    {noreply, State, hibernate};

handle_info(Info, #state{} = State) ->
    case Info of
	{tcp, Socket, Data} ->
	    handle_data(Socket, Data),
	    {noreply, State, ?TIMEOUT};
	{tcp_closed, _Socket} ->
	    debug("connection closed", []),
	    {stop, normal, State};
	{tcp_error, _Socket, Reason} ->
	    debug("connection error: ~p", [Reason]),
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
terminate(_Reason, #state{socket = Socket
		      }) ->
    gen_tcp:close(Socket).

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
    error_logger:info_msg(Data).

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
handle_data(_Socket, Data) ->
    debug("recved: ~p", [Data]).
