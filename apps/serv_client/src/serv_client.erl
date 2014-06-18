%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 10 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_client).

-behaviour(gen_server).

%-include("serv_pb.hrl").
%-include("serv.hrl").
%% API
-export([start_link/1]).
-export([start/1]).
-export([ping/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 3600).

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
-spec start_link(Port::non_neg_integer()) ->
			{ok, Pid::pid()} |
			ignore |
			{error, Error::term() | {already_started, Pid::pid()}}.
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [{port, Port}], []).

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
init(Args) ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    Port = proplists:get_value(port, Args, 10017),
    case gen_tcp:connect(SomeHostInNet, Port,
			 [{send_timeout, ?TIMEOUT},
			  {mode, binary},
			  {packet, 2}
			 ]) of
	{error, Reason} ->
	    {stop, Reason};
	{ok, Socket} ->
	    {ok, #state{socket = Socket}, ?TIMEOUT}
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
	    #state{socket = Socket} = State
	   ) ->
    case Command of
	ping ->
	    Ping = [1],%serv_pb_codec:encode(ping),
	    case gen_tcp:send(Socket, Ping) of
		{error, Reason} ->
		    {stop, Reason, State};
		ok ->
		    {reply, ok, State}
	    end;
	_ ->
	    {noreply, State}
    end;

handle_call(_Request, _From, State) ->
    {noreply, State}.

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
handle_cast(stop, State =
		#state{socket = Socket
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
	    case handle_packet(Data) of
		noreply ->
		    {noreply, State};
		Reply ->
		    case gen_tcp:send(Socket, Reply) of
			{error, Reason} ->
			    {stop, Reason, State};
			ok ->
			    {noreply, State}
		    end
	    end;
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
terminate(Reason, #state{socket = Socket}) ->
    debug("terminate: ~p", [Reason]),
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
    lager:info(Data).

debug(Format, Data) ->
    lager:info(Format, Data).

start(Port) ->
    lager:start(),
    start_link(Port).

stop() ->
    gen_server:cast(?SERVER, stop).

-spec handle_packet(Packet::binary()) ->
			   Reply::noreply | binary().
handle_packet(Packet) ->
    case parse_packat(Packet) of
	undefined ->
	    debug("recved: [~p]", [Packet]),
	    noreply;
	{MsgCode, MsgData} ->
	    %Response = serv_pb_codec:decode(MsgCode, MsgData),
	    debug("recved: [~p:~p]", [MsgCode, MsgData]),
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
ping() ->
    gen_server:call(?SERVER,
		    #request{command = ping}).
