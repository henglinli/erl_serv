%%
-module(serv_session).
-author('HenryLee<henglinli@gmail.com>').
-include("serv_spdy.hrl").
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).
-export([send/2]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, 5000).

-record(state, {socket::any(),
		transport::module(),
		streams::gb_trees:tree(integer(), any()),
		last_good_id = 0 ::integer()
	       }).
%% utils
debug(Format, Data) ->
    lager:info(Format, Data).

%% API.
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

send(Pid, Frame) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Frame}).

%% gen_server callback
%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
			  #state{socket = Socket,
				 transport = Transport,
				 streams = gb_trees:empty(),
				 last_good_id = 0
				},
			  ?TIMEOUT).

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

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
