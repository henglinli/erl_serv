%%
-module(serv_protocol).
-author('HenryLee<henglinli@gmail.com>').
-include("serv_spdy.hrl").
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

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

%% API.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).

%% gen_server.

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

handle_info(Info,
	    State=#state{socket = Socket,
			 transport = Transport,
			 streams = Streams,
			 last_good_id = LastGoodID
			}) ->
    debug("~p ~p", [Info, Transport]),
    {OK, Closed, Error} = Transport:messages(),
    case Info of
	{OK, _Socket, Data} ->
	    ok = Transport:setopts(Socket, [{active, once}]),
	    case serv_spdy:split_data(Data) of
		false ->
		    Reply = serv_spdy:build_frame(
			      #spdy_goaway{version = $l,
					   last_good_id = LastGoodID,
					   status_code = 
					       serv_spdy:goaway_status_code(
						 goaway_protocol_error)}),
		    Transport:send(Socket, Reply),
		    {stop, normal, State};
		{true, Frame, Rest} ->
		    Transport:send(Socket, <<"OK">>),		    
		    {noreply, State, hibernate}
	    end;
	{Closed, _Socket} ->
	    {stop, normal, State};
	{Error, _Socket, Reason} ->
	    {stop, Reason, State};
	timeout ->
	    %% Transport:send(Socket, <<"noreply">>),
	    %% {noreply, State, ?TIMEOUT};
	    %% {stop, normal, State};
	    {noreply, State, hibernate};
	_Info ->
	    {stop, normal, State}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal.
