-module(client).
-behaviour(gen_server).
-include("serv_spdy.hrl").

%% interface
-export([start/0]).
-export([t/1]).
-export([ping/0]).
-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {socket::gen_tcp:socket()}).
-record(request, {command::atom(), data::binary()}).

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).

start() ->
    start_link().

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init(_Args) ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    case gen_tcp:connect(SomeHostInNet, 9999,
			 [{active, false},
			  {send_timeout, 5000}
			 ]) of
	{error, Reason} ->
	    {stop, Reason};
	{ok, Socket} ->
	    {ok, #state{socket = Socket}}
    end.

ping() ->
    Ping = serv_spdy:build_frame(#spdy_ping{
				    version = $l,
				    id = 99}),
    t(Ping).

stop() ->
    gen_server:cast(?MODULE, stop).

t(Data) ->
    gen_server:call(?MODULE,
		    #request{command = send_and_recv,
			     data = Data
			    }).

do_send_and_recv(Socket, Data) ->
     case gen_tcp:send(Socket, Data) of
	 {error, timeout} ->
	     debug("send timeout", []),
	     gen_tcp:close(Socket);
	 {error, OtherSendError} ->
	     debug("send error ~p", [OtherSendError]),
	     gen_tcp:close(Socket);
	 ok ->
	     case gen_tcp:recv(Socket, 0) of
		 {error, Reason} ->
		     debug("recv error ~p", [Reason]),
		     gen_tcp:close(Socket);
		 {ok, Packet} ->
		     Packet
	     end
     end.

handle_call(#request{
	       command = send_and_recv,
	       data = Data
	      },
	    _From,
	    #state{socket = Socket} = State) ->
    case do_send_and_recv(Socket, Data) of
	ok ->
	    {stop, normal, State};
	Packet ->
	    {reply, Packet, State}
    end;

handle_call(#request{
	       command = stop,
	       data = _Data
	      },
	    _From,
	    #state{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, _State) ->
    {stop, normal, _State}.

terminate(_Reason, _State) ->
    ok.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
