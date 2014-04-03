-module(client).
-include("serv_spdy.hrl").
-export([t/0]).

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).

t() ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 9999,
				 [{active, false},
				  {send_timeout, 5000}
				 ]),
    Ping = serv_spdy:build_frame(#spdy_ping{
				    version = $l,
				    id = 99}),
    debug("Ping: ~p", [Ping]),
    ok = gen_tcp:send(Sock, Ping),
    Reply = gen_tcp:recv(Sock, 0, 5000),
    ok = gen_tcp:close(Sock),
    Reply.

%    loop(Sock).

loop(Sock) ->
    receive
	{Client, send_data, Binary} ->
	    case gen_tcp:send(Sock,[Binary]) of
		{error, timeout} ->
		    io:format("Send timeout, closing!~n",
			      []),
		    %handle_send_timeout(), % Not implemented here
		    Client ! {self(),{error_sending, timeout}},
		    %% Usually, it's a good idea to give up in case of a
		    %% send timeout, as you never know how much actually
		    %% reached the server, maybe only a packet header?!
		    gen_tcp:close(Sock);
		{error, OtherSendError} ->
		    io:format("Some other error on socket (~p), closing",
			      [OtherSendError]),
		    Client ! {self(),{error_sending, OtherSendError}},
		    gen_tcp:close(Sock);
		ok ->
		    Client ! {self(), data_sent},
		    loop(Sock)
	    end
    end.
