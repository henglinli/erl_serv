%% @doc entry point for TCP-based protocol buffers service

-module(serv_pb_listener).
-behaviour(gen_nb_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([sock_opts/0, new_connection/2]).
-export([get_listeners/0]).
-record(state, {portnum}).

%% @doc Starts the PB listener
-spec start_link(inet:ip_address() | string(),  non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(IpAddr, PortNum) ->
    gen_nb_server:start_link(?MODULE, IpAddr, PortNum, [PortNum]).

%% @doc Initialization callback for gen_nb_server.
-spec init(list()) -> {ok, #state{}}.
init([PortNum]) ->
    {ok, #state{portnum=PortNum}}.

%% @doc Preferred socket options for the listener.
-spec sock_opts() -> [gen_tcp:option()].
sock_opts() ->
    BackLog = app_helper:get_env(serv_pb, pb_backlog, 128),
    NoDelay = app_helper:get_env(serv_pb, disable_pb_nagle, true),
    [binary, {packet, 2}, {reuseaddr, true}, {backlog, BackLog}, {nodelay, NoDelay}].

%% @doc The handle_call/3 gen_nb_server callback. Unused.
-spec handle_call(term(), {pid(),_}, #state{}) -> {reply, term(), #state{}}.
handle_call(_Req, _From, State) ->
    {reply, not_implemented, State}.

%% @doc The handle_cast/2 gen_nb_server callback. Unused.
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) -> {noreply, State}.

%% @doc The handle_info/2 gen_nb_server callback. Unused.
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) -> {noreply, State}.

%% @doc The code_change/3 gen_nb_server callback. Unused.
-spec terminate(Reason, State) -> ok when
      Reason :: normal | shutdown | {shutdown,term()} | term(),
      State :: #state{}.
terminate(_Reason, _State) ->
    ok.

%% @doc The gen_server code_change/3 callback, called when performing
%% a hot code upgrade on the server. Currently unused.
-spec code_change(OldVsn, State, Extra) -> {ok, State} | {error, Reason}
					       when
      OldVsn :: Vsn | {down, Vsn},
      Vsn :: term(),
      State :: #state{},
      Extra :: term(),
      Reason :: term().
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @doc The connection initiation callback for gen_nb_server, called
%% when a new socket is accepted.
-spec new_connection(inet:socket(), State::term()) ->
			    {ok, NewState::term()} |
			    {stop, Reason::term(), NewState::term()}.
new_connection(Socket, State) ->
    %%{ok, Pid} = serv_pb_server_sup:start_socket(),
    case serv_pb_server:start_link() of
	{ok, Pid} ->
	    ok = gen_tcp:controlling_process(Socket, Pid),
	    ok = serv_pb_server:set_socket(Pid, Socket),
	    {ok, State};
	{error, Reason}->
	    {stop, Reason, State}
    end.

get_listeners() ->
    Listeners = app_helper:get_env(serv_pb, pb, [{"127.0.0.1", 8087}]),
    [ {I, P} || {I, P} <- Listeners ].
