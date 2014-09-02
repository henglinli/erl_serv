%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  2 Sep 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_listener).

-author('henglinli@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {callback :: module()}).

-callback sock_opts() -> [gen_tcp:listen_option()].

-callback new_connection(inet:socket()) ->
    ok |
    {error, Reason::term()}.

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
start_link(Callback, IpAddr, Port) when erlang:is_atom(Callback) ->
    gen_server:start_link({local, Callback},
			  ?MODULE, [Callback, IpAddr, Port], []).

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
init([Callback, IpAddr, Port]) ->
    case listen_on(Callback, IpAddr, Port) of
	{ok, _Socket} ->
	    {ok, #state{callback=Callback}};
	Error ->
	    {stop, Error}
    end;

init(_) ->
    {stop, badarg}.
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
handle_cast(_Msg, State) ->
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
handle_info({inet_async, ListenSocket, _Ref, {ok, ClientSocket}},
	    #state{callback=Callback}=State) ->
  inet_db:register_socket(ListenSocket, inet_tcp),
  case Callback:new_connection(ClientSocket) of
      ok ->
	  case prim_inet:async_accept(ListenSocket, -1) of
	      {ok, _NewRef} ->
		  {noreply, State};
	      {error, Reason} ->
		  {stop, Reason, State}
	  end;
      {error, Reason} ->
	  {stop, Reason, State}
  end;

handle_info({inet_async, _ListenSocket, _Ref, Error}, State) ->
    {stop, Error, State};

handle_info(_Info, State) ->
    lager:info("~p", [_Info]),
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

-spec listen_on(module(), inet:ip_address() | string(), non_neg_integer()) ->
		       {ok, inet:socket()} | {error, term()}.

listen_on(Callback, IpAddr, Port) when is_tuple(IpAddr) andalso
					     (8 =:= size(IpAddr) orelse
					      4 =:= size(IpAddr)) ->
    SockOpts = [{ip, IpAddr}|Callback:sock_opts()],
    case gen_tcp:listen(Port, SockOpts) of
	{ok, ListenSocket} ->
	    case prim_inet:async_accept(ListenSocket, -1) of
		{ok, _Ref} ->
		    {ok, ListenSocket};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end;

listen_on(Callback, IpAddrStr, Port) ->
    case inet_parse:address(IpAddrStr) of
	{ok, IpAddr} ->
	    listen_on(Callback, IpAddr, Port);
	Err ->
	    Err
    end.
