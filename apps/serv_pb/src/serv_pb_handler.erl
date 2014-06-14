%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 12 Jun 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler).

-author('HenryLee<henglinli@gmail.com>').

-include("serv_pb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([handler_map_ets/0, register_handler/2, lookup_handler/1,
	 handlers/0
	]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-callback handle_request(Request :: any(), Session :: #session{}) ->
    {Response :: binary() | noreply, NewSession :: #session{} | nochange}.

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
init([]) ->
    {ok, #state{}}.

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

-spec handler_map_ets() -> undefined | ets:tab().
handler_map_ets() ->
    case ets:info(?ETS_SERV_HANDLER_MAP_NAME) of
    	undefined ->
    	    undefined;
    	List when is_list(List) ->
    	    ?ETS_SERV_HANDLER_MAP_NAME
    end.

-spec register_handler(pos_integer(), module()) -> true.
register_handler(MsgCode, Service) ->
    ets:insert(?ETS_SERV_HANDLER_MAP_NAME, {MsgCode, Service}).

-spec lookup_handler(pos_integer()) -> undefined | module().
lookup_handler(MsgCode) ->
    case ets:lookup(?ETS_SESSION_MAP_NAME, MsgCode) of
	[] ->
	    undefined;
	[Tuple] ->
	    {MsgCode, Service} = Tuple,
	    Service
    end.

-spec handlers() -> [ module() ].
handlers() ->
    ets:tab2list(?ETS_SERV_HANDLER_MAP_NAME).
