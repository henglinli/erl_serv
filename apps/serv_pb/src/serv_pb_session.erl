%%%-------------------------------------------------------------------
%%% @author  Henry Lee <henglinli@gmail.com>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2014 by Henry Lee <henglinli@gmail.com>
%%%-------------------------------------------------------------------
-module(serv_pb_session).

-author('HenryLee<henglinli@gmail.com>').

-include("serv_pb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([ets/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ets_tab :: ets:tab()}).
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
-spec start_link() -> Result when
      Result :: {ok,Pid} | ignore | {error,Error},
      Pid :: pid(),
      Error :: {already_started,Pid} | term().
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
%% session map
-spec init(Args) -> Result when
      Args :: term(),      
      Result :: {ok,State} | {ok,State,Timeout} | {ok,State,hibernate} | {stop,Reason} | ignore,
      State :: term(),
      Timeout :: pos_integer() | infinity,
      Reason :: term().
init([]) ->
   case ets:new(?ETS_SERV_SESSION_NAME, ?ETS_SERV_SESSION_OPTS) of
       ?ETS_SERV_SESSION_NAME ->
	   {ok,  #state{ets_tab = ?ETS_SERV_SESSION_NAME}};
       _Other ->
	   {stop, "ets:new/2 error"}
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
handle_call(ets, _From, State = #state{ets_tab = EtsTab}) ->
    case ets:info(EtsTab) of
	undefined ->
	    {reply, undefined, State};
	InfoList when is_list(InfoList) ->
	    {reply, EtsTab, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = unknown_request,
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
-spec ets() -> undefined | ets:tab().
ets() ->
    gen_server:call(?SERVER, ets).
