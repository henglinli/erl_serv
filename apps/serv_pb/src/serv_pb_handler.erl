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

-export([ets/0, register/2, lookup/1, deregister/1,
	 handlers/0
	]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).

-record(state, {ets_tab :: ets:tab()}).

%% handle message
-callback handle(Request :: term(), Session :: term()) ->
    {error, Reason :: term()} |
    {noreply, nochange} |
    {noreply, Session :: term()} |
    {Response :: binary(), nochange} |
    {Response :: binary(), NewSession :: term()}.

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
-spec init(Args) -> Result when
      Args :: term(),
      Result :: {ok,State} | {ok,State,Timeout} | {ok,State,hibernate} | {stop,Reason} | ignore,
      State :: term(),
      Timeout :: pos_integer() | infinity,
      Reason :: term().
init([]) ->
    case ets:new(?ETS_SERV_HANDLER_NAME, ?ETS_SERV_HANDLER_OPTS) of
	?ETS_SERV_HANDLER_NAME ->
	    {ok, #state{ets_tab = ?ETS_SERV_HANDLER_NAME}, hibernate};
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
%% get
handle_call(ets, _From, State = #state{ets_tab = EtsTab}) ->
    case ets:info(EtsTab) of
	undefined ->
	    {reply, undefined, State};
	InfoList when is_list(InfoList) ->
	    {reply, EtsTab, State}
    end;
%% all
handle_call(handlers, _From, State = #state{ets_tab = EtsTab}) ->
    case ets:info(EtsTab) of
	undefined ->
	    {reply, undefined, State};
	InfoList when is_list(InfoList) ->
	    Result = ets:tab2list(EtsTab),
	    {reply, Result, State}
    end;
%% insert
handle_call({MsgCode, Handler}, _From, State = #state{ets_tab = EtsTab})
  when is_integer(MsgCode) ->
    case ets:info(EtsTab) of
	undefined ->
	    {reply, undefined, State};
	InfoList when is_list(InfoList) ->
	    Result = ets:insert(EtsTab, {MsgCode, Handler}),
	    {reply, Result, State}
    end;
%% lookup
handle_call({deregister, MsgCode}, _From, State = #state{ets_tab = EtsTab})
  when is_integer(MsgCode) ->
    case ets:info(EtsTab) of
	undefined ->
	    {reply, undefined, State};
	InfoList when is_list(InfoList) ->
	    Result = ets:delete(EtsTab, MsgCode),
	    {reply, Result, State}
    end;
%% lookup
handle_call({lookup, MsgCode}, _From, State = #state{ets_tab = EtsTab})
  when is_integer(MsgCode) ->
    case ets:info(EtsTab) of
	undefined ->
	    {reply, undefined, State};
	InfoList when is_list(InfoList) ->
	    case ets:lookup(EtsTab, MsgCode) of
		[] ->
		    {reply, undefined, State};
		[{MsgCode, Handler}] ->
		    {reply, Handler, State}
	    end
    end;
%%
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

-spec ets() -> atom().
ets() ->
    ?ETS_SERV_HANDLER_NAME.

-spec handlers() -> [tuple()].
handlers() ->
    ets:tab2list(?ETS_SERV_HANDLER_NAME).

-spec register(pos_integer(), module()) -> undefined | true.
register(MsgCode, Handler)
  when is_integer(MsgCode), is_atom(Handler) ->
	  ets:insert(?ETS_SERV_HANDLER_NAME, {MsgCode, Handler}).

-spec deregister(pos_integer()) -> undefined | true.
deregister(MsgCode)
  when is_integer(MsgCode) ->
    ets:delete(?ETS_SERV_HANDLER_NAME, MsgCode).

-spec lookup(pos_integer()) -> undefined | module().
lookup(MsgCode)
  when is_integer(MsgCode) ->
    [{MsgCode, Handler} | _Rest] = ets:lookup(?ETS_SERV_HANDLER_NAME, MsgCode),
    Handler.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

serv_pb_handler_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
       fun ets_test_case/0,
       fun handlers_test_case/0,
       fun register_test_case/0,
       fun deregister_test_case/0,
       fun lookup_test_case/0
      ]
    }.

steup() ->
    start_link().

ets_test_case() ->
    ?assertEqual(?ETS_SERV_HANDLER_NAME, ets()).

handlers_test_case() ->
    ?assert(is_list(handlers()).

register_test_case() ->
    ?assertEqual(true, register(1, fake_ping)),
    ?assertEqual(true, register(2, fake_info)),

deregister_test_case() ->
    ?assertEqual(true, deregister(2)).

lookup_test_case() ->
    ?assertEqual({1, fake_ping}, lookup(1)).

cleanup(_Ctx) ->
    ok.

-endif.
