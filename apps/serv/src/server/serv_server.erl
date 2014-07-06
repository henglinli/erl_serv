%%%-------------------------------------------------------------------
%%% @author HenryLee <lee@OSX.local>
%%% @copyright (C) 2014, HenryLee
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2014 by HenryLee <lee@OSX.local>
%%%-------------------------------------------------------------------
-module(serv_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([get/0]).

-define(SERVER, ?MODULE).

-record(state, {last :: node(), size :: integer()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec get() -> string().
get() ->
    Node = gen_server:call(?SERVER, get),
    node_to_host(Node).
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
    {ok, #state{last = 0, size = 0}}.

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
handle_call(get, _From, State = #state{last = Last, size = Size}) ->
    {ok, Ring}=riak_core_ring_manager:get_my_ring(),
    Status = riak_core_ring:all_member_status(Ring),
    ValidNodes = list:map(fun(Member) ->
				  case Member of
				      {Node, valid} ->
					  Node;
				      _NotValid ->
					  []
				  end
			  end,
			  Status),
    case erlang:length(ValidNodes) of
	0 ->
	    {reply, serv_donw, State};
	NewSize ->
	    case NewSize of
		Size ->
		    case Last of
			0 -> % no last use head
			    [First | _Rest] = ValidNodes,
			    {reply, First, State#state{last = 1}};
			Size -> % last is tail, use head
			    [First | _Rest] = ValidNodes,
			    {reply, First, State#state{last = 1}};
			_Else -> % last is not tail, use next
			    Node = list:nth(Last + 1, ValidNodes),
			    {reply, Node, State#state{last = Last + 1}}
		    end;
		_Else -> % valid nodes changes then use head
		    [First | _Rest] = ValidNodes,
		    {reply, First, State#state{last = 1, size = NewSize}}
	    end
    end;

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
-spec node_to_host(node()) -> string().
node_to_host(Node)
  when erlang:is_atom(Node) ->
    NodeString = erlang:atom_to_list(Node),
    lists:dropwhile(fun(Char) ->
			    case Char of
				'@' ->
				    true;
				_Else ->
				    fasle
			    end
		    end,
		    NodeString).
