-module(serv_vnode).
-behaviour(riak_core_vnode).
-include("serv.hrl").

-export([start_vnode/1,
	 init/1,
	 terminate/2,
	 handle_command/3,
	 is_empty/1,
	 delete/1,
	 handle_handoff_command/3,
	 handoff_starting/2,
	 handoff_cancelled/1,
	 handoff_finished/2,
	 handle_handoff_data/2,
	 encode_handoff_item/2,
	 handle_coverage/4,
	 handle_exit/3]).

-ignore_xref([start_vnode/1]).

-record(state, {server :: binary(),
		partition :: term(),
		stid :: ets:tid()
	       }).

-define(ETS_SESSION_OPTS, [ordered_set]).

%%-define(USE_POOL, true).

%% API
%% callback
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

-ifdef(USE_POOL).
init([Partition]) ->
    WorkerPoolSize = app_helper:get_env(serv, worker_pool_size, 8),
    WorkerPool = {pool, serv_vnode_worker, WorkerPoolSize, []},
    case app_helper:get_env(serv, session_type, riak_core) of
	riak_core ->
	    Stid = ets:new(undefined, ?ETS_SESSION_OPTS),
	    {ok, #state{server=get_host(),
			partition=Partition,
			stid=Stid},
	     [WorkerPool]};
	_Else ->
	    {ok, #state{server=get_host(),
			partition=Partition,
			stid=undefined},
	     [WorkerPool]}
    end.
-else.
init([Partition]) ->
    case app_helper:get_env(serv, session_type, riak_core) of
	riak_core ->
	    Stid = ets:new(undefined, ?ETS_SESSION_OPTS),
	    {ok, #state{server=get_host(),
			partition=Partition,
			stid=Stid}};
	_Else ->
	     {ok, #state{server=get_host(),
			 partition=Partition,
			 stid=undefined}}
    end.
-endif.
%% proxy of forward message
-ifdef(USE_POOL).
forward(Message, Sender, State) ->
    {async, {forward, Message}, Sender, State}.
-else.
forward(#message{id=Id, from=From, to=ToWho, msg=Msg},
	_Sender,
	#state{stid=Stid} = State) ->
    case ets:match_object(Stid, {'_', ToWho}) of
	[] ->
	    serv_pb_server:send({pid, From}, {reply, Id, not_found}),
	    {noreply, State};
	[{Pid, _User} | _Rest] ->
	    case Pid of
		From ->
		    serv_pb_server:send({pid, From}, {reply, Id, error}),
		    {noreply, State};
		_Else ->
		    %% Msg is binary of #chat{}
		    %% forward
		    serv_pb_server:send({pid, Pid}, {chat, Msg}),
		    %% reply
		    serv_pb_server:send({pid, From}, {reply, Id, forward}),
		    {noreply, State}
	    end
    end.
-endif.

%% select server
handle_command({select, From}, _Sender, #state{server=Server}=State) ->
    serv_pb_server:send({pid, From}, {server, Server}),
    {noreply, State};
%% register server process
handle_command({register,
		#session{pid=Pid, user=User}},
	       _Sender, #state{stid=Stid} = State) ->
    true = ets:insert(Stid, {Pid, User}),
    %% send serv_pb_server ok
    serv_pb_server:send({pid, Pid}, ok),
    {noreply, State};

handle_command({deregister, Pid}, _Sender,
	       #state{stid=Stid} = State) ->
    true = ets:delete(Stid, Pid),
    serv_pb_server:send({pid, Pid}, ok),
    {noreply, State};

%% forward message
handle_command({forward, Message}, Sender, State) ->
    forward(Message, Sender, State);

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, pong, State};

handle_command(Message, _Sender, State) ->
    lager:notice("Unknown message: ~P", [Message]),
    {reply, {error, <<"not impl">>}, State}.

handle_handoff_command(Message, Sender, State) ->
    lager:notice("handle_handoff_command(~p, ~p, State)", [Message, Sender]),
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    %%lager:info("handoff_starting(~p, State)", [TargetNode]),
    {true, State}.

handoff_cancelled(State) ->
    lager:info("handoff_cancelled(~p)", [State]),
    {ok, State}.

handoff_finished(TargetNode, State) ->
    lager:info("handoff_finished(~p, ~p)", [TargetNode, State]),
    {ok, State}.

handle_handoff_data(Data, State) ->
    lager:info("handle_handoff_data(~p, ~p)", [Data, State]),
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, {error, <<"not impl">>}, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_host() -> binary().
get_host() ->
    NodeString = erlang:atom_to_list(erlang:node()),
    [_Char | Server] = lists:dropwhile(fun(Char) ->
					       Char /= $@
				       end,
				       NodeString),
    erlang:list_to_binary(Server).
