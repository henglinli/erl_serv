%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_worker_sender).

-behaviour(serv_worker).

-include("serv.hrl").
%% serv_worker callback
-export([init_worker/1, handle_work/2, reply/2]).

%% API
-export([]).

%% servers tail was last, head was oldest
-record(state, {queue :: queue:queue(binary()),
		set :: gb_sets:set(binary())}).
%%%===================================================================
%%% API
%%%===================================================================
%% serv_worker callback
%% init worker
-spec init_worker(WorkerArgs :: term()) ->
    {ok, NewWorkState :: term()} | {error, Reason :: term()}.
init_worker([]) ->
    {ok, #state{queue=undefined, set=undefined}}.

%% handle worker
-spec handle_work(Work :: term(), WorkState ::term()) ->
			 {reply,
			  ToWho :: term() | {pid, Pid :: pid()},
			  Reply :: term(),
			  NewWorkState :: term()} |
			 {noreply, NewWorkState :: term()}.
%% select server
handle_work({select, From, User, _N},
	    #state{queue=undefined, set=undefined} = WorkState) ->
    case get_apl(?MESSAGE, User, 1) of
	[] ->
	    {reply, From, {server, {1, <<"serv down">>}}, WorkState};
	PrefList ->
	    Servers = send_select(PrefList, []),
	    [{server, Server} | _RestServers] = Servers,
	    {reply, From, {server, {0, Server}}, WorkState}
    end;

%% forward message
handle_work({forward,
	     #message{id=Id, from=From, to=ToWho, msg=_Msg} = Message,
	     N}, WorkState) ->
    case get_apl(?MESSAGE, ToWho, N) of
	[] ->
	    {reply, From, {reply, {Id, 1, <<"serv down">>}}, WorkState};
	PrefList ->
	    Reply = forward_reply(Id, PrefList, Message),
	    {reply, From, {reply, Reply}, WorkState}
    end;

handle_work(Work, WorkState) ->
    lager:warn("unkonw work ~p", [Work]),
    {noreply, WorkState}.

%% reply
-spec reply(WorkFrom :: term() | {pid, Pid :: pid()}, Reply :: term()) ->
		   ok | {error, Reason :: term()}.
%% this function is test only,
%% and never used in producntion .
reply(WorkFrom, {unkown_work, Work}) ->
    WorkFrom ! {unkown_work , Work};

%% select
reply(WorkFrom, {server, _Server} = Message) ->
    serv_pb_server:sync_send({pid, WorkFrom}, Message);
%% message will reply to serv_pb_server
reply(WorkFrom, {reply, _Reply} = Message) ->
    serv_pb_server:sync_send({pid, WorkFrom}, Message);

reply(_WorkFrom, _Reply) ->
    lager:warn("not impl", []),
    {error, <<"not impl">>}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_apl(binary(), binary(), integer()) -> node().
get_apl(Bucket, Key, N)
  when erlang:is_binary(Bucket) andalso erlang:is_binary(Key) ->
    DocIdx = riak_core_util:chash_key({Bucket, Key}),
    riak_core_apl:get_apl(DocIdx, N, ?SERV).

%% get_oldest(E, #state{queue=Q, set=S} = WorkState) ->
%%     case gb_sets:is_member(E, S) of
%%	false -> % new element, insert and return it
%%          NewS = gb_sets:insert(E, S),
%%          NewQ = queue:in(E, Q),
%%          {ok, E, #state{queue=NewQ, set=NewS}};
%%	true -> % old element, check front
%%          case queue:get(Q) of
%%		E ->
%%                  queue:
%%                  {ok, E,
%%     end.

%% send select, and get server list
send_select(IndexNode, [], Servers) ->
    {ok, Server} = riak_core_vnode_master:sync_command(IndexNode, select,
						       ?SERV, ?TIMEOUT),
    lists:append(Servers, [{server, Server}]);

send_select(IndexNode, PrefList, []) ->
    {ok, Server} = riak_core_vnode_master:sync_command(IndexNode, select,
						       ?SERV, ?TIMEOUT),
    send_select(PrefList, [{server, Server}]);

send_select(IndexNode, PrefList, Servers) ->
    {ok, Server} = riak_core_vnode_master:sync_command(IndexNode, select,
						       ?SERV, ?TIMEOUT),
    send_select(PrefList, lists:append(Servers, [{server, Server}])).

send_select(PrefList, Servers) ->
    [IndexNode | RestPrefList] = PrefList,
    send_select(IndexNode, RestPrefList, Servers).

%% forward
-spec forward(term(), term(), not_found | forward) ->
		     forward | error | not_found.
forward([], _Message, _Result) ->
    not_found;

forward(_PrefList, _Message, forward) ->
    forward;

forward(PrefList, Message, not_found) ->
    %% send messge to one index node
    [IndexNode| RestPrefList] = PrefList,
    Result = riak_core_vnode_master:sync_command(IndexNode, {forward, Message},
						 ?SERV, ?TIMEOUT),
    forward(RestPrefList, Message, Result).

%% forward_reply
forward_reply(Id, PrefList, Message) ->
    case forward(PrefList, Message, not_found) of
	forward ->
	    {Id, 0, <<"message was forwarded">>};
	error ->
	    {Id, 2, <<"serv internal error">>};
	not_found ->
	    {Id, 3, <<"message was not handle">>}
    end.
