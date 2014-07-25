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
-export([init_worker/1, handle_work/3, reply/2]).

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
-spec handle_work(Work :: term(), WorkFrom :: term(), WorkState ::term()) ->
    {reply, Reply :: term(), NewWorkState :: term()} |
    {noreply, NewWorkState :: term()}.

%% select server
handle_work({select, User, _N}, _WorkFrom,
            #state{queue=undefined, set=undefined} = WorkState) ->
    case get_apl(?MESSAGE, User, 1) of
        [] ->
            {reply, {server, {1, <<"serv down">>}}, WorkState};
        PrefList ->
            Servers = send_select(PrefList, []),
            [{server, Server} | _RestServers] = Servers,
            {reply, {server, {0, Server}}, WorkState}
    end;

%% forward message
handle_work({forward,
	     #{id := Id, to := ToWho, msg := _Msg} = Message, N},
	    WorkFrom, WorkState) ->
    lager:info("woker ~p form ~p", [Message, WorkFrom]),
    case get_apl(?MESSAGE, ToWho, N) of
        [] ->
            {reply, {reply, {Id, 1, <<"serv down">>}}, WorkState};
        PrefList ->
            Reply = forward_reply(Id, PrefList, {WorkFrom, Message}),
            {reply, {reply, Reply}, WorkState}
    end;

handle_work(Work, WorkFrom, WorkState) ->
    lager:info("undefined work ~p from ~p", [Work, WorkFrom]),
    {reply, {unkown_work, Work}, WorkState}.

%% reply
-spec reply(WorkFrom :: term() | {pid, Pid :: pid()}, Reply :: term()) ->
                   ok | {error, Reason :: term()}.
%% this function is test only,
%% and never used in producntion .
reply(WorkFrom, {unkown_work, Work}) ->
    WorkFrom ! {unkown_work , Work};

%% select
reply({pid, _Pid} = WorkFrom, {server, _Server} = Message) ->
    serv_pb_server:sync_send(WorkFrom, Message);

%% message will reply to serv_pb_server
reply({pid, _Pid} = WorkFrom, {reply, _Reply} = Message) ->
    serv_pb_server:sync_send(WorkFrom, Message);

reply(_WorkFrom, _Reply) ->
    lager:info("not impl", []),
    {error, not_impl}.
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

%% do_forward
do_forward(IndexNode, RestPrefList, Message) ->
    case riak_core_vnode_master:sync_command(IndexNode, {forward, Message},
					     ?SERV, ?TIMEOUT) of
	forward -> % message was forward
	    forward;
	not_found -> % not found target
	    forward(RestPrefList, Message)
    end.

%% forward
-spec forward(term(), term()) -> forward | error | not_found.
forward([], _Message) ->
    not_found;

%% forward
forward(PrefList, Message) ->
    %% send messge to one index node
    [IndexNode| RestPrefList] = PrefList,
    do_forward(IndexNode, RestPrefList, Message).

%% forward_reply
forward_reply(Id, PrefList, Message) ->
    case forward(PrefList, Message) of
        forward ->
            {Id, 0, <<"message was forwarded">>};
        error ->
            {Id, 2, <<"serv internal error">>};
        not_found ->
            {Id, 3, <<"message was not handle">>}
    end.
