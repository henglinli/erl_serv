%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_fsm).

-include("serv.hrl").

-behaviour(gen_fsm).

-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([wait_msg/2,
	 wait_msg/3]).

-export([sync_send/4]).
-define(SERVER, ?MODULE).

-record(state, {preflist :: riak_core_apl:preflist(),
		caller :: term(),
		message :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec sync_send(Pid:: pid(),
		ToWho :: binary(),
		Message :: binary(),
		N :: integer()) -> forward | not_found.
sync_send(Pid, ToWho, Message, N) ->
    gen_fsm:sync_send_event(Pid, {forward, ToWho, Message, N}).
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link([]) ->
    gen_fsm:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, wait_msg, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
wait_msg(Event, State) ->
    lager:notice("unknown event ~p", [Event]),
    {next_state, wait_msg, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
% do_forward
do_forward(IndexNode, Rest, Message, State) ->
    try
	case riak_core_vnode_master:sync_command(IndexNode, {forward, Message},
						 ?SERV, ?TIMEOUT) of
	    forward -> % message was forward
		{reply, forward, wait_msg, State};
	    not_found -> % not found target
		forward(Rest, Message, State)
	end
    catch
	{_Reason, _Where} ->
	    {reply, {error, sync_command_error}, wait_msg, State}
    end.
% forward
forward([], _Message, State) ->
    {reply, not_found, wait_msg, State};
% forward
forward(PrefList, Message, State) ->
    %% send messge to one index node
    [IndexNode| Rest] = PrefList,
    do_forward(IndexNode, Rest, Message, State).
%%

wait_msg({forward, ToWho, Message, N}, _From, State) ->
    case get_apl(?MESSAGE, ToWho, N) of
	[] ->
	    {reply, {error, serv_down}, wait_msg, State};
	PrefList ->
	    forward(PrefList, Message, State)
    end;

wait_msg({save, ToWho, Message}, From, State) ->
    lager:info("message: ~p from: ~p to: ~P  saved", [From, Message, ToWho]),
    {reply, saved, wait_msg, State};

wait_msg(Event, From, State) ->
    lager:notice("unknown event: ~p from: ~p", [Event, From]),
    Reply = not_impl,
    {reply, Reply, wait_msg, State}.

%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(Event, StateName, State) ->
    lager:notice("unknown event: ~p", [Event]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------

handle_sync_event(Event, From, StateName, State) ->
    lager:notice("unknown event: ~p from: ~p", [Event, From]),
    Reply = not_impl,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    lager:notice("unknown info: ~p", [Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_apl(binary(), binary(), integer()) -> node().
get_apl(Bucket, Key, N)
  when erlang:is_binary(Bucket) andalso erlang:is_binary(Key) ->
    DocIdx = riak_core_util:chash_key({Bucket, Key}),
    riak_core_apl:get_apl(DocIdx, N, ?SERV).
