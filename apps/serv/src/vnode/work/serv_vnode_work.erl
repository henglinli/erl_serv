%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_vnode_work).
-behaviour(riak_core_vnode_worker).

-export([init_worker/3,
	 handle_work/3]).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-record(state, {index :: partition()}).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Initialize the worker. Currently only the VNode index
%% parameter is used.
init_worker(VNodeIndex, _Args, _Props) ->
    {ok, #state{index=VNodeIndex}}.

%% @doc
handle_work(ping, _Sender, State) ->
    lager:info("vnode index: ~p", [State#state.index]),
    {reply, pong, State};

%% forward message
handle_work({forward, {Id, ToWho, Message}}, _Sender, State) ->
    lager:info("forward mesasge {~p, ~p, ~p}", [Id, ToWho, Message]),
    {reply, forword, State};

%% unknown
handle_work(Work, Sender, State) ->
    lager:notice("unknonw work: ~p from ~p", [Work, Sender]),
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
