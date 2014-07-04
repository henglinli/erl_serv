%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_fsm_pool).
%% API

-export([checkout/0, checkin/1]).
-export([pool_spec/0]).

%%%===================================================================
%%% API functions
%%%===================================================================
%% get a worker
-spec checkout() -> pid() | full.
checkout() ->
    poolboy:checkout(serv_fsm_pool, true).
%% put back a worker
-spec checkin(pid()) -> ok.
checkin(Worker) ->
    poolboy:checkin(serv_fsm_pool, Worker).
%%
-spec pool_spec() -> supervisor:child_spec().
pool_spec() ->
    PoolName = serv_fsm_pool,
    PoolArgs = [{name, {local, PoolName}},
		{worker_module, serv_fsm},
		{size, 8},
		{max_overflow, 16}],
    WorkArgs = [],
    poolboy:child_spec(PoolName, PoolArgs, WorkArgs).
