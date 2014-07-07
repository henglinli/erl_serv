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

-define(POOL_NAME, ?MODULE).

-export([checkout/0, checkin/1]).
-export([pool_spec/0]).

%%%===================================================================
%%% API functions
%%%===================================================================
%% get a worker
-spec checkout() -> pid() | full.
checkout() ->
    poolboy:checkout(?POOL_NAME, true).
%% put back a worker
-spec checkin(pid()) -> ok.
checkin(Worker) ->
    poolboy:checkin(?POOL_NAME, Worker).
%%
-spec pool_spec() -> supervisor:child_spec().
pool_spec() ->
    PoolName = ?POOL_NAME,
    PoolArgs = [{name, {local, PoolName}},
		{worker_module, serv_fsm},
		{size, 8},
		{max_overflow, 16}],
    WorkArgs = [],
    poolboy:child_spec(PoolName, PoolArgs, WorkArgs).
