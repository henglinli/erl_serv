%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 26 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_fsm_put_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_fsm_put/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 5000,
    Type = worker,

    FsmPut = {serv_fsm_put,
	      {serv_fsm_put, start_link, []},
	      Restart, Shutdown, Type, [serv_fsm_put]},

    {ok, {SupFlags, [FsmPut]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_fsm_put(Args) ->
    supervisor:start_child(?MODULE, Args).
