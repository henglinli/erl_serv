%%%-------------------------------------------------------------------
%%% @author HenryLee <lee@OSX.local>
%%% @copyright (C) 2014, HenryLee
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2014 by HenryLee <lee@OSX.local>
%%%-------------------------------------------------------------------
-module(serv_fsm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_fsm/0, stop_fsm/1]).
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
% start fsm
-spec start_fsm() -> supervisor:startchild_ret().
start_fsm() ->
    supervisor:start_child(?SERVER, []).

% stop fsm
-spec stop_fsm(Id :: pid()) -> Result :: ok | {error, not_found | simple_one_for_one}.
stop_fsm(Id) ->
    supervisor:terminate_child(?SERVER, Id).

% start link    
-spec start_link() -> supervisor:startlink_ret().
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
-spec init(Args) -> Result when
      Args :: term(),
      Result :: {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore,
      RestartStrategy :: supervisor:strategy(),
      MaxR :: pos_integer(),
      MaxT :: pos_integer(),
      ChildSpec :: supervisor:child_spec().
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,

    ServFsmSpec = {serv_fsm, {serv_fsm, start_link, []},
		   Restart, Shutdown, Type, [serv_fsm]},

    {ok, {SupFlags, [ServFsmSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
