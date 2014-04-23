%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_sup).

-behaviour(supervisor).

%% Supervisor callbacks
-export([start_link/1, init/1, stop/1]).
-export([start_socket/0]).

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
-spec start_link(Handlers :: [term()]) -> supervisor:startlink_ret().
start_link(Handlers) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Handlers]).

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
% @doc The init/1 supervisor callback, initializes the supervisor.
-spec init(list()) -> {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore when
      RestartStrategy :: supervisor:strategy(),
      MaxR :: pos_integer(),
      MaxT :: pos_integer(),
      ChildSpec :: supervisor:child_spec().

init(Args) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 4000,
    MaxSecondsBetweenRestarts = 8000,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,

    AChild = {serv_pb_session, {serv_pb_session, start_link, [Args]},
	      Restart, Shutdown, Type, [serv_pb_session]},

    {ok, {SupFlags, [AChild]}}.

%% @doc Stops the PB server supervisor.
-spec stop(term()) -> ok.
stop(_Sate) -> ok.

%% @doc Starts a PB socket server.
-spec start_socket() -> {ok, pid()} | {error, term()}.
start_socket() ->
    supervisor:start_child(?SERVER, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================
