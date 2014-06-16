%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_socket/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts a PB socket server.
-spec start_socket() -> {ok, pid()} | {error, term()}.
start_socket() ->
    supervisor:start_child(?SERVER, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
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

    Server = {serv_pb_server, {serv_pb_server, start_link, []},
	      Restart, Shutdown, Type, [serv_pb_server]},

    {ok, {SupFlags, [Server]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
