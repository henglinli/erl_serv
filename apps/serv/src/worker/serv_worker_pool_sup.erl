%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_worker_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    RestartStrategy = one_for_all,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    %% serv_worker_sup
    %% ServWorkerSupSpec = {serv_worker_sup,
    %%			 {serv_worker_sup, start_link, []},
    %%			 Restart, Shutdown, supervisor, [serv_worker_sup]},

    %% serv_worker_pool
    ServWorkerPooolSpec = {serv_worker_pool,
			   {serv_worker_pool, start_link, []},
			   Restart, Shutdown, Type, [serv_worker_pool]},

    Args = [{worker_callback_mod, serv_send_worker},
	    {worker_args, []}],

    ServWorkerSpecs = serv_worker_spec(8, Args, []),

    {ok, {SupFlags, [ServWorkerPooolSpec] ++ ServWorkerSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
serv_worker_name(N) ->
    NBinary = erlang:integer_to_binary(N),
    Name = <<"serv_worker_", NBinary/binary>>,
    erlang:binary_to_atom(Name, utf8).

serv_worker_spec(0, _Args, Specs) ->
    Specs;

serv_worker_spec(N, Args, []) ->
    Name = serv_worker_name(N),
    Spec = {Name,
	    {serv_worker, start_link, [Name, Args, last]},
	    permanent, 2000, worker, [serv_worker]},
    serv_worker_spec(N - 1, Args, [Spec]);

serv_worker_spec(N, Args, Specs) ->
    Name = serv_worker_name(N),
    Spec = {Name,
	    {serv_worker, start_link, [Name, Args]},
	    permanent, 2000, worker, [serv_worker]},
    serv_worker_spec(N - 1, Args, [Spec] ++ Specs).