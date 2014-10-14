%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_mc_sup).

-behaviour(supervisor).
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(MC_LISTENER(IP, Port), {serv_mc_listener,
				{serv_mc_listener, start_link, [IP, Port]},
				permanent, 5000, worker, [serv_mc_listener]}).
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
% @doc The init/1 supervisor callback, initializes the supervisor.
-spec init(Args) -> Result when
      Args :: term(),
      Result :: {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore,
      RestartStrategy :: supervisor:strategy(),
      MaxR :: pos_integer(),
      MaxT :: pos_integer(),
      ChildSpec :: supervisor:child_spec().

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Listener = mc_listener_specs(serv_mc_listener:get_listeners()),
    
    case app_helper:get_env(serv, session_type, riak_core) of
	riak_core ->
	    {ok, {SupFlags, [Listener]}};
	_Else ->
	    Session = ?CHILD(serv_pb_session, worker),
	    {ok, {SupFlags, [Session, Listener]}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

mc_listener_specs([{Ip, Port}]) ->
    ?MC_LISTENER(Ip, Port).
