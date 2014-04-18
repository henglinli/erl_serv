%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_sup).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    ServSessionMapSpec = {serv_session_map,
			  {serv_session_map, start_link, []},
			  Restart, Shutdown, worker,
			  [serv_session_map]},
    ServChatSpec = {serv_chat,
		    {serv_chat, start_link, []},
		    Restart, Shutdown, worker,
		    [serv_chat]},

    RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
		    Restart, Shutdown, Type, [ranch_sup]},
    ListenerSpec = ranch:child_spec(serv, 5,
				    ranch_tcp, [{port, 9999}, {packet, 2}],
				    serv_session,
				    []),
    {ok, {SupFlags, [ServSessionMapSpec,
		     ServChatSpec,
		     RanchSupSpec,
		     ListenerSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
