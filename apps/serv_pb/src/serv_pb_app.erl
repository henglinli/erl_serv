-module(serv_pb_app).

-behaviour(application).

-define(DEFAULT_LIMIT, 100000).

%% Application callbacks
-export([start/2, stop/1]).
-export([register_stat/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================
% @doc The application:start callback.
-spec start(Type, StartArgs)
	   -> {ok, Pid} | {ok, Pid, State} | {error, Reason} when
      Type :: normal
	    | {takeover, Node :: node()}
	    | {failover, Node :: node()},
      Pid :: pid(),
      State :: term(),
      StartArgs :: term(),
      Reason :: term().
start(_StartType, _StartArgs) ->
    %% start deps
    riak_core_util:start_app_deps(serv_pb),
    %% start sidejob supervisor
    case app_helper:get_env(serv_pb, client_limit, ?DEFAULT_LIMIT) of
	undfined ->
	    ok;
	Limit ->
	    sidejob:new_resource(serv_pb_server_sj, sidejob_supervisor, Limit)
    end,

%%     case app_helper:get_env(serv_pb, direct_stats, false) of
%%         true ->
%%             ok;
%%         false ->
%%             sidejob:new_resource(serv_pb_stat_sj, serv_pv_stat_worker, 10000)
%%     end,

    %% start serv_pb_sup
    case serv_pb_sup:start_link() of
	ignore ->
	    {error, ignore};
	{ok, Pid} ->
	    case riak_core:register(serv_pb, [{stat_mod, serv_pb_stat}]) of
		ok ->
		    {ok, Pid};
		_Other ->
		    {error, "riack_core:register serv_pb error"}
	    end;
	{error, Reason} ->
	    {error, Reason};
	_Undefined ->
	    {error, unknown}
    end.

%% @doc The application:stop callback.
-spec stop(State::term()) -> ok.
stop(_State) ->
    ok.

-spec register_stat() -> ok | {error, Reason::term()}.
register_stat() ->
     case riak_core:register(serv_pb, [{stat_mod, serv_pb_stat}]) of
	 ok ->
	     ok;
	 _Other ->
	     {error, "riack_core:register serv_pb error"}
     end.
