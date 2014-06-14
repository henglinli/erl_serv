-module(serv_pb_app).

-behaviour(application).

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
    riak_core_util:start_app_deps(serv_pb),
    case serv_pb_sup:start_link() of
	ignore ->
	    {error, ignore};
	{ok, Pid} ->
	    {ok, Pid};
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
    
