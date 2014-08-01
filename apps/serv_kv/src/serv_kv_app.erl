-module(serv_kv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([prep_stop/1, check_kv_health/1]).

-include_lib("riak_kv/include/riak_kv_types.hrl").

-define(MAX_FLUSH_PUT_FSM_RETRIES, 10).

-define(DEFAULT_FSM_LIMIT, 10000).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    serv_kv_sup:start_link().

stop(_State) ->
    ok.
