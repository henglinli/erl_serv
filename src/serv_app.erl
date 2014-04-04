%%
-module(serv_app).

-author('HenryLee<henglinli@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    serv_sup:start_link().

stop(_State) ->
    ok.
