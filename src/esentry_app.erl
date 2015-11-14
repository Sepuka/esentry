-module(esentry_app).

-behaviour(application).

%% Application callbacks
-export([start/0]).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(esentry).

start(_StartType, _StartArgs) ->
    esentry_sup:start_link().

stop(_State) ->
    ok.
