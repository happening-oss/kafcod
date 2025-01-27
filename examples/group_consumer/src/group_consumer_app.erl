-module(group_consumer_app).
-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    % Note the unconventional name here.
    group_consumer_app_sup:start_link().

stop(_State) ->
    ok.
