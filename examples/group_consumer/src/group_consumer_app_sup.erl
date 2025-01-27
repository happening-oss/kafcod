-module(group_consumer_app_sup).
-export([start_link/0]).
-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init({}) ->
    SupFlags = #{},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
