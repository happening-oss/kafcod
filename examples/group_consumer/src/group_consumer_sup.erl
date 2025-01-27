-module(group_consumer_sup).
-export([start_link/3]).
-behaviour(supervisor).
-export([init/1]).

%%% 'group_consumer_sup' is the supervisor for a group consumer.
%%% We might have more than one of these in the application, each with its own group ID and list of topics.

start_link({BootstrapHost, BootstrapPort}, ClientId, {GroupId, Topics}) ->
    % Because we want to pass the 'consumer_sup' pid to 'group_membership', we need to start them explicitly.
    % TODO: This doesn't work unless _we_ die, because the child specs are static. So even if 'membership' restarts,
    % it gets given the wrong CSup pid, and it all goes wrong.
    % TODO: Use a 'significant' child, so that we get killed? Alternatively, use a side-car-supervisor (term I just invented)
    % that does the magic.
    {ok, Sup} = supervisor:start_link(?MODULE, {}),
    {ok, CSup} = supervisor:start_child(Sup, #{
        id => consumers, start => {consumer_sup, start_link, []}, type => supervisor
    }),
    {ok, _} = supervisor:start_child(Sup, #{
        id => membership,
        start =>
            {group_membership, start_link, [
                CSup, {BootstrapHost, BootstrapPort}, ClientId, {GroupId, Topics}
            ]}
    }),
    {ok, Sup}.

init({}) ->
    SupFlags = #{
        % If any child dies, they should all be restarted.
        strategy => one_for_all
    },
    {ok, {SupFlags, []}}.
