-module(group_consumer).
-export([
    start_consumer/4,
    stop_consumer/1
]).

% TODO: Introduce group_consumer_sup_sup?
-define(SUPERVISOR, group_consumer_app_sup).

-spec start_consumer({inet:hostname(), inet:port_number()}, binary(), binary(), [binary()]) ->
    {ok, pid()}.
start_consumer(BootstrapBroker = {_, _}, ClientId, GroupId, Topics) ->
    ChildSpec = child_spec(BootstrapBroker, ClientId, GroupId, Topics),
    supervisor:start_child(?SUPERVISOR, ChildSpec).

child_spec(BootstrapBroker = {_, _}, ClientId, GroupId, Topics) when
    is_binary(ClientId), is_binary(GroupId), is_list(Topics)
->
    #{
        % We use the group id as the child id, so that you can use stop_consumer easily, below.
        % This is fine, because we assume that you only want to join the group once per application.
        id => GroupId,
        start =>
            {group_consumer_sup, start_link, [
                BootstrapBroker, ClientId, {GroupId, Topics}
            ]},
        type => supervisor,
        % 'permanent' is the default. We're explicit about it to make it clearer why 'delete_child', below.
        restart => permanent,
        % Give the group a chance to call LeaveGroup.
        shutdown => 5_000
    }.

stop_consumer(GroupId) ->
    ok = supervisor:terminate_child(?SUPERVISOR, GroupId),
    ok = supervisor:delete_child(?SUPERVISOR, GroupId).
