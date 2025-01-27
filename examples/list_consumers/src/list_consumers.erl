-module(list_consumers).
-export([main/1]).

-define(CLIENT_ID, atom_to_binary(?MODULE)).

main(Args) ->
    argparse:run(
        Args,
        #{
            arguments => [
                #{
                    name => broker,
                    help => "Bootstrap broker (host[:port])",
                    type => {custom, fun parse_broker/1}
                }
            ],
            handler => fun list_consumers/1
        },
        #{progname => ?MODULE}
    ).

-define(DEFAULT_BROKER_PORT, 9092).

parse_broker(Arg) when is_list(Arg) ->
    case string:split(Arg, ":") of
        [Host, Port] ->
            {Host, list_to_integer(Port)};
        [Host] ->
            {Host, ?DEFAULT_BROKER_PORT};
        _ ->
            error(badarg)
    end.

list_consumers(#{broker := {BootstrapHost, BootstrapPort}}) ->
    {ok, C} = kafka_connection:start_link(BootstrapHost, BootstrapPort, ?CLIENT_ID),

    % To get the full list of groups, we have to ask each broker and combine the results. So, first, we need a list of
    % brokers.
    {ok, #{brokers := Brokers}} = kafka_connection:call(
        C,
        fun metadata_request:encode_metadata_request_9/1,
        #{
            % empty list, because we don't want the topic names; null would return all topics.
            topics => [],
            allow_auto_topic_creation => false,
            include_cluster_authorized_operations => false,
            include_topic_authorized_operations => false
        },
        fun metadata_response:decode_metadata_response_9/1
    ),

    Groups = lists:foldl(
        fun(_Broker = #{host := Host, port := Port}, Acc) ->
            {ok, B} = kafka_connection:start_link(Host, Port, ?CLIENT_ID),
            {ok, #{error_code := 0, groups := Groups}} = kafka_connection:call(
                B,
                fun list_groups_request:encode_list_groups_request_3/1,
                #{},
                fun list_groups_response:decode_list_groups_response_3/1
            ),

            Acc ++ Groups
        end,
        [],
        Brokers
    ),

    % Neither Wireshark nor Kafka 3.4.0 support FindCoordinator v4, so we have to use v3. This only allows for one key,
    % so we'll have to make multiple calls.

    % Describe the groups. We have to find the coordinator for each group first.
    % Coordinators :: [Coordinator]
    Coordinators = lists:foldl(
        fun(#{group_id := GroupId}, Acc) ->
            {ok, Coordinator} = kafka_connection:call(
                C,
                fun find_coordinator_request:encode_find_coordinator_request_3/1,
                #{
                    key_type => 0,
                    key => GroupId
                },
                fun find_coordinator_response:decode_find_coordinator_response_3/1
            ),
            [Coordinator#{key => GroupId} | Acc]
        end,
        [],
        Groups
    ),

    % Group those by coordinator and then submit DescribeGroup requests.
    % GroupsByCoordinator :: #{{NodeId, Host, Port} := [GroupName]}
    GroupsByCoordinator = maps:groups_from_list(
        fun(#{node_id := NodeId, host := Host, port := Port}) -> {NodeId, Host, Port} end,
        fun(#{key := GroupName}) -> GroupName end,
        Coordinators
    ),

    GroupDescriptions = maps:fold(
        fun({_NodeId, Host, Port}, GroupNames, Acc) ->
            {ok, B} = kafka_connection:start_link(Host, Port, ?CLIENT_ID),
            {ok, #{groups := GDs}} = kafka_connection:call(
                B,
                fun describe_groups_request:encode_describe_groups_request_5/1,
                #{
                    groups => GroupNames,
                    include_authorized_operations => true
                },
                fun describe_groups_response:decode_describe_groups_response_5/1
            ),
            Acc ++ GDs
        end,
        [],
        GroupsByCoordinator
    ),

    io:format("~p~n", [simplify_group_descriptions(GroupDescriptions)]),
    ok.

simplify_group_descriptions(GroupDescriptions) ->
    lists:map(
        fun(GroupDesc = #{members := Members}) ->
            Members2 = lists:map(
                fun(Member) -> maps:without([member_metadata, member_assignment], Member) end,
                Members
            ),
            GroupDesc#{members := Members2}
        end,
        GroupDescriptions
    ).
