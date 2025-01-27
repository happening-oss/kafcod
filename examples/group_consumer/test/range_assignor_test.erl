-module(range_assignor_test).
-include_lib("eunit/include/eunit.hrl").

one_consumer_no_topic_test() ->
    M1 = create_member_id(1),
    Members = [
        #{
            member_id => M1,
            group_instance_id => null,
            metadata => range_assignor:metadata([])
        }
    ],
    TopicPartitions = [],
    Assignment = range_assignor:assign(Members, TopicPartitions),
    ?assertEqual([{M1, []}], Assignment),
    ok.

one_consumer_one_topic_test() ->
    M1 = create_member_id(1),
    Members = [
        #{
            member_id => M1,
            group_instance_id => null,
            metadata => range_assignor:metadata([<<"topic-a">>])
        }
    ],
    TopicPartitions = [#{name => <<"topic-a">>, partitions => [0, 1, 2]}],
    Assignment = range_assignor:assign(Members, TopicPartitions),
    ?assertEqual([{M1, [#{topic => <<"topic-a">>, partitions => [0, 1, 2]}]}], Assignment),
    ok.

one_consumer_multiple_topics_test() ->
    M1 = create_member_id(1),
    Members = [
        #{
            metadata => range_assignor:metadata([<<"topic-a">>, <<"topic-b">>]),
            member_id => M1,
            group_instance_id => null
        }
    ],
    TopicPartitions = [
        #{name => <<"topic-a">>, partitions => [0]},
        #{name => <<"topic-b">>, partitions => [0, 1]}
    ],
    Assignment = range_assignor:assign(Members, TopicPartitions),
    ?assertEqual(
        [
            {M1, [
                #{topic => <<"topic-a">>, partitions => [0]},
                #{topic => <<"topic-b">>, partitions => [0, 1]}
            ]}
        ],
        sort_assignment(Assignment)
    ),
    ok.

two_consumers_one_topic_one_partition_test() ->
    M1 = create_member_id(1),
    M2 = create_member_id(2),
    Members = [
        #{
            metadata => range_assignor:metadata([<<"topic-a">>]),
            member_id => M,
            group_instance_id => null
        }
     || M <- [M1, M2]
    ],
    TopicPartitions = [#{name => <<"topic-a">>, partitions => [0]}],
    Assignment = range_assignor:assign(Members, TopicPartitions),
    ?assertEqual(
        [
            % TODO: Does it make more sense for this to be a map, member => assigned_partitions?
            {M1, [#{topic => <<"topic-a">>, partitions => [0]}]},
            {M2, [#{topic => <<"topic-a">>, partitions => []}]}
        ],
        sort_assignment(Assignment)
    ),
    ok.

two_consumers_one_topic_two_partitions_test() ->
    M1 = create_member_id(1),
    M2 = create_member_id(2),
    Members = [
        #{
            metadata => range_assignor:metadata([<<"topic-a">>]),
            member_id => M,
            group_instance_id => null
        }
     || M <- [M1, M2]
    ],
    TopicPartitions = [#{name => <<"topic-a">>, partitions => [0, 1]}],
    Assignment = range_assignor:assign(Members, TopicPartitions),
    ?assertEqual(
        [
            {M1, [#{topic => <<"topic-a">>, partitions => [0]}]},
            {M2, [#{topic => <<"topic-a">>, partitions => [1]}]}
        ],
        Assignment
    ),
    ok.

two_consumers_two_topics_six_partitions_test() ->
    M1 = create_member_id(1),
    M2 = create_member_id(2),
    Members = [
        #{
            metadata => range_assignor:metadata([<<"topic-a">>, <<"topic-b">>]),
            member_id => M,
            group_instance_id => null
        }
     || M <- [M1, M2]
    ],
    TopicPartitions = [
        #{name => <<"topic-a">>, partitions => [0, 1, 2, 3, 4, 5]},
        #{name => <<"topic-b">>, partitions => [0, 1, 2, 3, 4, 5]}
    ],
    Assignment = range_assignor:assign(Members, TopicPartitions),
    ?assertEqual(
        [
            {M1, [
                #{topic => <<"topic-a">>, partitions => [0, 1, 2]},
                #{topic => <<"topic-b">>, partitions => [0, 1, 2]}
            ]},
            {M2, [
                #{topic => <<"topic-a">>, partitions => [3, 4, 5]},
                #{topic => <<"topic-b">>, partitions => [3, 4, 5]}
            ]}
        ],
        sort_assignment(Assignment)
    ),
    ok.

multiple_consumers_mixed_topics_test() ->
    Members = [
        create_member(1, [<<"topic-a">>]),
        create_member(2, [<<"topic-a">>, <<"topic-b">>]),
        create_member(3, [<<"topic-a">>])
    ],
    [M1, M2, M3] = [M || #{member_id := M} <- Members],
    TopicPartitions = [
        #{name => <<"topic-a">>, partitions => [0, 1, 2]},
        #{name => <<"topic-b">>, partitions => [0, 1]}
    ],
    Assignment = range_assignor:assign(Members, TopicPartitions),
    ?assertEqual(
        [
            {M1, [
                #{topic => <<"topic-a">>, partitions => [0]}
            ]},
            {M2, [
                #{topic => <<"topic-a">>, partitions => [1]},
                #{topic => <<"topic-b">>, partitions => [0, 1]}
            ]},
            {M3, [
                #{topic => <<"topic-a">>, partitions => [2]}
            ]}
        ],
        sort_assignment(Assignment)
    ),
    ok.

multiple_consumers_unwanted_topics_test() ->
    % Question: what if none of the consumers express an interest in one of the topics?
    % Answer: it'll fail when attempting to share the partitions between zero interested consumers.
    %
    % Don't do that.
    % Make sure that the TopicPartitions passed to 'assign' only contains topics that the consumers ask for.
    %
    % This test is a placeholder, so I can hang this comment somewhere.
    ok.

create_member_id(Index) ->
    create_member_id(<<"member">>, Index).

create_member_id(Prefix, Index) when is_binary(Prefix), is_integer(Index) ->
    % Member IDs are usually <prefix>-<uuid>, but we need them to be deterministic for tests.
    Fixed = <<"11223344-5566-7788-9900">>,
    iolist_to_binary(io_lib:format("~s-~s-~12..0B", [Prefix, Fixed, Index])).

create_member(Index, Topics) ->
    #{
        member_id => create_member_id(Index),
        metadata => range_assignor:metadata(Topics),
        group_instance_id => null
    }.

% The order of the returned assignment is non-deterministic, which is fine. Unless you're writing tests.
sort_assignment(Assignment) ->
    lists:sort(
        lists:map(
            fun({MemberId, TopicPartitions}) ->
                {MemberId,
                    lists:sort(
                        fun(#{topic := A}, #{topic := B}) -> A =< B end,
                        lists:map(
                            fun(TPs = #{partitions := Partitions}) ->
                                TPs#{partitions := lists:sort(Partitions)}
                            end,
                            TopicPartitions
                        )
                    )}
            end,
            Assignment
        )
    ).
