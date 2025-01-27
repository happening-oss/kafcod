-module(join_group_request_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

% Why are we testing the decode of join_group_request? Surely that's a broker thing? Yeah. I'm doing it because I'm
% trying to figure out how to *create* a JoinGroup request. This one was captured from kcat, and it might be
% illuminating to see what's in it.

v5_decode_test() ->
    Capture =
        <<0, 11, 0, 5, 0, 0, 0, 3, 0, 7, 114, 100, 107, 97, 102, 107, 97, 0, 7, 103, 114, 111, 117,
            112, 45, 97, 0, 0, 175, 200, 0, 4, 147, 224, 0, 0, 255, 255, 0, 8, 99, 111, 110, 115,
            117, 109, 101, 114, 0, 0, 0, 2, 0, 5, 114, 97, 110, 103, 101, 0, 0, 0, 23, 0, 1, 0, 0,
            0, 1, 0, 7, 116, 111, 112, 105, 99, 45, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 114, 111,
            117, 110, 100, 114, 111, 98, 105, 110, 0, 0, 0, 23, 0, 1, 0, 0, 0, 1, 0, 7, 116, 111,
            112, 105, 99, 45, 97, 0, 0, 0, 0, 0, 0, 0, 0>>,
    Decoded = join_group_request:decode_join_group_request_5(Capture),
    ?assertMatch(
        {
            #{
                group_id := <<"group-a">>,
                group_instance_id := null,
                member_id := <<>>,
                protocol_type := <<"consumer">>,
                protocols :=
                    [
                        #{
                            metadata := _,
                            name := <<"range">>
                        },
                        #{
                            metadata := _,
                            name := <<"roundrobin">>
                        }
                    ],
                rebalance_timeout_ms := 300000,
                session_timeout_ms := 45000
            },
            <<>>
        },
        Decoded
    ),
    {#{protocols := [#{metadata := Metadata1}, #{metadata := Metadata2}]}, _} = Decoded,
    % MemberMetadata => Version Subscription AssignmentStrategies
    ?assertEqual(
        iolist_to_binary([
            % Version => int16
            <<0, 1>>,
            % Subscription => Topics UserData
            [
                % Topics => [String]
                [<<0, 0, 0, 1>>, [<<0, 7, 116, 111, 112, 105, 99, 45, 97>>]],
                % UserData => bytes
                <<0, 0, 0, 0>>
            ],
            % OwnedPartitions => [TopicPartition]
            <<0, 0, 0, 0>>
        ]),
        Metadata1
    ),
    % They happen to be the same, so there's no need to repeat the long-winded assertion.
    ?assertEqual(
        Metadata1,
        Metadata2
    ),
    <<1:16/big-signed, Metadata/binary>> = Metadata1,
    ?assertEqual(
        {
            #{
                topics => [<<"topic-a">>],
                user_data => <<>>,
                owned_partitions => []
            },
            <<>>
        },
        consumer_protocol_subscription:decode_consumer_protocol_subscription_1(Metadata)
    ),
    ok.

v7_encode_test() ->
    ?assertEqual(
        <<0, 11, 0, 7, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45,
            72, 69, 82, 69, 0, 8, 103, 114, 111, 117, 112, 45, 97, 0, 0, 175, 200, 0, 4, 147, 224,
            1, 0, 9, 99, 111, 110, 115, 117, 109, 101, 114, 3, 6, 114, 97, 110, 103, 101, 1, 0, 11,
            114, 111, 117, 110, 100, 114, 111, 98, 105, 110, 1, 0, 0>>,
        iolist_to_binary(
            join_group_request:encode_join_group_request_7(#{
                client_id => ?CLIENT_ID,
                correlation_id => ?CORRELATION_ID,
                group_id => <<"group-a">>,
                group_instance_id => null,
                member_id => <<>>,
                protocol_type => <<"consumer">>,
                protocols =>
                    [
                        #{
                            metadata => <<>>,
                            name => <<"range">>
                        },
                        #{
                            metadata => <<>>,
                            name => <<"roundrobin">>
                        }
                    ],
                rebalance_timeout_ms => 300000,
                session_timeout_ms => 45000
            })
        )
    ),
    ok.

v7_metadata_encode_test() ->
    Version = 0,
    Metadata = iolist_to_binary([
        <<Version:16/big-signed>>,
        consumer_protocol_subscription:encode_consumer_protocol_subscription_0(#{
            topics => [<<"cars">>], user_data => <<>>
        })
    ]),
    Request = join_group_request:encode_join_group_request_7(#{
        client_id => ?CLIENT_ID,
        correlation_id => ?CORRELATION_ID,
        group_id => <<"group-a">>,
        group_instance_id => null,
        member_id => <<>>,
        protocol_type => <<"consumer">>,
        protocols =>
            [
                #{
                    metadata => Metadata,
                    name => <<"range">>
                },
                #{
                    metadata => Metadata,
                    name => <<"roundrobin">>
                }
            ],
        rebalance_timeout_ms => 300000,
        session_timeout_ms => 45000
    }),
    ?assertEqual(
        iolist_to_binary([
            <<0, 11>>,
            <<0, 7>>,
            <<12, 34, 56, 78>>,
            [<<0, 17>>, <<67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45, 72, 69, 82, 69>>],
            [<<0, 8>>, <<103, 114, 111, 117, 112, 45, 97>>],
            <<0, 0, 175, 200>>,
            <<0, 4, 147, 224>>,
            <<1>>,
            <<0>>,
            [<<9>>, <<99, 111, 110, 115, 117, 109, 101, 114>>],
            [
                <<3>>,
                [
                    <<6, 114, 97, 110, 103, 101>>,
                    [
                        <<17>>,
                        <<0, 0, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 0>>
                    ],
                    <<0>>
                ],
                [
                    <<11, 114, 111, 117, 110, 100, 114, 111, 98, 105, 110>>,
                    [
                        <<17>>,
                        <<0, 0, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 0, 0>>
                    ],
                    <<0>>
                ]
            ]
        ]),
        iolist_to_binary(Request)
    ),
    ok.

% kafire uses v2
v2_decode_test() ->
    Capture =
        <<0, 11, 0, 2, 0, 0, 0, 1, 0, 6, 107, 97, 102, 105, 114, 101, 0, 22, 116, 120, 95, 111, 102,
            102, 115, 101, 116, 95, 99, 111, 109, 109, 105, 116, 95, 103, 114, 111, 117, 112, 0, 0,
            39, 16, 0, 0, 19, 136, 0, 0, 0, 8, 99, 111, 110, 115, 117, 109, 101, 114, 0, 0, 0, 1, 0,
            10, 114, 111, 117, 110, 100, 114, 111, 98, 105, 110, 0, 0, 0, 32, 0, 0, 0, 0, 0, 2, 0,
            1, 112, 0, 1, 113, 0, 0, 0, 0, 0, 0, 0, 1, 0, 10, 114, 111, 117, 110, 100, 114, 111, 98,
            105, 110>>,
    ?assertEqual(
        {
            #{
                protocols => [
                    #{
                        name => <<"roundrobin">>,
                        metadata =>
                            <<0, 0, 0, 0, 0, 2, 0, 1, 112, 0, 1, 113, 0, 0, 0, 0, 0, 0, 0, 1, 0, 10,
                                114, 111, 117, 110, 100, 114, 111, 98, 105, 110>>
                    }
                ],
                group_id => <<"tx_offset_commit_group">>,
                correlation_id => 1,
                client_id => <<"kafire">>,
                api_key => 11,
                api_version => 2,
                rebalance_timeout_ms => 5000,
                member_id => <<>>,
                protocol_type => <<"consumer">>,
                session_timeout_ms => 10000
            },
            <<>>
        },
        join_group_request:decode_join_group_request_2(Capture)
    ),
    ok.

v5_cooperative_test() ->
    % Captured from:
    %   ./rdkafka_complex_consumer_example -g puorg -b localhost:9092 \
    %       -X partition.assignment.strategy=cooperative-sticky cars
    <<Length:32/big-signed, Capture:Length/binary>> =
        <<0, 0, 0, 100, 0, 11, 0, 5, 0, 0, 0, 2, 0, 7, 114, 100, 107, 97, 102, 107, 97, 0, 5, 112,
            117, 111, 114, 103, 0, 0, 175, 200, 0, 4, 147, 224, 0, 0, 255, 255, 0, 8, 99, 111, 110,
            115, 117, 109, 101, 114, 0, 0, 0, 1, 0, 18, 99, 111, 111, 112, 101, 114, 97, 116, 105,
            118, 101, 45, 115, 116, 105, 99, 107, 121, 0, 0, 0, 26, 0, 3, 0, 0, 0, 1, 0, 4, 99, 97,
            114, 115, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0>>,
    ?assertEqual(
        {
            #{
                protocols =>
                    [
                        #{
                            name => <<"cooperative-sticky">>,
                            metadata =>
                                <<0, 3, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 0, 0, 0, 0, 0,
                                    255, 255, 255, 255, 0, 0>>
                        }
                    ],
                api_key => 11,
                correlation_id => 2,
                client_id => <<"rdkafka">>,
                api_version => 5,
                group_id => <<"puorg">>,
                session_timeout_ms => 45000,
                member_id => <<>>,
                protocol_type => <<"consumer">>,
                rebalance_timeout_ms => 300000,
                group_instance_id => null
            },
            <<>>
        },
        join_group_request:decode_join_group_request_5(Capture)
    ),
    ok.

v5_cooperative_rejoin_test() ->
    <<Length:32/big-signed, Capture:Length/binary>> =
        <<0, 0, 0, 252, 0, 11, 0, 5, 0, 0, 0, 12, 0, 7, 114, 100, 107, 97, 102, 107, 97, 0, 5, 112,
            117, 111, 114, 103, 0, 0, 175, 200, 0, 4, 147, 224, 0, 44, 114, 100, 107, 97, 102, 107,
            97, 45, 98, 50, 57, 50, 98, 99, 97, 53, 45, 54, 48, 55, 48, 45, 52, 101, 50, 101, 45,
            56, 102, 54, 100, 45, 100, 49, 52, 50, 55, 101, 48, 55, 52, 101, 49, 99, 255, 255, 0, 8,
            99, 111, 110, 115, 117, 109, 101, 114, 0, 0, 0, 1, 0, 18, 99, 111, 111, 112, 101, 114,
            97, 116, 105, 118, 101, 45, 115, 116, 105, 99, 107, 121, 0, 0, 0, 134, 0, 3, 0, 0, 0, 1,
            0, 4, 99, 97, 114, 115, 0, 0, 0, 58, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 10, 0,
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0,
            0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0,
            10, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6,
            0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0>>,
    ?assertEqual(
        {
            #{
                protocols =>
                    [
                        #{
                            name => <<"cooperative-sticky">>,
                            metadata =>
                                <<0, 3, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 58, 0, 0, 0, 1,
                                    0, 4, 99, 97, 114, 115, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                    0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0,
                                    0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 1, 0, 4, 99,
                                    97, 114, 115, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2,
                                    0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0,
                                    0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0>>
                        }
                    ],
                api_key => 11,
                correlation_id => 12,
                client_id => <<"rdkafka">>,
                api_version => 5,
                group_id => <<"puorg">>,
                session_timeout_ms => 45000,
                member_id =>
                    <<"rdkafka-b292bca5-6070-4e2e-8f6d-d1427e074e1c">>,
                protocol_type => <<"consumer">>,
                rebalance_timeout_ms => 300000,
                group_instance_id => null
            },
            <<>>
        },
        join_group_request:decode_join_group_request_5(Capture)
    ),
    ok.
