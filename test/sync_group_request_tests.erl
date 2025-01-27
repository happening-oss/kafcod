-module(sync_group_request_tests).
-include_lib("eunit/include/eunit.hrl").

captured_leader() ->
    % Captured from Wireshark:
    % 1. Right-click on the SyncGroup v3 Request.
    % 2. Select "Copy" > "...as a Hex stream"
    % 3. In Erlang, 'rp(binary:decode_hex(<PASTE>)).'
    % Then hand-edited and -annotated to break it down into fields.
    iolist_to_binary([
        [<<0, 14>>, <<0, 3>>, <<0, 0, 0, 6>>, <<0, 7, 114, 100, 107, 97, 102, 107, 97>>],
        [
            % Group Name
            [<<0, 13>>, <<109, 121, 95, 103, 114, 111, 117, 112, 95, 110, 97, 109, 101>>],
            % Generation ID
            <<0, 0, 0, 1>>,
            % Member ID
            [
                <<0, 44>>,
                <<114, 100, 107, 97, 102, 107, 97, 45, 48, 53, 98, 101, 102, 54, 54, 49, 45, 102,
                    54, 102, 98, 45, 52, 97, 99, 100, 45, 97, 56, 102, 54, 45, 99, 49, 53, 54, 100,
                    48, 52, 99, 48, 53, 54, 48>>
            ],
            % Group Instance ID
            <<255, 255>>,
            % Assignments
            [
                <<0, 0, 0, 2>>,
                [
                    % MemberId
                    [
                        <<0, 44>>,
                        <<114, 100, 107, 97, 102, 107, 97, 45, 53, 57, 57, 49, 54, 55, 100, 53, 45,
                            99, 49, 97, 56, 45, 52, 54, 48, 51, 45, 98, 51, 55, 52, 45, 98, 56, 51,
                            56, 101, 97, 101, 50, 49, 52, 56, 50>>
                    ],
                    % Assignment
                    [
                        <<0, 0, 0, 52, 0, 0, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 8, 0, 0,
                            0, 8, 0, 0, 0, 9, 0, 0, 0, 10, 0, 0, 0, 11, 0, 0, 0, 12, 0, 0, 0, 13, 0,
                            0, 0, 14, 0, 0, 0, 15, 0, 0, 0, 0>>
                    ]
                ],
                [
                    [
                        <<0, 44>>,
                        <<114, 100, 107, 97, 102, 107, 97, 45, 48, 53, 98, 101, 102, 54, 54, 49, 45,
                            102, 54, 102, 98, 45, 52, 97, 99, 100, 45, 97, 56, 102, 54, 45, 99, 49,
                            53, 54, 100, 48, 52, 99, 48, 53, 54, 48>>
                    ],
                    % Assignment
                    [
                        % Length
                        <<0, 0, 0, 52>>,
                        % Version
                        <<0, 0, 0, 0>>,
                        % TopicPartitions
                        [
                            <<0, 1>>,
                            [
                                % Topic: <<"cars">>
                                [<<0, 4>>, <<99, 97, 114, 115>>],
                                % Partitions
                                [
                                    % [0..7]
                                    <<0, 0, 0, 8>>,
                                    [
                                        <<0, 0, 0, 0>>,
                                        <<0, 0, 0, 1>>,
                                        <<0, 0, 0, 2>>,
                                        <<0, 0, 0, 3>>,
                                        <<0, 0, 0, 4>>,
                                        <<0, 0, 0, 5>>,
                                        <<0, 0, 0, 6>>,
                                        <<0, 0, 0, 7>>
                                    ]
                                ]
                            ]
                        ],
                        <<0, 0, 0, 0>>
                    ]
                ]
            ]
        ]
    ]).

v3_encode_leader_test() ->
    ?assertEqual(
        captured_leader(),
        iolist_to_binary(
            sync_group_request:encode_sync_group_request_3(#{
                correlation_id => 6,
                client_id => <<"rdkafka">>,
                group_id => <<"my_group_name">>,
                generation_id => 1,
                member_id => <<"rdkafka-05bef661-f6fb-4acd-a8f6-c156d04c0560">>,
                group_instance_id => null,
                assignments => [
                    #{
                        member_id => <<"rdkafka-599167d5-c1a8-4603-b374-b838eae21482">>,
                        assignment => iolist_to_binary([
                            <<0:16/big-signed>>,
                            consumer_protocol_assignment:encode_consumer_protocol_assignment_0(
                                #{
                                    assigned_partitions => [
                                        #{
                                            topic => <<"cars">>,
                                            partitions => [8, 9, 10, 11, 12, 13, 14, 15]
                                        }
                                    ],
                                    user_data => <<>>
                                }
                            )
                        ])
                    },
                    #{
                        member_id => <<"rdkafka-05bef661-f6fb-4acd-a8f6-c156d04c0560">>,
                        assignment => iolist_to_binary([
                            <<0:16/big-signed>>,
                            consumer_protocol_assignment:encode_consumer_protocol_assignment_0(
                                #{
                                    assigned_partitions => [
                                        #{
                                            topic => <<"cars">>,
                                            partitions => [0, 1, 2, 3, 4, 5, 6, 7]
                                        }
                                    ],
                                    user_data => <<>>
                                }
                            )
                        ])
                    }
                ]
            })
        )
    ).

captured_follower() ->
    <<0, 14, 0, 3, 0, 0, 0, 4, 0, 7, 114, 100, 107, 97, 102, 107, 97, 0, 5, 103, 114, 111, 117, 112,
        0, 0, 0, 2, 0, 44, 114, 100, 107, 97, 102, 107, 97, 45, 57, 52, 102, 51, 49, 49, 97, 52, 45,
        54, 98, 51, 55, 45, 52, 56, 98, 101, 45, 97, 53, 102, 102, 45, 50, 51, 51, 51, 55, 98, 100,
        98, 97, 50, 54, 55, 255, 255, 0, 0, 0, 0>>.

v3_decode_follower_test() ->
    ?assertEqual(
        {
            #{
                group_id => <<"group">>,
                correlation_id => 4,
                client_id => <<"rdkafka">>,
                api_key => 14,
                api_version => 3,
                member_id => <<"rdkafka-94f311a4-6b37-48be-a5ff-23337bdba267">>,
                generation_id => 2,
                assignments => [],
                group_instance_id => null
            },
            <<>>
        },
        sync_group_request:decode_sync_group_request_3(captured_follower())
    ).
