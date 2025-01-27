-module(consumer_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

v0_subscription_encode_test() ->
    Version = 0,
    ?assertEqual(
        iolist_to_binary(
            [<<0, 0>>, [<<0, 0, 0, 1>>, [<<0, 4>>, <<99, 97, 114, 115>>]], <<0, 0, 0, 0>>]
        ),
        iolist_to_binary([
            <<Version:16/big-signed>>,
            consumer_protocol_subscription:encode_consumer_protocol_subscription_0(#{
                topics => [<<"cars">>], user_data => <<>>
            })
        ])
    ).

v0_assignment_encode_test() ->
    Version = 0,
    ?assertEqual(
        iolist_to_binary([
            <<0, 0>>,
            [
                <<0, 0, 0, 2>>,
                [
                    [
                        [<<0, 7>>, <<"topic-a">>],
                        [
                            <<0, 0, 0, 4>>,
                            [<<0, 0, 0, 0>>, <<0, 0, 0, 1>>, <<0, 0, 0, 2>>, <<0, 0, 0, 3>>]
                        ]
                    ],
                    [
                        [<<0, 7>>, <<"topic-b">>],
                        [
                            <<0, 0, 0, 4>>,
                            [<<0, 0, 0, 0>>, <<0, 0, 0, 1>>, <<0, 0, 0, 2>>, <<0, 0, 0, 3>>]
                        ]
                    ]
                ]
            ],
            [<<0, 0, 0, 5>>, <<"hello">>]
        ]),
        iolist_to_binary([
            <<Version:16/big-signed>>,
            consumer_protocol_assignment:encode_consumer_protocol_assignment_0(#{
                assigned_partitions => [
                    #{topic => <<"topic-a">>, partitions => [0, 1, 2, 3]},
                    #{topic => <<"topic-b">>, partitions => [0, 1, 2, 3]}
                ],
                user_data => <<"hello">>
            })
        ])
    ).

kcat_metadata_decode_v3_test() ->
    % kcat uses metadata/subscription v3
    Capture =
        <<0, 3, 0, 0, 0, 2, 0, 4, 99, 97, 114, 115, 0, 10, 104, 105, 103, 104, 108, 97, 110, 100,
            101, 114, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0>>,
    ?assertEqual(
        #{
            user_data => <<>>,
            topics => [<<"cars">>, <<"highlander">>],
            rack_id => <<>>,
            owned_partitions => [],
            generation_id => -1
        },
        kafcod_consumer_protocol:decode_metadata(Capture)
    ).

cooperative_metadata_decode_v3_test() ->
    % Captured from a cooperative/incremental JoinGroup. Since it's the initial join, it's basically the same as the one
    % above.
    Capture =
        <<0, 3, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0,
            0>>,
    ?assertEqual(
        #{
            user_data => <<>>,
            topics => [<<"cars">>],
            generation_id => -1,
            owned_partitions => [],
            rack_id => <<>>
        },
        kafcod_consumer_protocol:decode_metadata(Capture)
    ).

cooperative_metadata_rejoin_v3_test() ->
    Capture =
        <<0, 3, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 58, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115,
            0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0,
            0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 1, 0, 4, 99, 97, 114,
            115, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0,
            5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0>>,
    Metadata = kafcod_consumer_protocol:decode_metadata(Capture),
    ?assertEqual(
        #{
            user_data =>
                <<0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                    2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0,
                    0, 9, 0, 0, 0, 1>>,
            topics => [<<"cars">>],
            generation_id => 1,
            owned_partitions =>
                [
                    #{
                        topic => <<"cars">>,
                        partitions => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
                    }
                ],
            rack_id => <<>>
        },
        Metadata
    ),

    % We don't really need to assert this -- it's just the data, but we'll have a guess as to how it's made.
    #{user_data := UserData} = Metadata,
    ?assertEqual(
        iolist_to_binary(
            [
                % probably count(Topics), might be version
                <<0, 0, 0, 1>>,
                % "cars"
                [
                    <<0, 4>>,
                    <<99, 97, 114, 115>>
                ],
                [
                    % count(Partitions)
                    <<0, 0, 0, 10>>,
                    [
                        <<0, 0, 0, 0>>,
                        <<0, 0, 0, 1>>,
                        <<0, 0, 0, 2>>,
                        <<0, 0, 0, 3>>,
                        <<0, 0, 0, 4>>,
                        <<0, 0, 0, 5>>,
                        <<0, 0, 0, 6>>,
                        <<0, 0, 0, 7>>,
                        <<0, 0, 0, 8>>,
                        <<0, 0, 0, 9>>
                    ],

                    % /shrug
                    <<0, 0, 0, 1>>
                ]
            ]
        ),
        UserData
    ),
    ok.
