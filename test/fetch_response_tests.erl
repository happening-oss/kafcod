-module(fetch_response_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

-define(CORRELATION_ID, 203569230).

v4_mixed_test() ->
    Capture =
        <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 7, 116, 111, 112, 105, 99, 45, 97, 0, 0, 0, 1, 0,
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 1, 253,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 61, 0, 0, 0, 0, 2, 186, 97, 71, 204, 0, 0, 0, 0, 0, 0,
            0, 0, 1, 136, 200, 139, 4, 208, 0, 0, 1, 136, 200, 139, 4, 208, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 1, 22, 0, 0, 0, 1, 10, 104, 101,
            108, 108, 111, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 62, 0, 0, 0, 0, 2, 192, 20, 175, 17,
            0, 0, 0, 0, 0, 0, 0, 0, 1, 136, 200, 139, 51, 54, 0, 0, 1, 136, 200, 139, 51, 54, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 1, 24, 0, 0,
            0, 1, 12, 104, 101, 108, 108, 111, 50, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 62, 0, 0, 0,
            0, 2, 71, 99, 225, 193, 0, 0, 0, 0, 0, 0, 0, 0, 1, 136, 200, 139, 83, 104, 0, 0, 1, 136,
            200, 139, 83, 104, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            0, 0, 0, 1, 24, 0, 0, 0, 1, 12, 104, 101, 108, 108, 111, 51, 0, 0, 0, 0, 0, 0, 0, 0, 3,
            0, 0, 0, 69, 0, 0, 0, 0, 2, 74, 134, 57, 155, 0, 0, 0, 0, 0, 0, 0, 0, 1, 136, 200, 140,
            12, 245, 0, 0, 1, 136, 200, 140, 12, 245, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 0, 0, 0, 1, 38, 0, 0, 0, 14, 116, 104, 101, 45, 107, 101, 121,
            12, 104, 101, 108, 108, 111, 51, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 87, 0, 0, 0, 0, 2,
            112, 121, 7, 31, 0, 0, 0, 0, 0, 0, 0, 0, 1, 136, 201, 19, 209, 216, 0, 0, 1, 136, 201,
            19, 209, 216, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0,
            0, 0, 1, 74, 0, 0, 0, 14, 116, 104, 101, 45, 107, 101, 121, 32, 107, 101, 121, 115, 45,
            97, 110, 100, 45, 104, 101, 97, 100, 101, 114, 115, 2, 6, 102, 111, 111, 6, 98, 97, 114,
            0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 96, 0, 0, 0, 0, 2, 223, 180, 173, 230, 0, 0, 0, 0, 0,
            0, 0, 0, 1, 136, 201, 23, 5, 2, 0, 0, 1, 136, 201, 23, 5, 2, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 1, 92, 0, 0, 0, 14, 116, 104, 101,
            45, 107, 101, 121, 32, 107, 101, 121, 115, 45, 97, 110, 100, 45, 104, 101, 97, 100, 101,
            114, 115, 4, 6, 102, 111, 111, 6, 98, 97, 114, 6, 98, 97, 122, 8, 113, 117, 117, 120>>,
    ?assertEqual(
        {
            #{
                correlation_id => 2,
                responses =>
                    [
                        #{
                            partitions =>
                                [
                                    #{
                                        aborted_transactions => [],
                                        error_code => 0,
                                        high_watermark => 6,
                                        last_stable_offset => 6,
                                        partition_index => 1,
                                        records =>
                                            [
                                                #{
                                                    attributes => #{compression => none},
                                                    base_offset => 0,
                                                    base_sequence => -1,
                                                    base_timestamp => 1686991733968,
                                                    crc => 3126937548,
                                                    last_offset_delta => 0,
                                                    magic => 2,
                                                    max_timestamp => 1686991733968,
                                                    partition_leader_epoch => 0,
                                                    producer_epoch => -1,
                                                    producer_id => -1,
                                                    records => [
                                                        #{
                                                            attributes => 0,
                                                            headers => [],
                                                            key => null,
                                                            offset_delta => 0,
                                                            timestamp_delta => 0,
                                                            value => <<"hello">>
                                                        }
                                                    ]
                                                },
                                                #{
                                                    attributes => #{compression => none},
                                                    base_offset => 1,
                                                    base_sequence => -1,
                                                    base_timestamp => 1686991745846,
                                                    crc => 3222581009,
                                                    last_offset_delta => 0,
                                                    magic => 2,
                                                    max_timestamp => 1686991745846,
                                                    partition_leader_epoch => 0,
                                                    producer_epoch => -1,
                                                    producer_id => -1,
                                                    records => [
                                                        #{
                                                            attributes => 0,
                                                            headers => [],
                                                            key => null,
                                                            offset_delta => 0,
                                                            timestamp_delta => 0,
                                                            value => <<"hello2">>
                                                        }
                                                    ]
                                                },
                                                #{
                                                    attributes => #{compression => none},
                                                    base_offset => 2,
                                                    base_sequence => -1,
                                                    base_timestamp => 1686991754088,
                                                    crc => 1197728193,
                                                    last_offset_delta => 0,
                                                    magic => 2,
                                                    max_timestamp => 1686991754088,
                                                    partition_leader_epoch => 0,
                                                    producer_epoch => -1,
                                                    producer_id => -1,
                                                    records => [
                                                        #{
                                                            attributes => 0,
                                                            headers => [],
                                                            key => null,
                                                            offset_delta => 0,
                                                            timestamp_delta => 0,
                                                            value => <<"hello3">>
                                                        }
                                                    ]
                                                },
                                                #{
                                                    attributes => #{compression => none},
                                                    base_offset => 3,
                                                    base_sequence => -1,
                                                    base_timestamp => 1686991801589,
                                                    crc => 1250310555,
                                                    last_offset_delta => 0,
                                                    magic => 2,
                                                    max_timestamp => 1686991801589,
                                                    partition_leader_epoch => 0,
                                                    producer_epoch => -1,
                                                    producer_id => -1,
                                                    records => [
                                                        #{
                                                            attributes => 0,
                                                            headers => [],
                                                            key => <<"the-key">>,
                                                            offset_delta => 0,
                                                            timestamp_delta => 0,
                                                            value => <<"hello3">>
                                                        }
                                                    ]
                                                },
                                                #{
                                                    attributes => #{compression => none},
                                                    base_offset => 4,
                                                    base_sequence => -1,
                                                    base_timestamp => 1687000699352,
                                                    crc => 1886979871,
                                                    last_offset_delta => 0,
                                                    magic => 2,
                                                    max_timestamp => 1687000699352,
                                                    partition_leader_epoch => 0,
                                                    producer_epoch => -1,
                                                    producer_id => -1,
                                                    records => [
                                                        #{
                                                            attributes => 0,
                                                            headers => [{<<"foo">>, <<"bar">>}],
                                                            key => <<"the-key">>,
                                                            offset_delta => 0,
                                                            timestamp_delta => 0,
                                                            value => <<"keys-and-headers">>
                                                        }
                                                    ]
                                                },
                                                #{
                                                    attributes => #{compression => none},
                                                    base_offset => 5,
                                                    base_sequence => -1,
                                                    base_timestamp => 1687000909058,
                                                    crc => 3753160166,
                                                    last_offset_delta => 0,
                                                    magic => 2,
                                                    max_timestamp => 1687000909058,
                                                    partition_leader_epoch => 0,
                                                    producer_epoch => -1,
                                                    producer_id => -1,
                                                    records =>
                                                        [
                                                            #{
                                                                attributes => 0,
                                                                headers => [
                                                                    {<<"foo">>, <<"bar">>},
                                                                    {<<"baz">>, <<"quux">>}
                                                                ],
                                                                key => <<"the-key">>,
                                                                offset_delta => 0,
                                                                timestamp_delta => 0,
                                                                value => <<"keys-and-headers">>
                                                            }
                                                        ]
                                                }
                                            ]
                                    }
                                ],
                            topic => <<"topic-a">>
                        }
                    ],
                throttle_time_ms => 0
            },
            <<>>
        },
        fetch_response:decode_fetch_response_4(Capture)
    ).

v11_extra_data_test() ->
    % Quite frequently Kafka will include extra crap at the end of a message set; it looks like a truncated record
    % batch. We need to ignore it and skip to the next message set/partition.

    % As far as I can tell, when you fetch records, and Kafka's trying to honour your MaxBytes setting, you'll get a
    % complete partition, followed by an incomplete partition (with the truncated record batch), followed by an empty
    % partition.

    % This capture was taken when reproducing the problem with a (relatively) small set of produced messages
    Capture = iolist_to_binary(
        [
            % Correlation ID
            <<0, 0, 0, 1>>,
            % throttle_time_ms, error_code, session_id
            [<<0, 0, 0, 0>>, <<0, 0>>, <<0, 0, 0, 0>>],
            % Responses
            [
                % 1 topic
                <<0, 0, 0, 1>>,
                % "cars"
                [<<0, 4>>, <<99, 97, 114, 115>>],
                % 3 partitions
                <<0, 0, 0, 3>>,
                [
                    [
                        % partition_index = 2
                        <<0, 0, 0, 2>>,
                        % error_code, high_watermark, last_stable_offset, log_start_offset
                        [
                            <<0, 0>>,
                            <<0, 0, 0, 0, 0, 0, 0, 2>>,
                            <<0, 0, 0, 0, 0, 0, 0, 2>>,
                            <<0, 0, 0, 0, 0, 0, 0, 0>>
                        ],
                        % aborted_transactions, count = 0
                        <<0, 0, 0, 0>>,
                        % preferred_read_replica
                        <<255, 255, 255, 255>>,
                        % records
                        % length in bytes
                        <<0, 0, 2, 100>>,
                        <<0, 0, 0, 0, 0, 0, 0, 0>>,
                        % length, again
                        <<0, 0, 2, 88>>,
                        <<0, 0, 0, 0>>,
                        % magic
                        <<2>>,
                        % CRC
                        <<143, 131, 198, 19>>,
                        % Attributes
                        <<0, 0>>,
                        % Last offset delta, timestamps, producer id, producer epoch, base sequence
                        [
                            <<0, 0, 0, 1>>,
                            <<0, 0, 1, 140, 124, 109, 68, 88>>,
                            <<0, 0, 1, 140, 124, 109, 68, 88>>,
                            <<255, 255, 255, 255, 255, 255, 255, 255>>,
                            <<255, 255>>,
                            <<255, 255, 255, 255>>
                        ],
                        % count
                        <<0, 0, 0, 2>>,
                        [
                            % length (varint)
                            <<162, 4>>,
                            % attributes, timestamp delta, offset delta
                            [<<0>>, <<0>>, <<0>>],

                            % Key
                            [
                                <<40>>,
                                <<110, 81, 83, 48, 74, 115, 88, 72, 83, 65, 48, 75, 48, 85, 71, 47,
                                    104, 99, 56, 65>>
                            ],
                            % Value
                            [
                                <<224, 3>>,
                                <<118, 56, 108, 75, 113, 111, 70, 75, 98, 106, 49, 73, 98, 102, 119,
                                    50, 48, 43, 99, 55, 84, 88, 110, 85, 52, 102, 56, 118, 88, 74,
                                    73, 115, 108, 78, 107, 54, 57, 71, 97, 50, 75, 120, 108, 81, 75,
                                    65, 74, 102, 66, 98, 99, 110, 67, 97, 73, 109, 55, 103, 88, 65,
                                    109, 88, 112, 105, 48, 70, 121, 78, 74, 73, 118, 81, 79, 86, 70,
                                    102, 109, 99, 48, 47, 83, 101, 121, 83, 115, 116, 76, 117, 82,
                                    50, 80, 115, 105, 108, 72, 55, 50, 48, 122, 103, 73, 87, 122,
                                    107, 119, 72, 49, 103, 107, 54, 104, 52, 117, 102, 43, 71, 86,
                                    99, 116, 121, 106, 56, 57, 122, 65, 53, 79, 112, 110, 85, 79,
                                    103, 69, 99, 105, 43, 49, 115, 85, 77, 108, 49, 87, 74, 114, 87,
                                    50, 113, 100, 52, 76, 56, 104, 71, 48, 86, 71, 76, 100, 66, 89,
                                    73, 73, 118, 57, 78, 104, 100, 68, 99, 87, 80, 55, 84, 77, 52,
                                    43, 76, 97, 78, 50, 97, 112, 88, 109, 47, 50, 107, 119, 106, 98,
                                    71, 121, 100, 43, 83, 71, 76, 77, 99, 67, 56, 112, 65, 99, 86,
                                    86, 80, 66, 49, 75, 49, 85, 113, 112, 120, 68, 52, 55, 72, 68,
                                    77, 105, 88, 72, 79, 81, 106, 83, 120, 56, 111, 102, 105, 86,
                                    86, 76, 116, 89, 119>>
                            ],

                            % Headers, count 1
                            [
                                <<2>>,
                                [<<6>>, <<115, 101, 113>>],
                                [<<2>>, <<53>>]
                            ],

                            % length
                            <<164, 4>>,
                            % attributes, deltas
                            <<0, 0, 2>>,
                            % Key
                            [
                                <<40, 121, 86, 82, 51, 119, 69, 122, 72, 74, 47, 98, 118, 97, 86,
                                    82, 99, 43, 116, 72, 50>>
                            ],
                            % Value
                            [
                                <<224, 3>>,
                                <<76, 99, 86, 51, 102, 83, 103, 71, 119, 50, 74, 111, 105, 116, 83,
                                    118, 120, 76, 114, 77, 51, 72, 71, 122, 65, 76, 103, 115, 113,
                                    81, 88, 51, 69, 108, 111, 74, 102, 112, 115, 51, 101, 110, 107,
                                    119, 50, 114, 47, 88, 112, 116, 78, 82, 50, 109, 52, 81, 79, 67,
                                    103, 101, 113, 80, 85, 101, 98, 52, 55, 57, 119, 90, 117, 85,
                                    85, 86, 119, 48, 43, 119, 113, 108, 71, 122, 110, 56, 89, 107,
                                    43, 87, 102, 87, 90, 84, 89, 121, 82, 113, 65, 120, 111, 107,
                                    100, 103, 67, 105, 48, 102, 73, 53, 55, 112, 52, 74, 87, 121,
                                    113, 115, 84, 110, 101, 122, 102, 109, 87, 115, 89, 69, 55, 122,
                                    115, 89, 113, 71, 115, 99, 119, 90, 55, 109, 67, 109, 57, 52,
                                    89, 52, 84, 43, 107, 97, 121, 102, 111, 98, 68, 54, 76, 101, 68,
                                    67, 82, 87, 115, 50, 117, 47, 77, 97, 103, 101, 103, 110, 86,
                                    107, 67, 49, 103, 76, 86, 51, 71, 66, 80, 89, 47, 113, 67, 102,
                                    72, 83, 87, 69, 67, 88, 74, 77, 49, 110, 56, 100, 82, 99, 106,
                                    66, 114, 74, 98, 52, 81, 53, 106, 56, 86, 116, 48, 70, 54, 72,
                                    99, 105, 115, 47, 80, 49, 120, 101, 67, 119, 68, 119, 54, 81,
                                    103, 72, 119, 68, 112, 82, 97, 110, 75, 56>>
                            ],

                            % Headers, count 1
                            [<<2, 6, 115, 101, 113, 4, 50, 56>>]
                        ],

                        [
                            % partition_index = 8, with a single record; followed by extra content.
                            <<0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0,
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 1, 156,
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 68, 0, 0, 0, 0, 2, 109, 123, 96,
                                176, 0, 0, 0, 0, 0, 0, 0, 0, 1, 140, 124, 108, 199, 3, 0, 0, 1, 140,
                                124, 108, 199, 3, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                                255, 255, 255, 255, 0, 0, 0, 1, 162, 4, 0, 0, 0, 40, 106, 84, 80,
                                55, 101, 118, 97, 51, 110, 75, 76, 78, 107, 74, 105, 76, 56, 119,
                                111, 122, 224, 3, 70, 84, 67, 67, 56, 65, 70, 77, 85, 49, 87, 100,
                                115, 97, 106, 122, 99, 80, 88, 108, 104, 67, 97, 119, 105, 99, 50,
                                104, 101, 98, 57, 98, 103, 76, 116, 77, 99, 103, 98, 80, 86, 53,
                                103, 121, 99, 82, 73, 98, 99, 84, 112, 53, 118, 66, 71, 66, 52, 86,
                                122, 90, 102, 74, 116, 116, 97, 109, 71, 110, 69, 118, 120, 84, 65,
                                73, 86, 122, 85, 111, 73, 102, 52, 87, 56, 114, 83, 98, 53, 86, 50,
                                75, 52, 106, 86, 117, 81, 83, 51, 97, 116, 56, 109, 89, 69, 102, 53,
                                84, 102, 75, 65, 103, 51, 55, 79, 115, 53, 55, 113, 115, 79, 89,
                                122, 98, 54, 70, 86, 122, 102, 47, 112, 73, 88, 67, 88, 86, 111, 68,
                                97, 68, 102, 83, 121, 80, 85, 122, 119, 109, 52, 121, 89, 73, 87,
                                101, 72, 50, 72, 86, 70, 88, 69, 97, 115, 77, 109, 122, 107, 71, 89,
                                102, 86, 67, 47, 117, 80, 109, 79, 104, 55, 80, 115, 97, 52, 106,
                                100, 104, 122, 88, 110, 81, 82, 83, 55, 103, 83, 84, 109, 43, 72,
                                112, 69, 120, 76, 78, 102, 98, 73, 48, 70, 103, 65, 106, 101, 101,
                                107, 98, 80, 49, 73, 77, 89, 88, 113, 65, 51, 52, 80, 80, 83, 119,
                                104, 73, 79, 71, 111, 116, 79, 69, 49, 81, 86, 98, 2, 6, 115, 101,
                                113, 2, 49>>,

                            % Truncated Content
                            <<0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 4, 128, 0, 0, 0, 0, 2, 202, 13, 76, 135,
                                0, 0, 0, 0, 0, 3, 0, 0, 1, 140, 124, 109, 68, 88, 0, 0, 1, 140, 124,
                                109, 68, 88, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                                255, 255, 255, 0, 0, 0, 4, 162, 4, 0, 0, 0, 40, 74, 75, 70, 110, 70,
                                122, 47, 99, 74>>
                        ]
                    ]
                ],

                [
                    % partition_index = 5
                    <<0, 0, 0, 5>>,
                    <<0, 0>>,
                    <<0, 0, 0, 0, 0, 0, 0, 2>>,
                    <<0, 0, 0, 0, 0, 0, 0, 2>>,
                    <<0, 0, 0, 0, 0, 0, 0, 0>>,
                    <<0, 0, 0, 0>>,
                    <<255, 255, 255, 255>>,
                    <<0, 0, 0, 0>>
                ]
            ]
        ]
    ),

    ?assertMatch(
        {
            #{
                responses :=
                    [
                        #{
                            topic := <<"cars">>,
                            partitions :=
                                [
                                    #{partition_index := 2, records := [#{records := [_, _]}]},
                                    #{partition_index := 8, records := [#{records := [_]}]},
                                    % An empty fetch (see below) returns a partition with an empty message set, as here:
                                    #{partition_index := 5, records := []}
                                ]
                        }
                    ]
            },
            <<>>
        },
        fetch_response:decode_fetch_response_11(Capture)
    ).

v11_truncated_batch_test() ->
    % This is specifically the extra, truncated, data from above.
    Capture =
        <<0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 4, 128, 0, 0, 0, 0, 2, 202, 13, 76, 135, 0, 0, 0, 0, 0, 3,
            0, 0, 1, 140, 124, 109, 68, 88, 0, 0, 1, 140, 124, 109, 68, 88, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 4, 162, 4, 0, 0, 0, 40, 74, 75,
            70, 110, 70, 122, 47, 99, 74>>,
    ?assertEqual(truncated, kafcod_record_batch:decode_record_batch(Capture)).

v11_empty_response_test() ->
    % In order to check that the truncated data, above, looks right, we need to know what "right" looks like.
    % This test is a captured fetch response with no records.
    Capture =
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 4, 99, 97, 116, 115, 0, 0, 0, 4,
            0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0,
            0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0,
            0, 0>>,
    ?assertEqual(
        {
            #{
                session_id => 0,
                correlation_id => 1,
                responses =>
                    [
                        #{
                            partitions =>
                                [
                                    #{
                                        high_watermark => 0,
                                        records => [],
                                        aborted_transactions => [],
                                        error_code => 0,
                                        last_stable_offset => 0,
                                        partition_index => 3,
                                        log_start_offset => 0,
                                        preferred_read_replica => -1
                                    },
                                    #{
                                        high_watermark => 0,
                                        records => [],
                                        aborted_transactions => [],
                                        error_code => 0,
                                        last_stable_offset => 0,
                                        partition_index => 6,
                                        log_start_offset => 0,
                                        preferred_read_replica => -1
                                    },
                                    #{
                                        high_watermark => 0,
                                        records => [],
                                        aborted_transactions => [],
                                        error_code => 0,
                                        last_stable_offset => 0,
                                        partition_index => 9,
                                        log_start_offset => 0,
                                        preferred_read_replica => -1
                                    },
                                    #{
                                        high_watermark => 0,
                                        records => [],
                                        aborted_transactions => [],
                                        error_code => 0,
                                        last_stable_offset => 0,
                                        partition_index => 0,
                                        log_start_offset => 0,
                                        preferred_read_replica => -1
                                    }
                                ],
                            topic => <<"cats">>
                        }
                    ],
                error_code => 0,
                throttle_time_ms => 0
            },
            <<>>
        },
        fetch_response:decode_fetch_response_11(Capture)
    ).

v11_single_record_test() ->
    <<Length:32/big-signed, Capture:Length/binary>> =
        <<0, 0, 0, 190, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 48, 100, 105, 114,
            101, 99, 116, 95, 112, 114, 111, 100, 117, 99, 101, 95, 83, 85, 73, 84, 69, 95, 100,
            105, 114, 101, 99, 116, 95, 112, 114, 111, 100, 117, 99, 101, 95, 111, 110, 101, 95, 84,
            104, 115, 77, 118, 122, 78, 54, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0,
            76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 2, 227, 16, 146, 111, 0, 0, 0, 0,
            0, 0, 0, 0, 1, 145, 49, 176, 215, 143, 0, 0, 1, 145, 49, 176, 215, 143, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 1, 28, 0, 0, 0, 6, 107,
            101, 121, 10, 118, 97, 108, 117, 101, 0>>,
    ?assertEqual(
        {
            #{
                correlation_id => 3,
                throttle_time_ms => 0,
                error_code => 0,
                responses =>
                    [
                        #{
                            partitions =>
                                [
                                    #{
                                        high_watermark => 1,
                                        error_code => 0,
                                        partition_index => 0,
                                        log_start_offset => 0,
                                        records =>
                                            [
                                                #{
                                                    attributes => #{compression => none},
                                                    producer_id => -1,
                                                    producer_epoch => -1,
                                                    % Unix epoch, milliseconds; 2024-08-08T11:12:38.799Z
                                                    max_timestamp => 1723115558799,
                                                    records => [
                                                        #{
                                                            attributes => 0,
                                                            value => <<"value">>,
                                                            key => <<"key">>,
                                                            headers => [],
                                                            offset_delta => 0,
                                                            timestamp_delta => 0
                                                        }
                                                    ],
                                                    base_offset => 0,
                                                    base_sequence => -1,
                                                    % Unix epoch, milliseconds; 2024-08-08T11:12:38.799Z
                                                    base_timestamp => 1723115558799,
                                                    crc => 3809514095,
                                                    last_offset_delta => 0,
                                                    magic => 2,
                                                    partition_leader_epoch => 0
                                                }
                                            ],
                                        last_stable_offset => 1,
                                        aborted_transactions => [],
                                        preferred_read_replica => -1
                                    }
                                ],
                            topic => <<"direct_produce_SUITE_direct_produce_one_ThsMvzN6">>
                        }
                    ],
                session_id => 0
            },
            <<>>
        },
        fetch_response:decode_fetch_response_11(Capture)
    ),
    ok.

v11_single_record_eof_test() ->
    <<Length:32/big-signed, Capture:Length/binary>> =
        <<0, 0, 0, 114, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 48, 100, 105, 114,
            101, 99, 116, 95, 112, 114, 111, 100, 117, 99, 101, 95, 83, 85, 73, 84, 69, 95, 100,
            105, 114, 101, 99, 116, 95, 112, 114, 111, 100, 117, 99, 101, 95, 111, 110, 101, 95, 84,
            104, 115, 77, 118, 122, 78, 54, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0,
            0>>,
    ?assertEqual(
        {
            #{
                correlation_id => 4,
                throttle_time_ms => 0,
                error_code => 0,
                responses =>
                    [
                        #{
                            partitions =>
                                [
                                    #{
                                        high_watermark => 1,
                                        error_code => 0,
                                        partition_index => 0,
                                        log_start_offset => 0,
                                        records => [],
                                        last_stable_offset => 1,
                                        aborted_transactions => [],
                                        preferred_read_replica => -1
                                    }
                                ],
                            topic => <<"direct_produce_SUITE_direct_produce_one_ThsMvzN6">>
                        }
                    ],
                session_id => 0
            },
            <<>>
        },
        fetch_response:decode_fetch_response_11(Capture)
    ),
    ok.

v11_encode_test() ->
    Topic = <<"topic-name">>,
    PartitionIndex = 0,
    FetchOffset = 0,
    BaseTimestamp = MaxTimestamp = 0,

    FetchResponse = #{
        correlation_id => ?CORRELATION_ID,
        error_code => ?NONE,
        throttle_time_ms => 0,
        session_id => -1,
        responses => [
            #{
                topic => Topic,
                partitions => [
                    #{
                        partition_index => PartitionIndex,
                        error_code => ?NONE,
                        high_watermark => 3,
                        last_stable_offset => 3,
                        log_start_offset => 0,
                        aborted_transactions => null,
                        preferred_read_replica => -1,
                        records => [
                            #{
                                attributes => #{compression => none},
                                base_offset => FetchOffset,
                                base_sequence => -1,
                                base_timestamp => BaseTimestamp,
                                crc => -1,
                                last_offset_delta => 2,
                                magic => 2,
                                max_timestamp => MaxTimestamp,
                                partition_leader_epoch => 0,
                                producer_id => -1,
                                producer_epoch => -1,
                                records => [
                                    #{
                                        attributes => 0,
                                        headers => [],
                                        key => <<"key-1">>,
                                        offset_delta => 0,
                                        timestamp_delta => 0,
                                        value => <<"value-1">>
                                    },
                                    #{
                                        attributes => 0,
                                        headers => [],
                                        key => <<"key-2">>,
                                        offset_delta => 1,
                                        timestamp_delta => 0,
                                        value => <<"value-2">>
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    },

    % While working on the mock broker in kafine, I was missing fields encoding a fetch response, so I added this test.
    % I don't (yet) particularly care what the result is.
    ?assertMatch(
        <<_DontCare/binary>>,
        iolist_to_binary(fetch_response:encode_fetch_response_11(FetchResponse))
    ),
    ok.

v11_test() ->
    % The v11 and v12 tests have captured the same records, but v12 uses compact records.
    Capture =
        <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 4, 99, 97, 114, 115, 0, 0, 0, 3,
            0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0, 106, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 94, 0,
            0, 0, 0, 2, 65, 131, 173, 248, 0, 0, 0, 0, 0, 2, 0, 0, 1, 146, 51, 81, 95, 143, 0, 0, 1,
            146, 51, 81, 95, 143, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 0, 0, 0, 3, 28, 0, 0, 0, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0, 28, 0, 0,
            2, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0, 28, 0, 0, 4, 6, 107, 101, 121, 10,
            118, 97, 108, 117, 101, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 5, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            255, 255, 255, 255, 0, 0, 0, 0>>,
    ?assertEqual(
        v11_v12_expected(),
        fetch_response:decode_fetch_response_11(Capture)
    ),
    ok.

v12_test() ->
    % v12 adds compact records; we captured the same records as in v11 test, above.
    Capture =
        <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 5, 99, 97, 114, 115, 4, 0, 0, 0, 2, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 255, 255,
            255, 255, 107, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 94, 0, 0, 0, 0, 2, 65, 131, 173, 248, 0,
            0, 0, 0, 0, 2, 0, 0, 1, 146, 51, 81, 95, 143, 0, 0, 1, 146, 51, 81, 95, 143, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 3, 28, 0, 0, 0, 6,
            107, 101, 121, 10, 118, 97, 108, 117, 101, 0, 28, 0, 0, 2, 6, 107, 101, 121, 10, 118,
            97, 108, 117, 101, 0, 28, 0, 0, 4, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0, 0,
            0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 255, 255, 255, 255, 1, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 255, 255, 255, 255, 1, 0, 0, 0>>,
    ?assertEqual(v11_v12_expected(), fetch_response:decode_fetch_response_12(Capture)),
    ok.

v11_v12_expected() ->
    {
        #{
            session_id => 0,
            correlation_id => 2,
            error_code => 0,
            throttle_time_ms => 0,
            responses =>
                [
                    #{
                        topic => <<"cars">>,
                        partitions =>
                            [
                                #{
                                    high_watermark => 3,
                                    error_code => 0,
                                    partition_index => 2,
                                    records =>
                                        [
                                            #{
                                                attributes => #{compression => none},
                                                max_timestamp => 1727437823887,
                                                producer_id => -1,
                                                producer_epoch => -1,
                                                records =>
                                                    [
                                                        #{
                                                            attributes => 0,
                                                            value => <<"value">>,
                                                            key => <<"key">>,
                                                            headers => [],
                                                            timestamp_delta => 0,
                                                            offset_delta => 0
                                                        },
                                                        #{
                                                            attributes => 0,
                                                            value => <<"value">>,
                                                            key => <<"key">>,
                                                            headers => [],
                                                            timestamp_delta => 0,
                                                            offset_delta => 1
                                                        },
                                                        #{
                                                            attributes => 0,
                                                            value => <<"value">>,
                                                            key => <<"key">>,
                                                            headers => [],
                                                            timestamp_delta => 0,
                                                            offset_delta => 2
                                                        }
                                                    ],
                                                base_offset => 0,
                                                partition_leader_epoch => 0,
                                                magic => 2,
                                                crc => 1099148792,
                                                last_offset_delta => 2,
                                                base_timestamp => 1727437823887,
                                                base_sequence => -1
                                            }
                                        ],
                                    last_stable_offset => 3,
                                    aborted_transactions => [],
                                    log_start_offset => 0,
                                    preferred_read_replica => -1
                                },
                                #{
                                    high_watermark => 0,
                                    error_code => 0,
                                    partition_index => 8,
                                    records => [],
                                    last_stable_offset => 0,
                                    aborted_transactions => [],
                                    log_start_offset => 0,
                                    preferred_read_replica => -1
                                },
                                #{
                                    high_watermark => 0,
                                    error_code => 0,
                                    partition_index => 5,
                                    records => [],
                                    last_stable_offset => 0,
                                    aborted_transactions => [],
                                    log_start_offset => 0,
                                    preferred_read_replica => -1
                                }
                            ]
                    }
                ]
        },
        <<>>
    }.
