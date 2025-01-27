-module(kafcod_records_tests).
-include_lib("eunit/include/eunit.hrl").

record_batches_test_() ->
    EncodedRecordBatches = iolist_to_binary([
        % First record batch, exploded.
        [
            <<0, 0, 0, 0, 0, 0, 0, 0>>,
            <<0, 0, 0, 61>>,
            <<0, 0, 0, 0>>,
            <<2>>,
            <<83, 104, 36, 131>>,
            <<0, 0>>,
            <<0, 0, 0, 0>>,
            <<0, 0, 1, 136, 191, 12, 70, 86>>,
            <<0, 0, 1, 136, 191, 12, 70, 86>>,
            <<255, 255, 255, 255, 255, 255, 255, 255>>,
            <<255, 255>>,
            <<255, 255, 255, 255>>,
            [
                <<0, 0, 0, 1>>,
                [
                    <<22, 0, 0, 0, 1, 10, 104, 101, 108, 108, 111, 0>>
                ]
            ]
        ],
        % Second record batch
        <<0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 58, 0, 0, 0, 0, 2, 87, 254, 198, 51, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 136, 191, 34, 136, 116, 0, 0, 1, 136, 191, 34, 136, 116, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 1, 16, 0, 0, 0, 1, 4, 49, 49, 0>>
    ]),
    DecodedRecordBatches = [
        #{
            attributes => #{compression => none},
            base_offset => 0,
            base_sequence => -1,
            base_timestamp => 1686832432726,
            crc => 1399334019,
            last_offset_delta => 0,
            magic => 2,
            max_timestamp => 1686832432726,
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
            base_timestamp => 1686833891444,
            crc => 1476314675,
            last_offset_delta => 0,
            magic => 2,
            max_timestamp => 1686833891444,
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
                    value => <<"11">>
                }
            ]
        }
    ],
    [
        {"decode_record_batches",
            ?_assertEqual(
                DecodedRecordBatches, kafcod_records:decode_record_batches(EncodedRecordBatches)
            )},
        {"encode_record_batches",
            ?_assertEqual(
                EncodedRecordBatches,
                iolist_to_binary(kafcod_records:encode_record_batches(DecodedRecordBatches))
            )}
    ].

record_batches_snappy_test_() ->
    % Captured.
    EncodedRecordBatches = iolist_to_binary([
        [
            [
                <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63>>,
                [
                    <<0, 0, 0, 0, 2>>,
                    <<187, 58, 252, 207>>,
                    [
                        [
                            <<0, 2, 0, 0, 0, 0, 0, 0, 1, 136, 191, 12, 70, 86, 0, 0, 1, 136, 191,
                                12, 70, 86, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                                255, 255, 255>>
                        ],
                        [<<0, 0, 0, 1>>, <<12, 44, 22, 0, 0, 0, 1, 10, 104, 101, 108, 108, 111, 0>>]
                    ]
                ]
            ],
            [
                <<0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 60>>,
                [
                    <<0, 0, 0, 0, 2>>,
                    <<26, 179, 200, 217>>,
                    [
                        [
                            <<0, 2, 0, 0, 0, 0, 0, 0, 1, 136, 191, 34, 136, 116, 0, 0, 1, 136, 191,
                                34, 136, 116, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                                255, 255, 255>>
                        ],
                        [<<0, 0, 0, 1>>, <<9, 32, 16, 0, 0, 0, 1, 4, 49, 49, 0>>]
                    ]
                ]
            ]
        ]
    ]),
    DecodedRecordBatches = [
        #{
            attributes => #{compression => snappy},
            base_offset => 0,
            base_sequence => -1,
            base_timestamp => 1686832432726,
            crc => 3141205199,
            last_offset_delta => 0,
            magic => 2,
            max_timestamp => 1686832432726,
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
            attributes => #{compression => snappy},
            base_offset => 1,
            base_sequence => -1,
            base_timestamp => 1686833891444,
            crc => 447989977,
            last_offset_delta => 0,
            magic => 2,
            max_timestamp => 1686833891444,
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
                    value => <<"11">>
                }
            ]
        }
    ],
    [
        {"decode_record_batches",
            ?_assertEqual(
                DecodedRecordBatches, kafcod_records:decode_record_batches(EncodedRecordBatches)
            )},
        {"encode_record_batches",
            ?_assertEqual(
                EncodedRecordBatches,
                iolist_to_binary(kafcod_records:encode_record_batches(DecodedRecordBatches))
            )}
    ].

v11_v12_comparison_test_() ->
    Capture_11 = iolist_to_binary(
        [
            % Length; int32
            <<0, 0, 0, 106>>,
            % Everything else is the same.
            <<0, 0, 0, 0, 0, 0, 0, 0>>,
            <<0, 0, 0, 94>>,
            <<0, 0, 0, 0>>,
            <<2>>,
            <<65, 131, 173, 248>>,
            <<0, 0>>,
            <<0, 0, 0, 2>>,
            <<0, 0, 1, 146, 51, 81, 95, 143>>,
            <<0, 0, 1, 146, 51, 81, 95, 143>>,
            <<255, 255, 255, 255, 255, 255, 255, 255>>,
            <<255, 255>>,
            <<255, 255, 255, 255>>,
            [
                <<0, 0, 0, 3>>,
                [
                    <<28, 0, 0, 0, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0>>,
                    <<28, 0, 0, 2, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0>>,
                    <<28, 0, 0, 4, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0>>
                ]
            ]
        ]
    ),
    Capture_12 = iolist_to_binary(
        [
            % Length; varint. This appears to be the only difference between the two captures.
            <<107>>,
            % Offset
            <<0, 0, 0, 0, 0, 0, 0, 0>>,
            % Message size
            <<0, 0, 0, 94>>,
            % Leader epoch
            <<0, 0, 0, 0>>,
            % Magic
            <<2>>,
            % CRC
            <<65, 131, 173, 248>>,
            % Attributes
            <<0, 0>>,
            % Last offset delta
            <<0, 0, 0, 2>>,
            % First, Last Timestamp
            <<0, 0, 1, 146, 51, 81, 95, 143>>,
            <<0, 0, 1, 146, 51, 81, 95, 143>>,
            % Producer ID, epoch
            <<255, 255, 255, 255, 255, 255, 255, 255>>,
            <<255, 255>>,
            % Base sequence
            <<255, 255, 255, 255>>,
            % Records
            [
                <<0, 0, 0, 3>>,
                [
                    <<28, 0, 0, 0, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0>>,
                    <<28, 0, 0, 2, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0>>,
                    <<28, 0, 0, 4, 6, 107, 101, 121, 10, 118, 97, 108, 117, 101, 0>>
                ]
            ]
        ]
    ),
    Records = [
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
    [
        ?_assertEqual({Records, <<>>}, kafcod_records:decode_records(Capture_11)),
        ?_assertEqual({Records, <<>>}, kafcod_records:decode_compact_records(Capture_12)),

        ?_assertEqual(
            iolist_to_binary(Capture_11),
            iolist_to_binary(kafcod_records:encode_records(Records))
        ),
        ?_assertEqual(
            iolist_to_binary(Capture_12),
            iolist_to_binary(kafcod_records:encode_compact_records(Records))
        )
    ].
