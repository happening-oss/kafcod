-module(kafcod_message_set_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TIMESTAMP, 1_234_567_890).
% 64 characters, enough that a length prefix varint will be two bytes
-define(LONG_BINARY, <<"1234567890123456789012345678901234567890123456789012345678901234">>).

prepare_message_set_test() ->
    % The input messages are ordered. Note the numbers.
    Messages = [
        #{
            timestamp => ?TIMESTAMP,
            key => <<"key1">>,
            value => <<"value1">>,
            headers => [{<<"h">>, <<"1">>}]
        },
        #{
            timestamp => ?TIMESTAMP + 1,
            key => <<"key2">>,
            value => <<"value2">>,
            headers => [{<<"h">>, <<"2">>}]
        }
    ],

    ?assertMatch(
        [
            #{
                attributes := #{compression := none},
                % The records must be in the original order. The offset deltas must be strictly ascending.
                records := [
                    #{
                        attributes := 0,
                        value := <<"value1">>,
                        key := <<"key1">>,
                        timestamp_delta := 0,
                        offset_delta := 0,
                        headers := [{<<"h">>, <<"1">>}]
                    },
                    #{
                        attributes := 0,
                        value := <<"value2">>,
                        key := <<"key2">>,
                        timestamp_delta := 1,
                        offset_delta := 1,
                        headers := [{<<"h">>, <<"2">>}]
                    }
                ],
                base_offset := 0,
                base_sequence := _,
                base_timestamp := ?TIMESTAMP,
                % Must be the same as the offset_delta of the last record.
                last_offset_delta := 1,
                magic := 2,
                max_timestamp := ?TIMESTAMP + 1,
                partition_leader_epoch := _,
                producer_epoch := _,
                producer_id := _
            }
        ],
        kafcod_message_set:prepare_message_set(#{compression => none}, Messages)
    ),
    ok.

prepare_message_set_all_nulls_test() ->
    Messages = [
        #{timestamp => ?TIMESTAMP, key => null, value => null, headers => []}
    ],

    ?assertMatch(
        [
            #{
                attributes := #{compression := none},
                records := [
                    #{
                        attributes := 0,
                        value := null,
                        key := null,
                        timestamp_delta := 0,
                        offset_delta := 0,
                        headers := []
                    }
                ],
                base_offset := 0,
                base_sequence := _,
                base_timestamp := ?TIMESTAMP,
                % Must be the same as the offset_delta of the last record.
                last_offset_delta := 0,
                magic := 2,
                max_timestamp := ?TIMESTAMP,
                partition_leader_epoch := _,
                producer_epoch := _,
                producer_id := _
            }
        ],
        kafcod_message_set:prepare_message_set(#{compression => none}, Messages)
    ),
    ok.

uncompressed_size_test_() ->
    [
        ?_assertEqual(61, kafcod_message_set:uncompressed_size([])),
        ?_assertEqual(
            % 61 batch header, 1 record len, 3 record header, 1 key len, 1 value len, 1 header count
            61 + 1 + 3 + 1 + 1 + 1,
            kafcod_message_set:uncompressed_size([
                #{timestamp => ?TIMESTAMP, key => null, value => null, headers => []}
            ])
        ),
        ?_assertEqual(
            61 + (1 + 3 + 1 + 1 + 1) * 2,
            kafcod_message_set:uncompressed_size([
                #{timestamp => ?TIMESTAMP, key => null, value => null, headers => []},
                #{timestamp => ?TIMESTAMP, key => null, value => null, headers => []}
            ])
        ),
        ?_assertEqual(
            61 + 1 + 3 + 1 + 8 + 1 + 1,
            kafcod_message_set:uncompressed_size([
                #{timestamp => ?TIMESTAMP, key => <<"test key">>, value => null, headers => []}
            ])
        ),
        ?_assertEqual(
            % Both the total record size and the key size now require two bytes
            61 + 2 + 3 + 2 + 64 + 1 + 1,
            kafcod_message_set:uncompressed_size([
                #{timestamp => ?TIMESTAMP, key => ?LONG_BINARY, value => null, headers => []}
            ])
        ),
        ?_assertEqual(
            61 + 1 + 3 + 1 + 1 + 10 + 1,
            kafcod_message_set:uncompressed_size([
                #{timestamp => ?TIMESTAMP, key => null, value => <<"test value">>, headers => []}
            ])
        ),
        ?_assertEqual(
            % Both the total record size and the value size now require two bytes
            61 + 2 + 3 + 1 + 2 + 64 + 1,
            kafcod_message_set:uncompressed_size([
                #{timestamp => ?TIMESTAMP, key => null, value => ?LONG_BINARY, headers => []}
            ])
        ),
        ?_assertEqual(
            % + 1 header key len, 3 header key, 1 header value len
            61 + 1 + 3 + 1 + 1 + 1 + 1 + 3 + 1,
            kafcod_message_set:uncompressed_size([
                #{
                    timestamp => ?TIMESTAMP,
                    key => null,
                    value => null,
                    headers => [{<<"key">>, null}]
                }
            ])
        ),
        ?_assertEqual(
            % + 5 header value len
            61 + 1 + 3 + 1 + 1 + 1 + 1 + 3 + 1 + 5,
            kafcod_message_set:uncompressed_size([
                #{
                    timestamp => ?TIMESTAMP,
                    key => null,
                    value => null,
                    headers => [{<<"key">>, <<"value">>}]
                }
            ])
        ),
        ?_assertEqual(
            61 + 1 + 3 + 1 + 1 + 1 + (1 + 4 + 1) * 2,
            kafcod_message_set:uncompressed_size([
                #{
                    timestamp => ?TIMESTAMP,
                    key => null,
                    value => null,
                    headers => [
                        {<<"key1">>, null},
                        {<<"key2">>, null}
                    ]
                }
            ])
        ),
        ?_assertEqual(
            61 + 2 + 1 + 1 + 1 + 1 + 1 + 2 + (1 + 1 + 1) * 64,
            kafcod_message_set:uncompressed_size([
                #{
                    timestamp => ?TIMESTAMP,
                    key => null,
                    value => null,
                    % Enough headers that header count needs two bytes
                    % Each header is 3 bytes
                    headers => [{<<"k">>, null} || _ <- lists:seq(1, 64)]
                }
            ])
        ),
        ?_assertEqual(
            % record len and header key len are now 2 bytes
            61 + 2 + 3 + 1 + 1 + 1 + 2 + 64 + 1,
            kafcod_message_set:uncompressed_size([
                #{
                    timestamp => ?TIMESTAMP,
                    key => null,
                    value => null,
                    headers => [{?LONG_BINARY, null}]
                }
            ])
        ),
        ?_assertEqual(
            % record len and header value len are now 2 bytes
            61 + 2 + 3 + 1 + 1 + 1 + 1 + 3 + 2 + 64,
            kafcod_message_set:uncompressed_size([
                #{
                    timestamp => ?TIMESTAMP,
                    key => null,
                    value => null,
                    headers => [{<<"key">>, ?LONG_BINARY}]
                }
            ])
        ),
        ?_assertEqual(
            % timestamp delta of 64 takes 2 bytes
            61 + (1 + 3 + 1 + 1 + 1) + (1 + 4 + 1 + 1 + 1),
            kafcod_message_set:uncompressed_size([
                #{timestamp => ?TIMESTAMP, key => null, value => null, headers => []},
                #{timestamp => ?TIMESTAMP + 64, key => null, value => null, headers => []}
            ])
        ),
        ?_assertEqual(
            % 65th record has offset delta of 64, requiring 2 bytes
            61 + (1 + 3 + 1 + 1 + 1) * 64 + (1 + 4 + 1 + 1 + 1),
            kafcod_message_set:uncompressed_size([
                #{timestamp => ?TIMESTAMP, key => null, value => null, headers => []}
             || _ <- lists:seq(1, 65)
            ])
        )
    ].

uncompressed_size_accumulator_test() ->
    Acc0 = kafcod_message_set:uncompressed_size_init(),
    ?assertEqual({61, undefined, 0}, Acc0),

    Acc1 = kafcod_message_set:uncompressed_size_add(
        #{timestamp => ?TIMESTAMP, key => null, value => null, headers => []}, Acc0
    ),
    ?assertEqual({68, ?TIMESTAMP, 1}, Acc1),

    Acc2 = kafcod_message_set:uncompressed_size_add(
        #{timestamp => ?TIMESTAMP + 1, key => <<"key">>, value => null, headers => []}, Acc1
    ),
    ?assertEqual({78, ?TIMESTAMP, 2}, Acc2),

    Acc3 = kafcod_message_set:uncompressed_size_add(
        #{timestamp => ?TIMESTAMP + 1, key => null, value => <<"value">>, headers => []}, Acc2
    ),
    ?assertEqual({90, ?TIMESTAMP, 3}, Acc3),

    Acc4 = kafcod_message_set:uncompressed_size_add(
        #{timestamp => ?TIMESTAMP + 1, key => null, value => null, headers => [{<<"key">>, <<"value">>}]}, Acc3
    ),
    ?assertEqual({107, ?TIMESTAMP, 4}, Acc4),

    % 61 more to just trigger offset delta requiring 2 bytes
    Acc5 = lists:foldl(
        fun(_, Acc) ->
            kafcod_message_set:uncompressed_size_add(
                #{timestamp => ?TIMESTAMP + 1, key => null, value => null, headers => []}, Acc
            )
        end,
        Acc4,
        lists:seq(1, 61)
    ),
    ?assertEqual({107 + (7 * 60) + 8, ?TIMESTAMP, 65}, Acc5).
