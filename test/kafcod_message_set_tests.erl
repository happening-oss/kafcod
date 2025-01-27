-module(kafcod_message_set_tests).
-include_lib("eunit/include/eunit.hrl").

prepare_message_set_test() ->
    % The input messages are ordered. Note the numbers.
    Messages = [
        #{key => <<"key1">>, value => <<"value1">>, headers => [{<<"h">>, <<"1">>}]},
        #{key => <<"key2">>, value => <<"value2">>, headers => [{<<"h">>, <<"2">>}]}
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
                        timestamp_delta := 0,
                        offset_delta := 1,
                        headers := [{<<"h">>, <<"2">>}]
                    }
                ],
                base_offset := 0,
                base_sequence := _,
                base_timestamp := _,
                % Must be the same as the offset_delta of the last record.
                last_offset_delta := 1,
                magic := 2,
                max_timestamp := _,
                partition_leader_epoch := _,
                producer_epoch := _,
                producer_id := _
            }
        ],
        kafcod_message_set:prepare_message_set(#{compression => none}, Messages)
    ),
    ok.
