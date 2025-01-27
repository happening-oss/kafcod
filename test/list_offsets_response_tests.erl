-module(list_offsets_response_tests).
-include_lib("eunit/include/eunit.hrl").

v7_kcat_empty_test() ->
    <<Length:32/big-signed, Capture:Length/binary>> =
        <<0, 0, 0, 90, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 50, 116, 111, 112, 105, 99, 95, 99, 111, 110,
            115, 117, 109, 101, 114, 95, 112, 97, 114, 105, 116, 121, 95, 83, 85, 73, 84, 69, 95,
            118, 105, 114, 103, 105, 110, 95, 116, 111, 112, 105, 99, 95, 118, 77, 81, 121, 120,
            102, 100, 95, 2, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0>>,

    ?assertEqual(
        {
            #{
                correlation_id => 2,
                throttle_time_ms => 0,
                topics => [
                    #{
                        name => <<"topic_consumer_parity_SUITE_virgin_topic_vMQyxfd_">>,
                        partitions => [
                            #{
                                partition_index => 0,
                                error_code => 0,
                                timestamp => -1,
                                offset => 0,
                                leader_epoch => 19
                            }
                        ]
                    }
                ]
            },
            <<>>
        },
        list_offsets_response:decode_list_offsets_response_7(Capture)
    ),
    ok.

v7_kcat_single_message_test() ->
    <<Length:32/big-signed, Capture:Length/binary>> =
        <<0, 0, 0, 89, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 49, 100, 105, 114, 101, 99, 116, 95, 112, 114,
            111, 100, 117, 99, 101, 95, 83, 85, 73, 84, 69, 95, 100, 105, 114, 101, 99, 116, 95,
            112, 114, 111, 100, 117, 99, 101, 95, 111, 110, 101, 95, 84, 104, 115, 77, 118, 122, 78,
            54, 2, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0>>,
    ?assertEqual(
        {
            #{
                correlation_id => 2,
                throttle_time_ms => 0,
                topics => [
                    #{
                        name => <<"direct_produce_SUITE_direct_produce_one_ThsMvzN6">>,
                        partitions => [
                            #{
                                partition_index => 0,
                                error_code => 0,
                                timestamp => -1,
                                offset => 0,
                                leader_epoch => 0
                            }
                        ]
                    }
                ]
            },
            <<>>
        },
        list_offsets_response:decode_list_offsets_response_7(Capture)
    ),
    ok.
