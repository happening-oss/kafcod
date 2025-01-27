-module(offset_fetch_response_tests).
-include_lib("eunit/include/eunit.hrl").

v8_unknown_test() ->
    Capture =
        <<0, 0, 0, 7, 0, 0, 0, 0, 0, 2, 8, 103, 114, 111, 117, 112, 50, 50, 2, 5, 99, 97, 114, 115,
            6, 0, 0, 0, 7, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0, 0, 0,
            0, 0, 0, 6, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0, 0, 0, 0,
            0, 0, 9, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0, 0, 0, 0, 0,
            0, 8, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0, 0, 0, 0, 0, 0,
            5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0, 0, 0, 0, 0, 0, 0,
            0>>,
    ?assertEqual(
        {
            #{
                correlation_id => 7,
                throttle_time_ms => 0,
                groups =>
                    [
                        #{
                            group_id => <<"group22">>,
                            error_code => 0,
                            topics =>
                                [
                                    #{
                                        name => <<"cars">>,
                                        partitions =>
                                            [
                                                #{
                                                    metadata => <<>>,
                                                    error_code => 0,
                                                    partition_index => 7,
                                                    committed_offset => -1,
                                                    committed_leader_epoch => -1
                                                },
                                                #{
                                                    metadata => <<>>,
                                                    error_code => 0,
                                                    partition_index => 6,
                                                    committed_offset => -1,
                                                    committed_leader_epoch => -1
                                                },
                                                #{
                                                    metadata => <<>>,
                                                    error_code => 0,
                                                    partition_index => 9,
                                                    committed_offset => -1,
                                                    committed_leader_epoch => -1
                                                },
                                                #{
                                                    metadata => <<>>,
                                                    error_code => 0,
                                                    partition_index => 8,
                                                    committed_offset => -1,
                                                    committed_leader_epoch => -1
                                                },
                                                #{
                                                    metadata => <<>>,
                                                    error_code => 0,
                                                    partition_index => 5,
                                                    committed_offset => -1,
                                                    committed_leader_epoch => -1
                                                }
                                            ]
                                    }
                                ]
                        }
                    ]
            },
            <<>>
        },
        offset_fetch_response:decode_offset_fetch_response_8(Capture)
    ).
