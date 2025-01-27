-module(list_offsets_request_tests).
-include_lib("eunit/include/eunit.hrl").

v7_kcat_test() ->
    <<Length:32/big-signed, Capture:Length/binary>> =
        <<0, 0, 0, 49, 0, 2, 0, 7, 0, 0, 0, 2, 0, 7, 114, 100, 107, 97, 102, 107, 97, 0, 255, 255,
            255, 255, 1, 2, 5, 99, 97, 114, 115, 2, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 0, 0, 0>>,
    ?assertEqual(
        {
            #{
                api_key => 2,
                correlation_id => 2,
                client_id => <<"rdkafka">>,
                topics => [
                    #{
                        name => <<"cars">>,
                        partitions => [
                            #{timestamp => -1, partition_index => 0, current_leader_epoch => -1}
                        ]
                    }
                ],
                api_version => 7,
                replica_id => -1,
                isolation_level => 1
            },
            <<>>
        },
        list_offsets_request:decode_list_offsets_request_7(Capture)
    ),
    ok.
