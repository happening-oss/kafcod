-module(find_coordinator_response_tests).
-include_lib("eunit/include/eunit.hrl").

v3_decode_response_error_none_test() ->
    Capture =
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 5, 78, 79, 78, 69, 0, 0, 0, 103, 15, 49, 57, 50, 46, 49,
            54, 56, 46, 53, 56, 46, 49, 51, 54, 0, 0, 35, 134, 0>>,
    ?assertEqual(
        {
            #{
                port => 9094,
                host => <<"192.168.58.136">>,
                correlation_id => 1,
                throttle_time_ms => 0,
                error_code => 0,
                error_message => <<"NONE">>,
                node_id => 103
            },
            <<>>
        },
        find_coordinator_response:decode_find_coordinator_response_3(Capture)
    ).

v3_decode_response_coordinator_not_available_test() ->
    Capture =
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 15, 34, 84, 104, 101, 32, 99, 111, 111, 114, 100, 105, 110,
            97, 116, 111, 114, 32, 105, 115, 32, 110, 111, 116, 32, 97, 118, 97, 105, 108, 97, 98,
            108, 101, 46, 255, 255, 255, 255, 1, 255, 255, 255, 255, 0>>,
    ?assertEqual(
        {
            #{
                correlation_id => 1,
                port => -1,
                host => <<>>,
                node_id => -1,
                error_code => 15,
                throttle_time_ms => 0,
                error_message => <<"The coordinator is not available.">>
            },
            <<>>
        },
        find_coordinator_response:decode_find_coordinator_response_3(Capture)
    ).
