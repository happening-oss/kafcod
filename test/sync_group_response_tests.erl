-module(sync_group_response_tests).
-include_lib("eunit/include/eunit.hrl").

empty_assignment_test() ->
    % Captured by running two instances of kcat on a topic with only a single partition. One of them gets an empty assignment.
    Capture =
        <<0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    ?assertEqual(
        {
            #{
                correlation_id => 5,
                throttle_time_ms => 0,
                error_code => 0,
                assignment => <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>
            },
            <<>>
        },
        sync_group_response:decode_sync_group_response_3(Capture)
    ),
    ok.

single_partition_test() ->
    % Same capture; this is the other guy.
    Capture =
        <<0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 0, 0, 1, 0, 10, 104, 105, 103, 104,
            108, 97, 110, 100, 101, 114, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0>>,
    ?assertEqual(
        {
            #{
                correlation_id => 6,
                throttle_time_ms => 0,
                error_code => 0,
                assignment =>
                    <<0, 0, 0, 0, 0, 1, 0, 10, 104, 105, 103, 104, 108, 97, 110, 100, 101, 114, 0,
                        0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0>>
            },
            <<>>
        },
        sync_group_response:decode_sync_group_response_3(Capture)
    ),
    ok.

missing_assignment_test() ->
    Capture =
        <<0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 9, 99, 111, 110, 115, 117, 109, 101, 114, 5, 110, 117,
            108, 108, 1, 0>>,
    ?assertEqual(
        {
            #{
                correlation_id => 3,
                error_code => 0,
                throttle_time_ms => 0,
                protocol_name => <<"null">>,
                protocol_type => <<"consumer">>,
                assignment => <<>>
            },
            <<>>
        },
        sync_group_response:decode_sync_group_response_5(Capture)
    ),
    ok.
