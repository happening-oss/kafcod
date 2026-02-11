-module(fetch_response_error_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kafcod/include/error_code.hrl").
-include("catch.hrl").

-define(CORRELATION_ID, 203569230).

% I found this bug while testing something in kamock. Amusingly, eqwalizer spots the problem in the test.
error_test() ->
    CorrelationId = ?CORRELATION_ID,
    FetchResponse = #{
        correlation_id => CorrelationId,
        error_code => ?NONE,
        session_id => -1,
        responses =>
            [
                #{
                    topic => <<"topic">>,
                    partitions => [
                        #{
                            % This should be an int32; we get that error correctly.
                            partition_index => #{},
                            error_code => ?NONE,
                            log_start_offset => 0,
                            high_watermark => 0,
                            last_stable_offset => 0,
                            aborted_transactions => [],
                            preferred_read_replica => -1,
                            % the bug is that it complains about records, here.
                            records => []
                        }
                    ]
                }
            ],
        throttle_time_ms => 0
    },
    {error, Reason = badarg, StackTrace} = ?CATCH(
        fetch_response:encode_fetch_response_11(FetchResponse)
    ),

    ?assertEqual(
        #{
            1 =>
                "expected 'partition_index' to be of type 'int32', but has type 'map', value #{}"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

partition_data_test() ->
    % The underlying problem stems from here:
    Args = #{
        % This should be an int32; we get that error correctly.
        partition_index => #{},
        error_code => ?NONE,
        log_start_offset => 0,
        high_watermark => 0,
        last_stable_offset => 0,
        aborted_transactions => [],
        preferred_read_replica => -1,
        % the bug is that it used to complain about records, here.
        records => []
    },
    Expected = #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_11},
        preferred_read_replica => int32,
        records => records
    },
    {error_info, #{cause := Cause}} = kafcod_errors:create_error_info(Args, Expected),
    ?assertEqual([{wrong_type, partition_index, {expected, int32}, {value, #{}}}], Cause),
    ok.
