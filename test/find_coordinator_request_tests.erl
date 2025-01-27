-module(find_coordinator_request_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

v3_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Expected =
        <<0, 10, 0, 3, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45,
            72, 69, 82, 69, 0, 2, 97, 0, 0>>,
    Actual = iolist_to_binary(
        find_coordinator_request:encode_find_coordinator_request_3(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            key_type => 0,
            key => <<"a">>
        })
    ),
    ?assertEqual(Expected, Actual).

v3_missing_fields_test() ->
    % missing key_type and coordinator_keys
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    % I tried defining an ?assertExpectedExceptionInfo macro, but couldn't figure it out. Hence the repetition.
    Catch =
        catch find_coordinator_request:encode_find_coordinator_request_3(#{
            correlation_id => CorrelationId, client_id => ClientId
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "missing 'key' (string)\n"
                "missing 'key_type' (int8)"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v3_wrong_type_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Catch =
        catch find_coordinator_request:encode_find_coordinator_request_3(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            key_type => 0,
            key => 1234
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{1 => "expected 'key' to be of type 'string', but has type 'integer', value 1234"},
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v3_value_too_large_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Catch =
        catch find_coordinator_request:encode_find_coordinator_request_3(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            % too large for int8
            key_type => 128,
            key => <<"a">>
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    % TODO: It'd be nice if this said something about the value being out of range, but it'll do.
    ?assertEqual(
        #{1 => "expected 'key_type' to be of type 'int8', but has type 'integer', value 128"},
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v4_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    % v4 isn't supported by Wireshark or the Kafka broker I used, so this was eye-balled.
    Expected =
        <<0, 10, 0, 4, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45,
            72, 69, 82, 69, 0, 0, 3, 2, 97, 2, 98, 0>>,
    Actual = iolist_to_binary(
        find_coordinator_request:encode_find_coordinator_request_4(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            key_type => 0,
            coordinator_keys => [<<"a">>, <<"b">>]
        })
    ),
    ?assertEqual(Expected, Actual).

v4_missing_fields_test() ->
    % missing key_type and coordinator_keys
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Catch =
        catch find_coordinator_request:encode_find_coordinator_request_4(#{
            correlation_id => CorrelationId, client_id => ClientId
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "missing 'coordinator_keys' (array of string)\n"
                "missing 'key_type' (int8)"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.
