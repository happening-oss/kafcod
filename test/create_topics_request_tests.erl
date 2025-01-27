-module(create_topics_request_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

v2_null_array_test() ->
    Catch =
        catch create_topics_request:encode_create_topics_request_2(#{
            correlation_id => ?CORRELATION_ID,
            client_id => ?CLIENT_ID,
            topics => [
                #{
                    name => <<"topic-a">>,
                    num_partitions => 4,
                    replication_factor => 3,
                    configs => null,
                    assignments => null
                }
            ],
            timeout_ms => 3_000,
            validate_only => false
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "expected 'assignments' to be an array of 'creatable_replica_assignment_2', but has type 'null', value null\n"
                "expected 'configs' to be an array of 'createable_topic_config_2', but has type 'null', value null"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v5_not_a_map_test() ->
    Catch =
        catch create_topics_request:encode_create_topics_request_5(not_a_map),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{1 => "expected a map"},
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v5_topic_not_a_map_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Catch =
        catch create_topics_request:encode_create_topics_request_5(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            validate_only => false,
            topics => [not_a_map],
            timeout_ms => 5_000
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    % TODO: It would be improved if the error message said _what_ was expected to be a map.
    % In this case topics, of type []CreatableTopic.
    ?assertEqual(
        #{1 => "expected a map"},
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v5_topic_missing_fields_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Catch =
        catch create_topics_request:encode_create_topics_request_5(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            validate_only => false,
            topics => [#{}],
            timeout_ms => 5_000
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "missing 'assignments' (array of creatable_replica_assignment_5)\n"
                "missing 'configs' (array of createable_topic_config_5)\n"
                "missing 'name' (string)\n"
                "missing 'num_partitions' (int32)\n"
                "missing 'replication_factor' (int16)"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v5_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Expected =
        <<0, 19, 0, 5, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45,
            72, 69, 82, 69, 0, 2, 9, 109, 121, 95, 116, 111, 112, 105, 99, 0, 0, 0, 64, 0, 3, 1, 1,
            0, 0, 0, 19, 136, 0, 0>>,
    Actual = iolist_to_binary(
        create_topics_request:encode_create_topics_request_5(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            validate_only => false,
            topics => [
                #{
                    name => <<"my_topic">>,
                    replication_factor => 3,
                    num_partitions => 64,
                    % empty array for automatic assignment
                    assignments => [],
                    % also empty, although that's not mentioned in the docs/schema anywhere.
                    configs => []
                }
            ],
            timeout_ms => 5_000
        })
    ),
    ?assertEqual(Expected, Actual).
