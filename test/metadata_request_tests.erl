-module(metadata_request_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

v1_test() ->
    % v1 is the first version where topics is nullable.
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Expected =
        <<0, 3, 0, 1, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45, 72,
            69, 82, 69, 255, 255, 255, 255>>,
    Actual = iolist_to_binary(
        metadata_request:encode_metadata_request_1(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            topics => null
        })
    ),
    ?assertEqual(Expected, Actual).

v4_wrong_fields_test() ->
    Catch =
        catch metadata_request:encode_metadata_request_4(#{
            client_id => aaa,
            correlation_id => ?CORRELATION_ID,
            topics => [],
            allow_auto_topic_creation => false
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "expected 'client_id' to be of type 'nullable_string', but has type 'atom', value aaa"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v9_missing_fields_test() ->
    % Ad-hoc testing revealed that when any field was missing, the bool fields were being reported as the wrong type.
    % This is a regression test to make sure that stays fixed, and we don't complain about the bool fields at all.
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Catch =
        catch metadata_request:encode_metadata_request_9(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            topics => [],
            allow_auto_topic_creation => false,
            include_topic_authorized_operations => true
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "missing 'include_cluster_authorized_operations' (bool)"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v9_missing_topics_test() ->
    % Errors about arrays should include the element type.
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Catch =
        catch metadata_request:encode_metadata_request_9(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            allow_auto_topic_creation => false,
            include_topic_authorized_operations => true,
            include_cluster_authorized_operations => false
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "missing 'topics' (nullable_array of metadata_request_topic_9)"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

v9_decode_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Capture =
        <<0, 3, 0, 9, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45, 72,
            69, 82, 69, 0, 1, 0, 1, 1, 0>>,
    ?assertEqual(
        Capture,
        iolist_to_binary(
            metadata_request:encode_metadata_request_9(#{
                correlation_id => CorrelationId,
                client_id => ClientId,
                topics => [],
                allow_auto_topic_creation => false,
                include_cluster_authorized_operations => true,
                include_topic_authorized_operations => true
            })
        )
    ),

    ?assertEqual(
        {
            #{
                api_key => 3,
                api_version => 9,
                client_id => ?CLIENT_ID,
                correlation_id => ?CORRELATION_ID,
                allow_auto_topic_creation => false,
                include_cluster_authorized_operations => true,
                include_topic_authorized_operations => true,
                topics => []
            },
            <<>>
        },
        metadata_request:decode_metadata_request_9(Capture)
    ),
    ok.

v9_kcat_garbage_test() ->
    Capture = iolist_to_binary([
        <<0, 3, 0, 9, 0, 0, 0, 5, 0, 7, 114, 100, 107, 97, 102, 107, 97, 0, 0, 0, 0, 0, 1, 0, 0, 0>>
    ]),
    ?assertEqual(
        {
            #{
                correlation_id => 5,
                client_id => <<"rdkafka">>,
                topics => null,
                api_key => 3,
                api_version => 9,
                include_cluster_authorized_operations => false,
                allow_auto_topic_creation => false,
                include_topic_authorized_operations => false
            },
            % trailing garbage
            <<0>>
        },
        metadata_request:decode_metadata_request_9(Capture)
    ),
    ok.

v12_homebrew_kcat_decode_test() ->
    % Homebrew kcat (1.7.0, librdkafka 2.3.0) sends two Metadata request messages. The first seems OK. The second has
    % some trailing garbage.
    % The latest versions of kcat, librdkafka (at the time of writing) are 1.7.1 and 2.3.0.
    Capture = iolist_to_binary([
        % request header
        [<<0, 3>>, <<0, 12>>, <<0, 0, 0, 2>>, <<0, 7, 114, 100, 107, 97, 102, 107, 97>>, <<0>>],
        % topics = []
        <<1>>,
        % two booleans
        [<<0>>, <<0>>],
        % no tagged fields
        <<0>>
    ]),
    ?assertEqual(
        {
            #{
                correlation_id => 2,
                client_id => <<"rdkafka">>,
                topics => [],
                api_key => 3,
                api_version => 12,
                allow_auto_topic_creation => false,
                include_topic_authorized_operations => false
            },
            <<>>
        },
        metadata_request:decode_metadata_request_12(Capture)
    ),
    ok.

v12_homebrew_kcat_decode_trailing_test() ->
    Capture = iolist_to_binary([
        % request header
        [<<0, 3>>, <<0, 12>>, <<0, 0, 0, 3>>, <<0, 7, 114, 100, 107, 97, 102, 107, 97>>, <<0>>],
        % topics = null
        <<0>>,
        % two booleans
        [<<0>>, <<0>>],
        % no tagged fields
        <<0>>,
        % trailing garbage; seems to be a bug in kcat; Kafka appears to ignore it.
        <<1, 0, 0>>
    ]),
    ?assertEqual(
        {
            #{
                correlation_id => 3,
                client_id => <<"rdkafka">>,
                topics => null,
                api_key => 3,
                api_version => 12,
                allow_auto_topic_creation => false,
                include_topic_authorized_operations => false
            },
            % trailing garbage
            <<1, 0, 0>>
        },
        metadata_request:decode_metadata_request_12(Capture)
    ),
    ok.
