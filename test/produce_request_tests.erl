-module(produce_request_tests).
-include_lib("eunit/include/eunit.hrl").
-include("catch.hrl").

v7_encoded() ->
    % echo "ksagasgfsaeys-and-headers" | kcat -P -b localhost:9093 -t topic-a -p 1 -k the-key -H fodddo=bar -H baz=quuxsss
    <<0, 0, 0, 7, 0, 0, 0, 2, 0, 7, 114, 100, 107, 97, 102, 107, 97, 255, 255, 255, 255, 0, 0, 117,
        48, 0, 0, 0, 1, 0, 7, 116, 111, 112, 105, 99, 45, 97, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 123,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 111, 0, 0, 0, 0, 2, 29, 191, 134, 54, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 136, 221, 72, 252, 146, 0, 0, 1, 136, 221, 72, 252, 146, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 1, 122, 0, 0, 0, 14, 116, 104, 101, 45,
        107, 101, 121, 50, 107, 115, 97, 103, 97, 115, 103, 102, 115, 97, 101, 121, 115, 45, 97,
        110, 100, 45, 104, 101, 97, 100, 101, 114, 115, 4, 12, 102, 111, 100, 100, 100, 111, 6, 98,
        97, 114, 6, 98, 97, 122, 14, 113, 117, 117, 120, 115, 115, 115>>.

v7_decoded() ->
    #{
        api_key => 0,
        api_version => 7,
        client_id => <<"rdkafka">>,
        correlation_id => 2,
        acks => -1,
        timeout_ms => 30000,
        topic_data =>
            [
                #{
                    name => <<"topic-a">>,
                    partition_data =>
                        [
                            #{
                                index => 1,
                                records =>
                                    [
                                        #{
                                            attributes => #{compression => none},
                                            base_offset => 0,
                                            base_sequence => -1,
                                            base_timestamp => 1687339728018,
                                            crc => 499090998,
                                            last_offset_delta => 0,
                                            magic => 2,
                                            max_timestamp => 1687339728018,
                                            partition_leader_epoch => 0,
                                            producer_epoch => -1,
                                            producer_id => -1,
                                            records =>
                                                [
                                                    #{
                                                        attributes => 0,
                                                        headers => [
                                                            {<<"fodddo">>, <<"bar">>},
                                                            {<<"baz">>, <<"quuxsss">>}
                                                        ],
                                                        key => <<"the-key">>,
                                                        offset_delta => 0,
                                                        timestamp_delta => 0,
                                                        value =>
                                                            <<"ksagasgfsaeys-and-headers">>
                                                    }
                                                ]
                                        }
                                    ]
                            }
                        ]
                }
            ],
        transactional_id => null
    }.

v7_decode_test() ->
    ?assertEqual({v7_decoded(), <<>>}, produce_request:decode_produce_request_7(v7_encoded())).

v7_encode_test() ->
    % The decoder (currently) discards the request header. This will cause a problem if we ever write a broker. The
    % encoder, on the other hand, requires the header fields. So this test needs to put them back.
    ProduceRequest0 = v7_decoded(),
    % 'rdkafka' because it's captured from kcat.
    ClientId = <<"rdkafka">>,
    ProduceRequest = ProduceRequest0#{correlation_id => 2, client_id => ClientId},
    ?assertEqual(
        v7_encoded(), iolist_to_binary(produce_request:encode_produce_request_7(ProduceRequest))
    ).

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

v8_encode_test() ->
    ProduceRequest = #{
        correlation_id => ?CORRELATION_ID,
        client_id => ?CLIENT_ID,

        acks => -1,
        timeout_ms => 5_000,
        transactional_id => null,

        % It turns out that it's valid to send a request with empty 'topic_data', or with empty 'partition_data' inside
        % 'topic_data'. The broker returns a response with empty 'responses'. However, if you send empty 'records', the
        % broker returns an INVALID_RECORD error.
        topic_data => []
    },
    ?assertEqual(
        <<0, 0, 0, 8, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45, 72,
            69, 82, 69, 255, 255, 255, 255, 0, 0, 19, 136, 0, 0, 0, 0>>,
        iolist_to_binary(produce_request:encode_produce_request_8(ProduceRequest))
    ).

v8_encode_null_records_error_test() ->
    ProduceRequest = #{
        correlation_id => ?CORRELATION_ID,
        client_id => ?CLIENT_ID,

        acks => -1,
        timeout_ms => 5_000,
        transactional_id => null,

        topic_data => [
            #{
                name => <<"example">>,
                partition_data => [
                    #{
                        index => 0,
                        % Even though the JSON schema files say this is nullable; it isn't really. We should throw an
                        % error.
                        records => null
                    }
                ]
            }
        ]
    },

    {error, Reason = badarg, StackTrace} = ?CATCH(
        produce_request:encode_produce_request_8(ProduceRequest)
    ),
    ?assertEqual(
        #{
            1 =>
                "expected 'records' to be of type 'records', but has type 'null', value null"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.
