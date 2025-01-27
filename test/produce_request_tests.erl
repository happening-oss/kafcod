-module(produce_request_tests).
-include_lib("eunit/include/eunit.hrl").

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
