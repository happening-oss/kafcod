-module(create_topics_response_tests).
-include_lib("eunit/include/eunit.hrl").

v2_decode_test() ->
    Capture =
        <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 7, 116, 111, 112, 105, 99, 45, 98, 0, 0, 255,
            255>>,
    Decoded = {
        #{
            correlation_id => 2,
            throttle_time_ms => 0,
            topics => [#{error_code => 0, error_message => null, name => <<"topic-b">>}]
        },
        <<>>
    },
    ?assertEqual(Decoded, create_topics_response:decode_create_topics_response_2(Capture)).
