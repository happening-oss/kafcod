-module(broker_heartbeat_request_tests).
-include_lib("eunit/include/eunit.hrl").

%% broker_heartbeat_request has two ?_decode_bool() calls in it; we want to check that doesn't get broken.

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

encode_decode_test_() ->
    Encoded = iolist_to_binary([
        [<<0, 63, 0, 0, 12, 34, 56, 78>>, [<<0, 17>>, <<"CLIENT-ID-IN-HERE">>], <<0>>],
        <<0, 0, 0, 101>>,
        <<0, 0, 0, 0, 0, 0, 0, 42>>,
        <<0, 0, 0, 0, 0, 0, 48, 57>>,
        <<1>>,
        <<0>>,
        [<<0>>]
    ]),
    Decoded = #{
        broker_epoch => 42,
        broker_id => 101,
        client_id => ?CLIENT_ID,
        correlation_id => ?CORRELATION_ID,
        current_metadata_offset => 12345,
        want_fence => true,
        want_shut_down => false
    },
    [
        ?_assertEqual(
            Encoded,
            iolist_to_binary(
                broker_heartbeat_request:encode_broker_heartbeat_request_0(Decoded)
            )
        ),
        ?_assertEqual(
            {Decoded#{api_key => 63, api_version => 0}, <<>>},
            broker_heartbeat_request:decode_broker_heartbeat_request_0(Encoded)
        )
    ].
