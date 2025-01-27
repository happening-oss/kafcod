-module(controlled_shutdown_request_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

v0_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Expected = <<0, 7, 0, 0, 12, 34, 56, 78, 0, 0, 3, 132>>,
    Actual = iolist_to_binary(
        controlled_shutdown_request:encode_controlled_shutdown_request_0(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            broker_id => 900
        })
    ),
    ?assertEqual(Expected, Actual).
