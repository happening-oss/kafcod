-module(describe_configs_request_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

encode_test() ->
    ?assertEqual(
        <<0, 32, 0, 0, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45,
            72, 69, 82, 69, 0, 0, 0, 1, 2, 0, 4, 99, 97, 114, 115, 0, 0, 0, 2, 0, 14, 99, 108, 101,
            97, 110, 117, 112, 46, 112, 111, 108, 105, 99, 121, 0, 12, 114, 101, 116, 101, 110, 116,
            105, 111, 110, 46, 109, 115>>,
        iolist_to_binary(
            describe_configs_request:encode_describe_configs_request_0(#{
                correlation_id => ?CORRELATION_ID,
                client_id => ?CLIENT_ID,
                resources => [
                    #{
                        resource_type => 2,
                        resource_name => <<"cars">>,
                        configuration_keys => [<<"cleanup.policy">>, <<"retention.ms">>]
                    }
                ]
            })
        )
    ).
