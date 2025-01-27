-module(api_versions_request_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CORRELATION_ID, 203569230).
-define(CLIENT_ID, <<"CLIENT-ID-IN-HERE">>).

v2_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    Expected =
        <<0, 18, 0, 2, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45,
            72, 69, 82, 69>>,
    Actual = iolist_to_binary(
        api_versions_request:encode_api_versions_request_2(#{
            correlation_id => CorrelationId,
            client_id => ClientId
        })
    ),
    ?assertEqual(Expected, Actual).

v3_test() ->
    CorrelationId = ?CORRELATION_ID,
    ClientId = ?CLIENT_ID,
    ClientSoftwareName = <<"kafcod">>,
    ClientSoftwareVersion = <<"0.1.0">>,
    Expected =
        <<0, 18, 0, 3, 12, 34, 56, 78, 0, 17, 67, 76, 73, 69, 78, 84, 45, 73, 68, 45, 73, 78, 45,
            72, 69, 82, 69, 0, 7, 107, 97, 102, 99, 111, 100, 6, 48, 46, 49, 46, 48, 0>>,
    Actual = iolist_to_binary(
        api_versions_request:encode_api_versions_request_3(#{
            correlation_id => CorrelationId,
            client_id => ClientId,
            client_software_name => ClientSoftwareName,
            client_software_version => ClientSoftwareVersion
        })
    ),
    ?assertEqual(Expected, Actual).
