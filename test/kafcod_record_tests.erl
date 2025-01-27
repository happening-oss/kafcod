-module(kafcod_record_tests).
-include_lib("eunit/include/eunit.hrl").

empty_headers_test_() ->
    Encoded = iolist_to_binary([
        <<38, 0, 0, 0>>,
        <<16, 103, 114, 101, 101, 116, 105, 110, 103>>,
        <<10, 104, 101, 108, 108, 111>>,
        <<0>>
    ]),
    Decoded = #{
        attributes => 0,
        headers => [],
        key => <<"greeting">>,
        offset_delta => 0,
        timestamp_delta => 0,
        value => <<"hello">>
    },
    [
        {"decode_record", ?_assertEqual({Decoded, <<>>}, kafcod_record:decode_record(Encoded))},
        {"encode_record",
            ?_assertEqual(Encoded, iolist_to_binary(kafcod_record:encode_record(Decoded)))}
    ].

null_key_test_() ->
    Encoded = iolist_to_binary([
        <<22, 0, 0, 0>>,
        <<1>>,
        <<10, 104, 101, 108, 108, 111>>,
        <<0>>
    ]),
    Decoded = #{
        attributes => 0,
        headers => [],
        key => null,
        offset_delta => 0,
        timestamp_delta => 0,
        value => <<"hello">>
    },
    [
        {"decode_record", ?_assertEqual({Decoded, <<>>}, kafcod_record:decode_record(Encoded))},
        {"encode_record",
            ?_assertEqual(Encoded, iolist_to_binary(kafcod_record:encode_record(Decoded)))}
    ].

null_value_test_() ->
    Encoded = iolist_to_binary([
        <<22, 0, 0, 0>>,
        <<10, 104, 101, 108, 108, 111>>,
        <<1>>,
        <<0>>
    ]),
    Decoded = #{
        attributes => 0,
        headers => [],
        key => <<"hello">>,
        offset_delta => 0,
        timestamp_delta => 0,
        value => null
    },
    [
        {"decode_record", ?_assertEqual({Decoded, <<>>}, kafcod_record:decode_record(Encoded))},
        {"encode_record",
            ?_assertEqual(Encoded, iolist_to_binary(kafcod_record:encode_record(Decoded)))}
    ].

null_key_and_value_test_() ->
    Encoded = iolist_to_binary(
        [
            <<12, 0, 0, 0>>,
            <<1>>,
            <<1>>,
            <<0>>
        ]
    ),
    Decoded = #{
        attributes => 0,
        headers => [],
        key => null,
        offset_delta => 0,
        timestamp_delta => 0,
        value => null
    },
    [
        {"decode_record", ?_assertEqual({Decoded, <<>>}, kafcod_record:decode_record(Encoded))},
        {"encode_record",
            ?_assertEqual(Encoded, iolist_to_binary(kafcod_record:encode_record(Decoded)))}
    ].

encode_record_headers_wrong_type_test() ->
    Catch =
        catch kafcod_record:encode_record(#{
            attributes => 0,
            % headers must be empty, rather than null
            headers => null,
            key => null,
            offset_delta => 0,
            timestamp_delta => 0,
            value => <<"hello">>
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "expected 'headers' to be an array of 'header', but has type 'null', value null"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

encode_headers_test() ->
    Headers =
        [
            {<<"a-header">>, <<"its-value">>},
            {<<"another-header">>, <<"some-value">>}
        ],
    Encoded = [
        % Count = 2, varint-encoded
        <<4>>,
        [
            % Length, Value
            <<16>>,
            <<97, 45, 104, 101, 97, 100, 101, 114>>,
            <<18>>,
            <<105, 116, 115, 45, 118, 97, 108, 117, 101>>,
            <<28>>,
            <<97, 110, 111, 116, 104, 101, 114, 45, 104, 101, 97, 100, 101, 114>>,
            <<20>>,
            <<115, 111, 109, 101, 45, 118, 97, 108, 117, 101>>
        ]
    ],
    ?assertEqual(
        iolist_to_binary(Encoded), iolist_to_binary(kafcod_record:encode_headers(Headers))
    ).

encode_headers_error_test() ->
    Headers = [{an_atom, another_atom}],
    Catch = catch kafcod_record:encode_headers(Headers),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{
            1 =>
                "expected 'key' to be of type 'string', but has type 'atom', value an_atom\n"
                "expected 'value' to be of type 'nullable_bytes', but has type 'atom', value another_atom"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

encode_headers_null_test_() ->
    [
        {"header key cannot be null",
            ?_assertError(badarg, kafcod_record:encode_headers([{null, <<"value">>}]))},
        {"header value can be null",
            ?_assertEqual(
                <<2, 6, 107, 101, 121, 1>>,
                iolist_to_binary(kafcod_record:encode_headers([{<<"key">>, null}]))
            )}
    ].
