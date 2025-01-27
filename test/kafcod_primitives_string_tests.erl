-module(kafcod_primitives_string_tests).
-include_lib("eunit/include/eunit.hrl").

encode_compact_string_test() ->
    ?assertEqual(
        <<15, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54>>,
        iolist_to_binary(kafcod_primitives:encode_compact_string(<<"192.168.58.136">>))
    ).

decode_compact_string_test_() ->
    [
        ?_assertEqual(
            {<<"192.168.58.136">>, <<>>},
            kafcod_primitives:decode_compact_string(
                <<15, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54>>
            )
        ),

        ?_assertEqual(
            {<<"1Xv8WPwoSUKD2d-eqDjMxw">>, <<>>},
            kafcod_primitives:decode_compact_nullable_string(
                <<23, 49, 88, 118, 56, 87, 80, 119, 111, 83, 85, 75, 68, 50, 100, 45, 101, 113, 68,
                    106, 77, 120, 119>>
            )
        )
    ].
