-module(kafcod_primitives_array_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/encoders.hrl").
-include("../src/decoders.hrl").

unexpected_element(_) ->
    % When encoding or decoding an array, a function is used to encode or decode the array elements.
    % That function should never be called when we're encoding/decoding an empty/null array.
    error(unexpected).

encode_empty_string_array_test() ->
    ?assertEqual(
        <<0, 0, 0, 0>>, iolist_to_binary(kafcod_primitives:encode_array([], ?encode_string_))
    ).

encode_multi_string_array_test() ->
    ?assertEqual(
        <<0, 0, 0, 2, 0, 3, $a, $b, $c, 0, 4, $c, $d, $e, $f>>,
        iolist_to_binary(kafcod_primitives:encode_array([<<"abc">>, <<"cdef">>], ?encode_string_))
    ).

encode_null_nullable_array_test() ->
    ?assertEqual(
        <<-1:32/big-signed>>,
        iolist_to_binary(kafcod_primitives:encode_nullable_array(null, ?encode_int64_))
    ).

encode_empty_nullable_array_test() ->
    ?assertEqual(
        <<0:32/big-signed>>,
        iolist_to_binary(kafcod_primitives:encode_nullable_array([], ?encode_int64_))
    ).

encode_nullable_array_int64_test() ->
    ?assertEqual(
        <<0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 34>>,
        iolist_to_binary(kafcod_primitives:encode_nullable_array([12, 34], ?encode_int64_))
    ).

encode_nullable_array_string_test() ->
    ?assertEqual(
        <<0, 0, 0, 2, 0, 3, 97, 98, 99, 0, 3, 100, 101, 102>>,
        iolist_to_binary(
            kafcod_primitives:encode_nullable_array([<<"abc">>, <<"def">>], ?encode_string_)
        )
    ).

decode_null_nullable_array_string_test() ->
    ?assertEqual(
        {null, <<>>},
        kafcod_primitives:decode_nullable_array(
            <<-1:32/big-signed>>, fun unexpected_element/1
        )
    ).

decode_empty_nullable_array_string_test() ->
    ?assertEqual(
        {[], <<>>},
        kafcod_primitives:decode_nullable_array(
            <<0, 0, 0, 0>>, fun unexpected_element/1
        )
    ).

decode_nullable_array_string_test() ->
    ?assertEqual(
        {[<<"abc">>, <<"def">>], <<>>},
        kafcod_primitives:decode_nullable_array(
            <<0, 0, 0, 2, 0, 3, 97, 98, 99, 0, 3, 100, 101, 102>>, ?decode_string_
        )
    ).

encode_null_int32_compact_array_test() ->
    ?assertEqual(
        <<0>>,
        iolist_to_binary(kafcod_primitives:encode_compact_nullable_array(null, ?encode_int32_))
    ).

encode_empty_int32_compact_array_test() ->
    ?assertEqual(
        <<1>>, iolist_to_binary(kafcod_primitives:encode_compact_nullable_array([], ?encode_int32_))
    ).

encode_int32_compact_array_test() ->
    ?assertEqual(
        <<4, 200:32/big, 100:32/big, 300:32/big>>,
        iolist_to_binary(
            kafcod_primitives:encode_compact_nullable_array([200, 100, 300], ?encode_int32_)
        )
    ).

decode_empty_compact_array_trailing_test() ->
    ?assertEqual(
        {[], <<0>>},
        kafcod_primitives:decode_compact_array(<<1, 0>>, fun unexpected_element/1)
    ).

decode_int32_compact_nullable_array_test() ->
    ?assertEqual(
        {[200, 100, 300], <<>>},
        kafcod_primitives:decode_compact_nullable_array(
            <<4, 200:32/big, 100:32/big, 300:32/big>>, ?decode_int32_
        )
    ).

decode_empty_compact_nullable_array_test() ->
    ?assertEqual(
        {[], <<$a, $b>>},
        kafcod_primitives:decode_compact_nullable_array(<<1, $a, $b>>, fun unexpected_element/1)
    ).

decode_null_compact_nullable_array_test() ->
    ?assertEqual(
        {null, <<$m, $o, $r, $e>>},
        kafcod_primitives:decode_compact_nullable_array(
            <<0, $m, $o, $r, $e>>, fun unexpected_element/1
        )
    ).
