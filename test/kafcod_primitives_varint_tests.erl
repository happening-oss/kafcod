-module(kafcod_primitives_varint_tests).
-include_lib("eunit/include/eunit.hrl").

unsigned_varint_roundtrip_test_() ->
    [
        fun() ->
            Enc = kafcod_primitives:encode_unsigned_varint(V),
            {Dec, <<>>} = kafcod_primitives:decode_unsigned_varint(Enc),
            ?assertEqual(Dec, V)
        end
     || V <- [
            0,
            1,
            16#F,
            16#1F,
            16#7F,
            16#FF,
            16#1FF,
            16#7FF,
            16#FFF,
            16#FFFF,
            16#F_FFFF,
            16#FF_FFFF,
            16#FFF_FFFF,
            16#FFFF_FFFF,
            16#F_FFFF_FFFF,
            16#FF_FFFF_FFFF,
            16#FFF_FFFF_FFFF,
            16#FFFF_FFFF_FFFF_FFFF
        ]
    ].

unsigned_varint_encoding_test_() ->
    [
        ?_assertEqual(<<0>>, kafcod_primitives:encode_unsigned_varint(0)),
        ?_assertEqual(<<1>>, kafcod_primitives:encode_unsigned_varint(1))
    ].

unsigned_varint_decoding_test_() ->
    [
        ?_assertEqual({0, <<>>}, kafcod_primitives:decode_unsigned_varint(<<0>>)),
        ?_assertEqual({1, <<>>}, kafcod_primitives:decode_unsigned_varint(<<1>>)),
        ?_assertEqual({23, <<>>}, kafcod_primitives:decode_unsigned_varint(<<23>>))
    ].

signed_varint_encoding_equivalence_test_() ->
    % Signed varints are encoded using "zigzag" encoding, to avoid the sign extension. Instead, positive numbers are
    % doubled (so 1 is encoded as 2, 2 as 4, 3 as 6, and so on), and negative numbers are encoded as odd numbers in the
    % gaps this opens up. Compare this with "Hilbert's Hotel".
    % The Protocol Buffers documentation, at https://protobuf.dev/programming-guides/encoding/#signed-ints,
    % gives a table of signed and unsigned equivalent numbers. This test is based on that table.
    % Note, however, that the bit-shifting in the document is wrong. Ignore it.
    Assert = fun(Signed, Unsigned) ->
        {
            iolist_to_binary(
                io_lib:format("signed ~B should encode as unsigned ~B", [Signed, Unsigned])
            ),
            ?_assertEqual(
                kafcod_primitives:encode_signed_varint(Signed),
                kafcod_primitives:encode_unsigned_varint(Unsigned)
            )
        }
    end,

    [
        % Zero is encoded as zero.
        Assert(0, 0),
        % Positive numbers are doubled.
        Assert(1, 2),
        Assert(2, 4),
        Assert(3, 6),
        Assert(16#7FFF_FFFF, 16#FFFF_FFFE),
        % Negative numbers go in the gaps: (abs(x) * 2) - 1
        Assert(-1, 1),
        Assert(-2, 3),
        Assert(-3, 5),
        Assert(-(16#8000_0000), 16#FFFF_FFFF)
    ].

signed_varint_encoding_test_() ->
    [
        fun() ->
            Binary = kafcod_primitives:encode_signed_varint(Value),
            {Actual, <<>>} = kafcod_primitives:decode_signed_varint(Binary),
            ?assertEqual(Value, Actual)
        end
     || Value <- [1, 12, 4577, 0, -64, -1, -6545745]
    ].

encoding_length_test_() ->
    [
        ?_assertEqual(1, byte_size(kafcod_primitives:encode_unsigned_varint(1))),
        ?_assertEqual(1, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 7 - 1))),
        ?_assertEqual(2, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 7))),
        ?_assertEqual(2, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 14 - 1))),
        ?_assertEqual(3, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 14))),
        ?_assertEqual(3, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 21 - 1))),
        ?_assertEqual(4, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 21))),
        ?_assertEqual(4, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 28 - 1))),
        ?_assertEqual(5, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 28))),
        ?_assertEqual(5, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 35 - 1))),
        ?_assertEqual(6, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 35))),
        ?_assertEqual(6, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 42 - 1))),
        ?_assertEqual(7, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 42))),
        ?_assertEqual(7, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 49 - 1))),
        ?_assertEqual(8, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 49))),
        ?_assertEqual(8, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 56 - 1))),
        ?_assertEqual(9, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 56))),
        ?_assertEqual(9, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 63 - 1))),
        ?_assertEqual(10, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 63))),
        ?_assertEqual(10, byte_size(kafcod_primitives:encode_unsigned_varint(1 bsl 70 - 1))),

        ?_assertEqual(5, byte_size(kafcod_primitives:encode_unsigned_varint(16#FFFF_FFFF))),
        ?_assertEqual(10, byte_size(kafcod_primitives:encode_signed_varint(16#FFFF_FFFF_FFFF_FFFF)))
    ].
