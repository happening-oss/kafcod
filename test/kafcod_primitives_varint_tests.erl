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
        ?_assertEqual(1, iolist_size(kafcod_primitives:encode_unsigned_varint(1))),
        ?_assertEqual(1, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 7 - 1))),
        ?_assertEqual(2, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 7))),
        ?_assertEqual(2, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 14 - 1))),
        ?_assertEqual(3, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 14))),
        ?_assertEqual(3, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 21 - 1))),
        ?_assertEqual(4, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 21))),
        ?_assertEqual(4, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 28 - 1))),
        ?_assertEqual(5, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 28))),
        ?_assertEqual(5, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 35 - 1))),
        ?_assertEqual(6, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 35))),
        ?_assertEqual(6, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 42 - 1))),
        ?_assertEqual(7, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 42))),
        ?_assertEqual(7, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 49 - 1))),
        ?_assertEqual(8, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 49))),
        ?_assertEqual(8, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 56 - 1))),
        ?_assertEqual(9, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 56))),
        ?_assertEqual(9, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 63 - 1))),
        ?_assertEqual(10, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 63))),
        ?_assertEqual(10, iolist_size(kafcod_primitives:encode_unsigned_varint(1 bsl 70 - 1))),

        ?_assertEqual(5, iolist_size(kafcod_primitives:encode_unsigned_varint(16#FFFF_FFFF))),
        ?_assertEqual(10, iolist_size(kafcod_primitives:encode_signed_varint(16#FFFF_FFFF_FFFF_FFFF)))
    ].

sizeof_test_() ->
    [
        ?_assertEqual(1, kafcod_primitives:sizeof_unsigned_varint(0)),
        ?_assertEqual(1, kafcod_primitives:sizeof_unsigned_varint(1)),
        ?_assertEqual(1, kafcod_primitives:sizeof_unsigned_varint(1 bsl 7 - 1)),
        ?_assertEqual(2, kafcod_primitives:sizeof_unsigned_varint(1 bsl 7)),
        ?_assertEqual(2, kafcod_primitives:sizeof_unsigned_varint(1 bsl 14 - 1)),
        ?_assertEqual(3, kafcod_primitives:sizeof_unsigned_varint(1 bsl 14)),
        ?_assertEqual(3, kafcod_primitives:sizeof_unsigned_varint(1 bsl 21 - 1)),
        ?_assertEqual(4, kafcod_primitives:sizeof_unsigned_varint(1 bsl 21)),
        ?_assertEqual(4, kafcod_primitives:sizeof_unsigned_varint(1 bsl 28 - 1)),
        ?_assertEqual(5, kafcod_primitives:sizeof_unsigned_varint(1 bsl 28)),
        ?_assertEqual(5, kafcod_primitives:sizeof_unsigned_varint(1 bsl 35 - 1)),
        ?_assertEqual(6, kafcod_primitives:sizeof_unsigned_varint(1 bsl 35)),
        ?_assertEqual(6, kafcod_primitives:sizeof_unsigned_varint(1 bsl 42 - 1)),
        ?_assertEqual(7, kafcod_primitives:sizeof_unsigned_varint(1 bsl 42)),
        ?_assertEqual(7, kafcod_primitives:sizeof_unsigned_varint(1 bsl 49 - 1)),
        ?_assertEqual(8, kafcod_primitives:sizeof_unsigned_varint(1 bsl 49)),
        ?_assertEqual(8, kafcod_primitives:sizeof_unsigned_varint(1 bsl 56 - 1)),
        ?_assertEqual(9, kafcod_primitives:sizeof_unsigned_varint(1 bsl 56)),
        ?_assertEqual(9, kafcod_primitives:sizeof_unsigned_varint(1 bsl 63 - 1)),
        ?_assertEqual(10, kafcod_primitives:sizeof_unsigned_varint(1 bsl 63)),
        ?_assertEqual(10, kafcod_primitives:sizeof_unsigned_varint(1 bsl 70 - 1)),

        % for signed positive, we just have 1 fewer bit available
        ?_assertEqual(1, kafcod_primitives:sizeof_signed_varint(0)),
        ?_assertEqual(1, kafcod_primitives:sizeof_signed_varint(1)),
        ?_assertEqual(1, kafcod_primitives:sizeof_signed_varint(1 bsl 6 - 1)),
        ?_assertEqual(2, kafcod_primitives:sizeof_signed_varint(1 bsl 6)),
        ?_assertEqual(2, kafcod_primitives:sizeof_signed_varint(1 bsl 13 - 1)),
        ?_assertEqual(3, kafcod_primitives:sizeof_signed_varint(1 bsl 13)),
        ?_assertEqual(3, kafcod_primitives:sizeof_signed_varint(1 bsl 20 - 1)),
        ?_assertEqual(4, kafcod_primitives:sizeof_signed_varint(1 bsl 20)),
        ?_assertEqual(4, kafcod_primitives:sizeof_signed_varint(1 bsl 27 - 1)),
        ?_assertEqual(5, kafcod_primitives:sizeof_signed_varint(1 bsl 27)),
        ?_assertEqual(5, kafcod_primitives:sizeof_signed_varint(1 bsl 34 - 1)),
        ?_assertEqual(6, kafcod_primitives:sizeof_signed_varint(1 bsl 34)),
        ?_assertEqual(6, kafcod_primitives:sizeof_signed_varint(1 bsl 41 - 1)),
        ?_assertEqual(7, kafcod_primitives:sizeof_signed_varint(1 bsl 41)),
        ?_assertEqual(7, kafcod_primitives:sizeof_signed_varint(1 bsl 48 - 1)),
        ?_assertEqual(8, kafcod_primitives:sizeof_signed_varint(1 bsl 48)),
        ?_assertEqual(8, kafcod_primitives:sizeof_signed_varint(1 bsl 55 - 1)),
        ?_assertEqual(9, kafcod_primitives:sizeof_signed_varint(1 bsl 55)),
        ?_assertEqual(9, kafcod_primitives:sizeof_signed_varint(1 bsl 62 - 1)),
        ?_assertEqual(10, kafcod_primitives:sizeof_signed_varint(1 bsl 62)),
        ?_assertEqual(10, kafcod_primitives:sizeof_signed_varint(1 bsl 69 - 1)),
        % for signed negative we also have 1 fewer bit available, but also 1 is added implicitly to
        % the binary value. Eg. -3 looks like:
        % terminator bit |      2 | sign bit
        %              0 | 000010 | 1
        % As 63 is the limit for 6 bits, -64 fits in 1 byte, -65 requires 2
        ?_assertEqual(1, kafcod_primitives:sizeof_signed_varint(-1)),
        ?_assertEqual(1, kafcod_primitives:sizeof_signed_varint(-(1 bsl 6))),
        ?_assertEqual(2, kafcod_primitives:sizeof_signed_varint(-(1 bsl 6 + 1))),
        ?_assertEqual(2, kafcod_primitives:sizeof_signed_varint(-(1 bsl 13))),
        ?_assertEqual(3, kafcod_primitives:sizeof_signed_varint(-(1 bsl 13 + 1))),
        ?_assertEqual(3, kafcod_primitives:sizeof_signed_varint(-(1 bsl 20))),
        ?_assertEqual(4, kafcod_primitives:sizeof_signed_varint(-(1 bsl 20 + 1))),
        ?_assertEqual(4, kafcod_primitives:sizeof_signed_varint(-(1 bsl 27))),
        ?_assertEqual(5, kafcod_primitives:sizeof_signed_varint(-(1 bsl 27 + 1))),
        ?_assertEqual(5, kafcod_primitives:sizeof_signed_varint(-(1 bsl 34))),
        ?_assertEqual(6, kafcod_primitives:sizeof_signed_varint(-(1 bsl 34 + 1))),
        ?_assertEqual(6, kafcod_primitives:sizeof_signed_varint(-(1 bsl 41))),
        ?_assertEqual(7, kafcod_primitives:sizeof_signed_varint(-(1 bsl 41 + 1))),
        ?_assertEqual(7, kafcod_primitives:sizeof_signed_varint(-(1 bsl 48))),
        ?_assertEqual(8, kafcod_primitives:sizeof_signed_varint(-(1 bsl 48 + 1))),
        ?_assertEqual(8, kafcod_primitives:sizeof_signed_varint(-(1 bsl 55))),
        ?_assertEqual(9, kafcod_primitives:sizeof_signed_varint(-(1 bsl 55 + 1))),
        ?_assertEqual(9, kafcod_primitives:sizeof_signed_varint(-(1 bsl 62))),
        ?_assertEqual(10, kafcod_primitives:sizeof_signed_varint(-(1 bsl 62 + 1))),
        ?_assertEqual(10, kafcod_primitives:sizeof_signed_varint(-(1 bsl 69)))
    ].
