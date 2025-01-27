-module(kafcod_primitives).
-export([
    encode_signed_varint/1,
    decode_signed_varint/1,

    encode_unsigned_varint/1,
    decode_unsigned_varint/1
]).
-export([
    decode_bool/1
]).
-export([
    encode_string/1,
    decode_string/1,

    encode_nullable_string/1,
    decode_nullable_string/1,

    encode_compact_string/1,
    decode_compact_string/1,

    encode_compact_nullable_string/1,
    decode_compact_nullable_string/1
]).
-export([
    encode_bytes/1,
    decode_bytes/1,

    encode_nullable_bytes/1,
    decode_nullable_bytes/1,

    encode_compact_bytes/1,
    decode_compact_bytes/1,

    encode_compact_nullable_bytes/1,
    decode_compact_nullable_bytes/1
]).
-export([
    encode_array/2,
    decode_array/2,

    encode_nullable_array/2,
    decode_nullable_array/2,

    encode_compact_array/2,
    decode_compact_array/2,

    encode_compact_nullable_array/2,
    decode_compact_nullable_array/2
]).
-export([
    encode_uuid/1,
    decode_uuid/1
]).
-export([
    encode_tagged_fields/2,
    decode_tagged_fields/3
]).

-spec encode_signed_varint(Value :: integer()) -> iodata().

encode_signed_varint(Value) when is_integer(Value) ->
    encode_unsigned_varint(encode_zigzag(Value)).

-spec decode_signed_varint(Input :: nonempty_binary()) -> {Value :: integer(), Rest :: binary()}.

decode_signed_varint(Binary) when is_binary(Binary) ->
    {Value, Rest} = decode_unsigned_varint(Binary),
    {decode_zigzag(Value), Rest}.

% Zigzag encoding is used to avoid sign-extending negative numbers. Positive numbers are doubled, and negative numbers
% are folded over to fit in the gaps.
% For example, 0 -> 0, 1 -> 2, 2 -> 4, 3 -> 6, and -1 -> 1, -2 -> 3, -3 -> 5.
-spec encode_zigzag(integer()) -> non_neg_integer().

encode_zigzag(Value) when Value >= 0 -> 2 * Value;
encode_zigzag(Value) when Value < 0 -> (2 * -Value) - 1.

-spec decode_zigzag(non_neg_integer()) -> integer().

decode_zigzag(Value) when Value band 1 =:= 0 -> Value div 2;
decode_zigzag(Value) when Value band 1 =:= 1 -> -((Value + 1) div 2).

-spec encode_unsigned_varint(Value :: non_neg_integer()) -> iodata().

encode_unsigned_varint(Value) when is_integer(Value), Value >= 0 ->
    encode_unsigned_varint(<<>>, Value).

-spec encode_unsigned_varint(Acc :: binary(), Value :: non_neg_integer()) -> iodata().

encode_unsigned_varint(Acc, Value) when Value =< 127 ->
    <<Acc/binary, Value>>;
encode_unsigned_varint(Acc, Value) ->
    encode_unsigned_varint(<<Acc/binary, (Value band 127 bor 128)>>, Value bsr 7).

-spec decode_unsigned_varint(Input :: nonempty_binary()) ->
    {Value :: non_neg_integer(), Rest :: binary()}.

% 64 bit number, varint-encoded, can be up to ten octets; we unroll all the cases here for (hopefully) speed.
% Smaller values (more common) go first.
% erlfmt-ignore
% ^^ it's easier to see the similarities across the single lines.
decode_unsigned_varint(<<0:1, A:7, Rest/binary>>) ->
    {A, Rest};
decode_unsigned_varint(<<1:1, A:7, 0:1, B:7, Rest/binary>>) ->
    {(B bsl 7) bor A, Rest};
decode_unsigned_varint(<<1:1, A:7, 1:1, B:7, 0:1, C:7, Rest/binary>>) ->
    {(C bsl 14) bor (B bsl 7) bor A, Rest};
decode_unsigned_varint(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 0:1, D:7, Rest/binary>>) ->
    {(D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A, Rest};
decode_unsigned_varint(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 1:1, D:7, 0:1, E:7, Rest/binary>>) ->
    {(E bsl 28) bor (D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A, Rest};
decode_unsigned_varint(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 1:1, D:7, 1:1, E:7, 0:1, F:7, Rest/binary>>) ->
    {(F bsl 35) bor (E bsl 28) bor (D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A, Rest};
decode_unsigned_varint(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 1:1, D:7, 1:1, E:7, 1:1, F:7, 0:1, G:7, Rest/binary>>) ->
    {(G bsl 42) bor (F bsl 35) bor (E bsl 28) bor (D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A, Rest};
decode_unsigned_varint(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 1:1, D:7, 1:1, E:7, 1:1, F:7, 1:1, G:7, 0:1, H:7, Rest/binary>>) ->
    {(H bsl 49) bor (G bsl 42) bor (F bsl 35) bor (E bsl 28) bor (D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A, Rest};
decode_unsigned_varint(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 1:1, D:7, 1:1, E:7, 1:1, F:7, 1:1, G:7, 1:1, H:7, 0:1, I:7, Rest/binary>>) ->
    {(I bsl 56) bor (H bsl 49) bor (G bsl 42) bor (F bsl 35) bor (E bsl 28) bor (D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A, Rest};
decode_unsigned_varint(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 1:1, D:7, 1:1, E:7, 1:1, F:7, 1:1, G:7, 1:1, H:7, 1:1, I:7, 0:1, J:7, Rest/binary>>) ->
    {(J bsl 63) bor (I bsl 56) bor (H bsl 49) bor (G bsl 42) bor (F bsl 35) bor (E bsl 28) bor (D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A, Rest}.

decode_bool(<<0:8/big, Rest/binary>>) -> {false, Rest};
decode_bool(<<1:8/big, Rest/binary>>) -> {true, Rest}.

-spec encode_string(Value :: binary()) -> iodata().

encode_string(Value) when is_binary(Value), byte_size(Value) =< 65536 ->
    Size = byte_size(Value),
    [<<Size:16/big-signed>>, Value].

-spec decode_string(Input :: nonempty_binary()) -> {Value :: binary(), Rest :: binary()}.

decode_string(<<Size:16/big-signed, Value:Size/binary, Rest/binary>>) ->
    {Value, Rest}.

-spec encode_nullable_string(Value :: binary() | null) -> iodata().

encode_nullable_string(Value) when is_binary(Value), byte_size(Value) =< 65536 ->
    Size = byte_size(Value),
    [<<Size:16/big-signed>>, Value];
encode_nullable_string(null) ->
    Size = -1,
    <<Size:16/big-signed>>.

-spec decode_nullable_string(Input :: nonempty_binary()) ->
    {Value :: binary() | null, Rest :: binary()}.

decode_nullable_string(<<Size:16/big-signed, Rest/binary>>) when Size =:= -1 -> {null, Rest};
decode_nullable_string(<<Size:16/big-signed, Value:Size/binary, Rest/binary>>) -> {Value, Rest}.

-spec encode_compact_string(Value :: binary()) -> iodata().

encode_compact_string(Value) when is_binary(Value) ->
    % COMPACT_STRING uses the same encoding as COMPACT_BYTES
    encode_compact_bytes(Value).

-spec decode_compact_string(Input :: nonempty_binary()) -> {Value :: binary(), Rest :: binary()}.

decode_compact_string(Binary) when is_binary(Binary) ->
    % COMPACT_STRING uses the same encoding as COMPACT_BYTES
    decode_compact_bytes(Binary).

-spec encode_compact_nullable_string(Value :: binary() | null) -> iodata().

encode_compact_nullable_string(Value) ->
    % COMPACT_NULLABLE_STRING uses the same encoding as COMPACT_NULLABLE_BYTES
    encode_compact_nullable_bytes(Value).

-spec decode_compact_nullable_string(Input :: nonempty_binary()) ->
    {Value :: binary() | null, Rest :: binary()}.

decode_compact_nullable_string(Binary) ->
    % COMPACT_NULLABLE_STRING uses the same encoding as COMPACT_NULLABLE_BYTES
    decode_compact_nullable_bytes(Binary).

-spec encode_bytes(Value :: binary()) -> iodata().

encode_bytes(Bin) when is_binary(Bin) ->
    Size = byte_size(Bin),
    [<<Size:32/big-signed>>, Bin].

-spec decode_bytes(Input :: nonempty_binary()) -> {Value :: binary(), Rest :: binary()}.

decode_bytes(_Bin = <<Length:32/big-signed, Bytes:Length/binary, Rest/binary>>) ->
    {Bytes, Rest}.

-spec encode_nullable_bytes(Value :: binary() | null) -> iodata().

encode_nullable_bytes(Value) when is_binary(Value) ->
    % Unlike NULLABLE_STRING, NULLABLE_BYTES uses INT32 for the size.
    Size = byte_size(Value),
    [<<Size:32/big-signed>>, Value];
encode_nullable_bytes(null) ->
    Size = -1,
    <<Size:32/big-signed>>.

-spec decode_nullable_bytes(Input :: nonempty_binary()) ->
    {Value :: binary() | null, Rest :: binary()}.

decode_nullable_bytes(<<Size:32/big-signed, Rest/binary>>) when Size =:= -1 -> {null, Rest};
decode_nullable_bytes(<<Size:32/big-signed, Value:Size/binary, Rest/binary>>) -> {Value, Rest}.

-spec encode_compact_bytes(Value :: binary()) -> iodata().

encode_compact_bytes(Value) when is_binary(Value) ->
    Length = byte_size(Value),
    [encode_unsigned_varint(Length + 1), Value].

-spec decode_compact_bytes(Input :: nonempty_binary()) -> {Value :: binary(), Rest :: binary()}.

decode_compact_bytes(Binary) when is_binary(Binary) ->
    {Length1, Rest} = decode_unsigned_varint(Binary),
    Length = Length1 - 1,
    <<Value:Length/binary, More/binary>> = Rest,
    {Value, More}.

-spec encode_compact_nullable_bytes(Value :: binary() | null) -> iodata().

encode_compact_nullable_bytes(Value) when is_binary(Value) ->
    encode_compact_bytes(Value);
encode_compact_nullable_bytes(null) ->
    <<0:8>>.

-spec decode_compact_nullable_bytes(nonempty_binary()) -> {binary() | null, binary()}.

decode_compact_nullable_bytes(<<0:8, Rest/binary>>) ->
    {null, Rest};
decode_compact_nullable_bytes(Binary) ->
    decode_compact_bytes(Binary).

-spec encode_array(Array, Fun) -> iodata() when
    Array :: [Element],
    Fun :: fun((Element) -> iodata()).

encode_array(Array, Encoder) when is_list(Array), is_function(Encoder, 1) ->
    Length = length(Array),
    [<<Length:32/big-signed>>, [Encoder(Item) || Item <- Array]].

%% decode_array takes 'Input', a binary with a length prefix, and 'Fun', a function which pulls an element from the
%% head of the binary and returns the remaining binary. decode_array then returns the decoded array and the left-over
%% binary.
-spec decode_array(Input, Fun) -> {Array, Rest} when
    Input :: nonempty_binary(),
    Fun :: fun((Bin :: binary()) -> {Element, Rest}),
    Array :: [Element],
    Rest :: binary().

decode_array(_Bin = <<N:32/big-signed, Rest/binary>>, Fun) ->
    decode_array(N, Rest, Fun, []).

-spec decode_array(Count, Input, Fun, Acc) -> {Acc, Rest} when
    Count :: non_neg_integer(),
    Input :: binary(),
    Fun :: fun((Bin :: binary()) -> {Element, Rest}),
    Acc :: Array,
    Array :: [Element],
    Rest :: binary().

decode_array(N, Bin, Fun, Acc) when N > 0 ->
    {Elem, Rest} = Fun(Bin),
    decode_array(N - 1, Rest, Fun, Acc ++ [Elem]);
decode_array(N, Bin, _Fun, Acc) when N == 0 ->
    {Acc, Bin}.

-spec encode_nullable_array(Array | null, Fun) -> iodata() when
    Array :: [Element],
    Fun :: fun((Element) -> iodata()).

encode_nullable_array(null, _Encoder) ->
    % A null array is represented with a length of -1.
    Length = -1,
    [<<Length:32/big-signed>>];
encode_nullable_array(Array, Encoder) when is_list(Array), is_function(Encoder, 1) ->
    encode_array(Array, Encoder).

-spec decode_nullable_array(Input, Fun) -> {Array | null, Rest} when
    Input :: nonempty_binary(),
    Fun :: fun((Bin :: binary()) -> {Element, Rest}),
    Array :: [Element],
    Rest :: binary().

decode_nullable_array(_Bin = <<Length:32/big-signed, Rest/binary>>, _Fun) when Length =:= -1 ->
    {null, Rest};
decode_nullable_array(Bin, Fun) ->
    decode_array(Bin, Fun).

-spec encode_compact_array(Array, Fun) -> iodata() when
    Array :: [Element],
    Fun :: fun((Element) -> iodata()).

encode_compact_array(Array, Encoder) when is_list(Array), is_function(Encoder, 1) ->
    Length = length(Array),
    [encode_unsigned_varint(Length + 1), [Encoder(Item) || Item <- Array]].

-spec encode_compact_nullable_array(Array | null, Fun) -> iodata() when
    Array :: [Element],
    Fun :: fun((Element) -> iodata()).

encode_compact_nullable_array(null, _Encoder) ->
    % A null array is represented with a length of 0.
    [<<0:8/big>>];
encode_compact_nullable_array(Array, Encoder) when is_list(Array), is_function(Encoder, 1) ->
    encode_compact_array(Array, Encoder).

-spec decode_compact_nullable_array(Input, Fun) -> {Array | null, Rest} when
    Input :: nonempty_binary(),
    Fun :: fun((Bin :: binary()) -> {Element, Rest}),
    Array :: [Element],
    Rest :: binary().

decode_compact_nullable_array(Bin, Fun) when is_binary(Bin), is_function(Fun, 1) ->
    % COMPACT_ARRAY can always be nullable, according to the documentation. The schema files don't necessarily agree,
    % but until we figure that out, we just have them be the same. The encoder (above), on the other hand, _does_ make a
    % distinction, so that you don't accidentally encode a null where the schema says you shouldn't.
    decode_compact_array(Bin, Fun).

-spec decode_compact_array(Input, Fun) -> {Array, Rest} when
    Input :: nonempty_binary(),
    Fun :: fun((Bin :: binary()) -> {Element, Rest}),
    Array :: [Element],
    Rest :: binary().

decode_compact_array(Bin, Fun) when is_binary(Bin), is_function(Fun, 1) ->
    % First, the length N + 1 is given as an UNSIGNED_VARINT.
    {N1, Rest} = decode_unsigned_varint(Bin),
    decode_compact_array(N1, Rest, Fun).

-spec decode_compact_array(N, Input, Fun) -> {Array | null, Rest} when
    N :: non_neg_integer(),
    Input :: binary(),
    Fun :: fun((Bin :: binary()) -> {Element, Rest}),
    Array :: [Element],
    Rest :: binary().

decode_compact_array(N1, Rest, Fun) when N1 > 0 ->
    % Then N instances of type T follow.
    decode_compact_array(N1 - 1, Rest, Fun, []);
decode_compact_array(N1, Rest, _Fun) when N1 =:= 0 ->
    % A null array is represented with a length of 0.
    {null, Rest}.

-spec decode_compact_array(N, Input, Fun, Acc) -> {Array | null, Rest} when
    N :: non_neg_integer(),
    Input :: binary(),
    Fun :: fun((Bin :: binary()) -> {Element, Rest}),
    Acc :: Array,
    Array :: [Element],
    Rest :: binary().

decode_compact_array(N, Bin, Fun, Acc) when N > 0 ->
    {Elem, Rest} = Fun(Bin),
    decode_compact_array(N - 1, Rest, Fun, Acc ++ [Elem]);
decode_compact_array(N, Bin, _Fun, Acc) when N == 0 ->
    {Acc, Bin}.

-spec encode_uuid(kafcod:uuid()) -> binary().

encode_uuid(<<A:8/binary, "-", B:4/binary, "-", C:4/binary, "-", D:4/binary, "-", E:12/binary>>) ->
    binary:decode_hex(<<A/binary, B/binary, C/binary, D/binary, E/binary>>).

-spec decode_uuid(Input :: binary()) -> {Value :: kafcod:uuid(), Rest :: binary()}.

decode_uuid(Bin = <<Value:16/binary, Rest/binary>>) when is_binary(Bin) ->
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = string:lowercase(
        binary:encode_hex(Value)
    ),
    {<<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>, Rest}.

-spec encode_tagged_fields(Fun, Args) -> iodata() when
    Fun :: fun((Key, Value) -> {Tag, Data}),
    Args :: map(),
    Key :: atom(),
    Value :: binary(),
    Tag :: non_neg_integer(),
    Data :: binary().

encode_tagged_fields(Fun, Args) when is_function(Fun, 2), is_map(Args) ->
    {_, Count, EncodedFields} = maps:fold(fun encode_tagged_field/3, {Fun, 0, []}, Args),
    [
        encode_unsigned_varint(Count),
        EncodedFields
    ].

-spec encode_tagged_field(Key, Value, Acc) -> Acc when
    Key :: atom(),
    Value :: binary(),
    Acc :: {Fun, Count, Result},
    Fun :: fun((Key, Value) -> {Tag, Data} | ignore),
    Count :: non_neg_integer(),
    Result :: iodata(),
    Tag :: non_neg_integer(),
    Data :: iodata().

encode_tagged_field(Key, Value, _Acc = {Fun, Count, ResultAcc}) ->
    case Fun(Key, Value) of
        {Tag, Data} when is_integer(Tag) ->
            Len = iolist_size(Data),
            {Fun, Count + 1, [
                ResultAcc, [encode_unsigned_varint(Tag), encode_unsigned_varint(Len), Data]
            ]};
        ignore ->
            {Fun, Count, ResultAcc}
    end.

-spec decode_tagged_fields(Fun, Acc0, Bin) -> {Acc1, Rest} when
    Fun :: fun((Tag, Value, AccIn) -> AccOut),
    Tag :: non_neg_integer(),
    Value :: binary(),
    Bin :: binary(),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Rest :: binary().

decode_tagged_fields(Fun, Acc, Bin) when is_function(Fun, 3), is_binary(Bin) ->
    % Note: no guard on 'Acc', because *this* function doesn't actually care about the type; it depends on 'Fun'.
    {Count, Rest} = decode_unsigned_varint(Bin),
    decode_tagged_fields(Fun, Count, Acc, Rest).

-spec decode_tagged_fields(Fun, Count, Acc0, Input) -> {Acc1, Rest} when
    Fun :: fun((Tag, Value, AccIn) -> AccOut),
    Count :: non_neg_integer(),
    Input :: binary(),
    Tag :: non_neg_integer(),
    Value :: binary(),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Rest :: binary().

decode_tagged_fields(Fun, Count, Acc, Bin0) when Count > 0 ->
    {Tag, Bin1} = decode_unsigned_varint(Bin0),
    {Len, Bin2} = decode_unsigned_varint(Bin1),
    <<Value:Len/binary, Bin3/binary>> = Bin2,
    Acc2 = Fun(Tag, Value, Acc),
    decode_tagged_fields(Fun, Count - 1, Acc2, Bin3);
decode_tagged_fields(_Fun, _Count = 0, Acc, Bin) ->
    {Acc, Bin}.
