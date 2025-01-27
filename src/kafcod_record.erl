-module(kafcod_record).
-export([
    decode_record/1,
    encode_record/1
]).
-export_type([record/0]).

% Exported for testing. See kafcod_record_tests.
-ifdef(TEST).
-export([
    encode_headers/1
]).
-endif.

-include("guards.hrl").
-include("error.hrl").

-type record() :: #{
    attributes => non_neg_integer(),
    timestamp_delta => non_neg_integer(),
    offset_delta => non_neg_integer(),
    key => binary() | null,
    value => binary() | null,
    headers => [header()]
}.
-type header() :: {Key :: binary(), Value :: binary() | null}.

-spec decode_record(nonempty_binary()) -> {record(), Rest :: binary()}.

decode_record(Bin0) ->
    {_Length, Bin1} = kafcod_primitives:decode_signed_varint(Bin0),
    <<Attributes:8/big-signed, Bin2/binary>> = Bin1,
    {TimestampDelta, Bin3} = kafcod_primitives:decode_signed_varint(Bin2),
    {OffsetDelta, Bin4} = kafcod_primitives:decode_signed_varint(Bin3),
    % Key and Value are byte[]
    {Key, Bin5} = decode_bytes(Bin4),
    {Value, Bin6} = decode_bytes(Bin5),
    {Headers, Bin7} = decode_headers(Bin6),

    {
        #{
            attributes => Attributes,
            timestamp_delta => TimestampDelta,
            offset_delta => OffsetDelta,
            key => Key,
            value => Value,
            headers => Headers
        },
        Bin7
    }.

-spec encode_record(record()) -> iodata().

encode_record(
    _Args = #{
        attributes := Attributes,
        timestamp_delta := TimestampDelta,
        offset_delta := OffsetDelta,
        key := Key,
        value := Value,
        headers := Headers
    }
) when
    ?is_int8(Attributes),
    is_integer(TimestampDelta),
    is_integer(OffsetDelta),
    ?is_nullable_bytes(Key),
    ?is_nullable_bytes(Value),
    ?is_array(Headers)
->
    Record = [
        <<Attributes:8/big-signed>>,
        kafcod_primitives:encode_signed_varint(TimestampDelta),
        kafcod_primitives:encode_signed_varint(OffsetDelta),
        encode_bytes(Key),
        encode_bytes(Value),
        encode_headers(Headers)
    ],
    Length = iolist_size(Record),
    [
        kafcod_primitives:encode_signed_varint(Length),
        Record
    ];
encode_record(Args) ->
    ?encoder_error(
        Args, #{
            attributes => int8,
            timestamp_delta => varint,
            offset_delta => varint,
            key => nullable_bytes,
            value => nullable_bytes,
            headers => {array, header}
        }
    ).

% Per KIP-82, "duplicate headers with the same key must be supported.", so it's a list of KV.
% The same KIP says that the order must be preserved, but that's probably going to require PropEr.
-spec encode_headers([header()]) -> iodata().

encode_headers(Headers) when is_list(Headers) ->
    Count = length(Headers),
    [
        kafcod_primitives:encode_signed_varint(Count),
        [encode_header(Key, Value) || {Key, Value} <- Headers]
    ].

% Looking in the Kafka source code (clients/src/main/java/org/apache/kafka/common/header/internals/RecordHeader.java),
% we see that the header key MUST NOT be null, but that the value MAY be null.
-spec encode_header(Key :: binary(), Value :: binary() | null) -> iodata().

encode_header(Key, Value) when ?is_string(Key), ?is_nullable_bytes(Value) ->
    [encode_bytes(Key), encode_bytes(Value)];
encode_header(Key, Value) ->
    ?encoder_error(#{key => Key, value => Value}, #{
        key => string, value => nullable_bytes
    }).

-spec decode_headers(Input :: nonempty_binary()) -> {Headers :: [header()], Rest :: binary()}.

decode_headers(Bin) when is_binary(Bin) ->
    {Count, Bin1} = kafcod_primitives:decode_signed_varint(Bin),
    decode_headers(Count, Bin1, []).

-spec decode_headers(Count :: non_neg_integer(), Input :: binary(), AccIn) ->
    {Headers, Rest :: binary()}
when
    AccIn :: [header()], Headers :: [header()].

decode_headers(Count, Bin, Acc) when Count > 0 ->
    % Key is a string, Value is byte[].
    {Header, Bin1} = decode_header(Bin),
    decode_headers(Count - 1, Bin1, [Header | Acc]);
decode_headers(_Count = 0, Bin, Acc) ->
    {lists:reverse(Acc), Bin}.

decode_header(Bin) ->
    {Key, Bin1} = decode_bytes(Bin),
    {Value, Bin2} = decode_bytes(Bin1),
    {{Key, Value}, Bin2}.

-spec encode_bytes(binary() | null) -> iodata().

encode_bytes(null) ->
    <<1:8/big-signed>>;
encode_bytes(Bytes) when is_binary(Bytes) ->
    [
        kafcod_primitives:encode_signed_varint(byte_size(Bytes)),
        Bytes
    ].

-spec decode_bytes(nonempty_binary()) -> {Value :: binary() | null, Rest :: binary()}.

decode_bytes(<<1:8/big-signed, Bin1/binary>>) ->
    % Optimization: null is encoded as a signed varint, -1, which encodes as a <<1>> byte. Rather than go through
    % decode_signed_varint/decode_unsigned_varint/decode_zigzag, let's just skip that.
    {null, Bin1};
decode_bytes(Bin) when is_binary(Bin) ->
    % Encoded differently from COMPACT_NULLABLE_BYTES, so we can't reuse that code.
    case kafcod_primitives:decode_signed_varint(Bin) of
        {-1, Bin1} ->
            {null, Bin1};
        {Length, Bin1} ->
            <<Value:Length/binary, Bin2/binary>> = Bin1,
            {Value, Bin2}
    end.
