-module(kafcod_records).
-export([
    decode_records/1,
    encode_records/1,

    decode_compact_records/1,
    encode_compact_records/1
]).
-export_type([
    records/0
]).

% Exported for testing. See kafcod_records_tests.
-ifdef(TEST).
-export([
    decode_record_batches/1,
    encode_record_batches/1
]).
-endif.

% It looks like a FetchRequest returns a message set (that's what Wireshark calls it, so we'll go with that) containing
% zero or more record batches and each batch contains one or more records.

-type records() :: [kafcod_record_batch:record_batch()].

-spec decode_records(Input :: nonempty_binary()) ->
    {[kafcod_record_batch:record_batch()], Rest :: binary()}.

decode_records(<<Length:32/big-signed, EncodedRecordBatches:Length/binary, Rest/binary>>) ->
    telemetry:execute([kafcod, records, decode_records], #{byte_size => Length}),
    RecordBatches = decode_record_batches(EncodedRecordBatches),
    {RecordBatches, Rest}.

-spec decode_compact_records(Input :: nonempty_binary()) ->
    {[kafcod_record_batch:record_batch()], Rest :: binary()}.

decode_compact_records(<<0:8, Rest/binary>>) ->
    {null, Rest};
decode_compact_records(Input) ->
    {Length1, Bin1} = kafcod_primitives:decode_unsigned_varint(Input),
    Length = Length1 - 1,
    telemetry:execute([kafcod, records, decode_records], #{byte_size => Length}),
    <<EncodedRecordBatches:Length/binary, Rest/binary>> = Bin1,
    RecordBatches = decode_record_batches(EncodedRecordBatches),
    {RecordBatches, Rest}.

-spec decode_record_batches(binary()) -> [kafcod_record_batch:record_batch()].

decode_record_batches(RecordBatches) when is_binary(RecordBatches) ->
    decode_record_batches(RecordBatches, []).

-spec decode_record_batches(Input :: binary(), Acc :: [kafcod_record_batch:record_batch()]) ->
    [kafcod_record_batch:record_batch()].

decode_record_batches(<<>>, Acc) ->
    lists:reverse(Acc);
decode_record_batches(RecordBatches, Acc) ->
    case kafcod_record_batch:decode_record_batch(RecordBatches) of
        {RecordBatch, Rest} when is_map(RecordBatch), is_binary(Rest) ->
            decode_record_batches(Rest, [RecordBatch | Acc]);
        truncated ->
            lists:reverse(Acc)
    end.

-spec encode_records([kafcod_record_batch:record_batch()]) -> iodata().

encode_records(RecordBatches) when is_list(RecordBatches) ->
    EncodedRecordBatches = encode_record_batches(RecordBatches),
    Length = iolist_size(EncodedRecordBatches),
    telemetry:execute([kafcod, records, encode_records], #{byte_size => Length}),
    [<<Length:32/big-signed>>, EncodedRecordBatches].

-spec encode_compact_records([kafcod_record_batch:record_batch()]) -> iodata().

encode_compact_records(RecordBatches) when is_list(RecordBatches) ->
    EncodedRecordBatches = encode_record_batches(RecordBatches),
    Length = iolist_size(EncodedRecordBatches),
    telemetry:execute([kafcod, records, encode_records], #{byte_size => Length}),
    [kafcod_primitives:encode_unsigned_varint(Length + 1), EncodedRecordBatches].

-spec encode_record_batches([kafcod_record_batch:record_batch()]) -> iodata().

encode_record_batches(RecordBatches) ->
    [kafcod_record_batch:encode_record_batch(RecordBatch) || RecordBatch <- RecordBatches].
