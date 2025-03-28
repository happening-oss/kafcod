-module(kafcod_record_batch).

% "records" are collected into "record batches". The per-partition data in a Produce request or a Fetch response
% contains zero or more record batches. Confusingly, this is referred to with the type name "RECORDS" or
% "COMPACT_RECORDS".
%
% Here's some pseudo-grammar:
%
% fetch_response :: {..., [fetchable_topic_response[T], ...]}
%   fetchable_topic_response :: {..., [partition_data[P], ...]}
%     partition_data :: {..., RECORDS}
%
% RECORDS :: [record_batch, ...]
%   record_batch :: {..., [record, ...]}
%     record :: {key, value, headers}

-export([
    encode_record_batch/1,
    decode_record_batch/1
]).
-export_type([
    record_batch/0,
    batch_attributes/0
]).

% Exported for testing. See kafcod_record_batch_tests.
-ifdef(TEST).
-export([
    encode_records/1,
    decode_records/2
]).
-endif.
-include("guards.hrl").
-include("error.hrl").
-include("compression.hrl").

-define(CRC(X), kafcod_crc32c:value(X)).
%-define(CRC(X), kafcod_crc32c_erl:value(X)).

% We don't handle any magic older than v2.
-define(EXPECTED_MAGIC, 2).

-type compression() :: none | gzip | snappy | lz4 | zstd.

-type batch_attributes() :: #{
    compression := compression()
}.

-type record_batch() :: #{
    base_offset := non_neg_integer(),
    partition_leader_epoch := integer(),
    magic := ?EXPECTED_MAGIC,
    % CRC is calculated by us, so it's optional.
    crc => non_neg_integer(),
    attributes := batch_attributes(),
    last_offset_delta := non_neg_integer(),
    % TODO: introduce type aliases for timestamps/offsets?
    base_timestamp := integer(),
    max_timestamp := integer(),
    producer_id := integer(),
    producer_epoch := integer(),
    base_sequence := integer(),
    records := [kafcod_record:record()]
}.

-spec encode_record_batch(record_batch()) -> iodata().

encode_record_batch(
    _Args = #{
        base_offset := BaseOffset,
        partition_leader_epoch := PartitionLeaderEpoch,
        magic := Magic = ?EXPECTED_MAGIC,
        attributes := Attributes0,
        last_offset_delta := LastOffsetDelta,
        base_timestamp := BaseTimestamp,
        max_timestamp := MaxTimestamp,
        producer_id := ProducerId,
        producer_epoch := ProducerEpoch,
        base_sequence := BaseSequence,
        records := Records
    }
) when
    ?is_int64(BaseOffset),
    ?is_int32(PartitionLeaderEpoch),
    ?is_int8(Magic),
    is_map(Attributes0),
    ?is_int32(LastOffsetDelta),
    ?is_int64(BaseTimestamp),
    ?is_int64(MaxTimestamp),
    ?is_int16(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_int32(BaseSequence),
    ?is_array(Records)
->
    Attributes = encode_batch_attributes(Attributes0),
    Prefix = [
        <<Attributes:16/big, LastOffsetDelta:32/big-signed, BaseTimestamp:64/big-signed,
            MaxTimestamp:64/big-signed, ProducerId:64/big-signed, ProducerEpoch:16/big-signed,
            BaseSequence:32/big-signed>>
    ],

    #{compression := Compression} = Attributes0,
    EncodedRecords = compress_records(Compression, Records),

    CRC = ?CRC([Prefix, EncodedRecords]),
    Batch = [
        <<PartitionLeaderEpoch:32/big-signed, Magic:8/big-signed>>,
        <<CRC:32/big>>,
        [Prefix, EncodedRecords]
    ],
    BatchLength = iolist_size(Batch),
    [<<BaseOffset:64/big-signed, BatchLength:32/big-signed>>, Batch];
encode_record_batch(Args) ->
    ?encoder_error(Args, #{
        base_offset => int64,
        partition_leader_epoch => int32,
        magic => int8,
        attributes => map,
        last_offset_delta => int32,
        base_timestamp => int64,
        max_timestamp => int64,
        producer_id => int64,
        producer_epoch => int16,
        base_sequence => int32,
        records => {array, record}
    }).

encode_batch_attributes(_Attributes = #{compression := Compression}) ->
    encode_compression_attribute(Compression).

encode_compression_attribute(none) -> ?COMPRESSION_NONE;
encode_compression_attribute(gzip) -> ?COMPRESSION_GZIP;
encode_compression_attribute(snappy) -> ?COMPRESSION_SNAPPY;
encode_compression_attribute(lz4) -> ?COMPRESSION_LZ4;
encode_compression_attribute(zstd) -> ?COMPRESSION_ZSTD;
encode_compression_attribute(Compression) -> error(badarg, [Compression]).

compress_records(_Compression = none, Records) ->
    Count = length(Records),
    [<<Count:32/big-signed>>, encode_records(Records)];
compress_records(_Compression = gzip, Records) ->
    Count = length(Records),
    EncodedRecords = iolist_to_binary(encode_records(Records)),
    CompressedRecords = zlib:gzip(EncodedRecords),
    telemetry:execute([kafcod, record_batch, compress_records], #{
        compression => gzip,
        uncompressed_byte_size => byte_size(EncodedRecords),
        compressed_byte_size => iolist_size(CompressedRecords)
    }),
    [<<Count:32/big-signed>>, CompressedRecords];
compress_records(_Compression = snappy, Records) ->
    Count = length(Records),
    EncodedRecords = iolist_to_binary(encode_records(Records)),
    {ok, CompressedRecords} = kafcod_snappy:compress(EncodedRecords),
    telemetry:execute([kafcod, record_batch, compress_records], #{
        compression => snappy,
        uncompressed_byte_size => byte_size(EncodedRecords),
        compressed_byte_size => iolist_size(CompressedRecords)
    }),
    [<<Count:32/big-signed>>, CompressedRecords].

-spec encode_records([kafcod_record:record()]) -> iodata().

encode_records(Records) ->
    [kafcod_record:encode_record(Record) || Record <- Records].

% The documentation for a Record Batch is at https://kafka.apache.org/documentation/#recordbatch
-spec decode_record_batch(nonempty_binary()) ->
    {Batch :: record_batch(), Rest :: binary()} | truncated.

decode_record_batch(
    <<BaseOffset:64/big-signed, BatchLength:32/big-signed, Batch:BatchLength/binary, Rest/binary>>
) ->
    <<PartitionLeaderEpoch:32/big-signed, Magic:8/big-signed, CRC:32/big-unsigned, Data/binary>> =
        Batch,

    % Assert that the magic and the CRC are correct.
    ?EXPECTED_MAGIC = Magic,
    CRC = ?CRC(Data),

    <<Attributes0:16/big, LastOffsetDelta:32/big-signed, BaseTimestamp:64/big-signed,
        MaxTimestamp:64/big-signed, ProducerId:64/big-signed, ProducerEpoch:16/big-signed,
        BaseSequence:32/big-signed, RecordCount:32/big-signed, EncodedRecords/binary>> = Data,

    Attributes = decode_batch_attributes(Attributes0),
    #{compression := Compression} = Attributes,

    Records = decompress_records(Compression, RecordCount, EncodedRecords),
    RecordBatch = #{
        base_offset => BaseOffset,
        partition_leader_epoch => PartitionLeaderEpoch,
        magic => Magic,
        crc => CRC,
        attributes => Attributes,
        last_offset_delta => LastOffsetDelta,
        base_timestamp => BaseTimestamp,
        max_timestamp => MaxTimestamp,
        producer_id => ProducerId,
        producer_epoch => ProducerEpoch,
        base_sequence => BaseSequence,
        records => Records
    },
    {RecordBatch, Rest};
decode_record_batch(_) ->
    truncated.

-spec decode_batch_attributes(non_neg_integer()) -> #{compression := compression()}.

decode_batch_attributes(Attributes) ->
    <<_:9, _HasDeleteHorizonMs:1, _IsControlBatch:1, _IsTransactional:1, _TimestampType:1,
        Compression:3>> = <<Attributes:16/big-signed>>,
    #{compression => decode_compression_attribute(Compression)}.

decode_compression_attribute(?COMPRESSION_NONE) -> none;
decode_compression_attribute(?COMPRESSION_GZIP) -> gzip;
decode_compression_attribute(?COMPRESSION_SNAPPY) -> snappy;
decode_compression_attribute(?COMPRESSION_LZ4) -> lz4;
decode_compression_attribute(?COMPRESSION_ZSTD) -> zstd.

-spec decompress_records(
    Compression :: compression(), Count :: integer(), Input :: nonempty_binary()
) ->
    [kafcod_record:record()].

decompress_records(_Compression = none, Count, Records) when
    is_integer(Count), is_binary(Records)
->
    decode_records(Count, Records);
decompress_records(_Compression = gzip, Count, CompressedRecords) when
    is_integer(Count), is_binary(CompressedRecords)
->
    Records = zlib:gunzip(CompressedRecords),
    telemetry:execute([kafcod, record_batch, decompress_records], #{
        compression => gzip,
        compressed_byte_size => byte_size(CompressedRecords),
        decompressed_byte_size => byte_size(Records)
    }),
    decode_records(Count, Records);
decompress_records(_Compression = snappy, Count, CompressedRecords) when
    is_integer(Count), is_binary(CompressedRecords)
->
    {ok, Records} = kafcod_snappy:decompress(CompressedRecords),
    telemetry:execute([kafcod, record_batch, decompress_records], #{
        compression => snappy,
        compressed_byte_size => byte_size(CompressedRecords),
        decompressed_byte_size => byte_size(Records)
    }),
    decode_records(Count, Records);
decompress_records(Compression, Count, Records) ->
    error(badarg, [Compression, Count, Records]).

-spec decode_records(Count :: non_neg_integer(), Input :: binary()) -> [kafcod_record:record()].

decode_records(Count, Records) ->
    decode_records(Count, Records, []).

-spec decode_records(Count :: non_neg_integer(), Input :: binary(), AccIn) -> AccOut when
    AccIn :: [kafcod_record:record()], AccOut :: [kafcod_record:record()].

decode_records(Count, Records, Acc) when Count > 0 ->
    {Record, Rest} = kafcod_record:decode_record(Records),
    decode_records(Count - 1, Rest, [Record | Acc]);
decode_records(_Count = 0, <<>>, Acc) ->
    lists:reverse(Acc).
