-module(kafcod_message_set).
-export([
    prepare_message_set/1,
    prepare_message_set/2,

    uncompressed_size/1,
    uncompressed_size_init/0,
    uncompressed_size_add/2
]).

-export_type([
    headers/0,
    message/0,

    uncompressed_size_acc/0
]).

% Per KIP-82, "duplicate headers with the same key must be supported.", so it's a list of KV.
-type headers() :: [{Key :: binary(), Value :: binary() | null}].
-type message() :: #{
    timestamp := non_neg_integer(),
    key := binary() | null,
    value := binary() | null,
    headers := headers()
}.

-type uncompressed_size_acc() :: {
    Size :: non_neg_integer(),
    BaseTimestamp :: non_neg_integer() | undefined,
    OffsetDelta :: non_neg_integer()
}.

-spec prepare_message_set(Messages :: nonempty_list(message())) ->
    [kafcod_record_batch:record_batch()].

prepare_message_set(Messages) ->
    prepare_message_set(#{}, Messages).

-spec prepare_message_set(
    BatchAttributes :: #{compression => kafcod_record_batch:compression()},
    Messages :: nonempty_list(message())
) ->
    [kafcod_record_batch:record_batch()].

prepare_message_set(BatchAttributes, Messages = [_ | _]) when
    is_map(BatchAttributes), is_list(Messages)
->
    DefaultBatchAttributes = #{compression => none},
    BatchAttributes1 = maps:merge(DefaultBatchAttributes, BatchAttributes),
    [
        % According to the Kafka source, v3.8.1,
        % clients/src/main/java/org/apache/kafka/common/requests/ProduceRequest.java, line 227-254, validateRecords(),
        % Produce requests must have exactly one record batch per partition.
        prepare_record_batch(BatchAttributes1, Messages)
    ];
prepare_message_set(BatchAttributes, Messages) ->
    error(badarg, [BatchAttributes, Messages]).

-spec prepare_record_batch(
    BatchAttributes :: kafcod_record_batch:batch_attributes(),
    Messages :: nonempty_list(message())
) ->
    kafcod_record_batch:record_batch().

prepare_record_batch(BatchAttributes, Messages = [#{timestamp := BaseTimestamp} | _]) when
    is_map(BatchAttributes), is_list(Messages), is_integer(BaseTimestamp), BaseTimestamp >= 0
->
    {Records, LastOffsetDelta} = prepare_records(Messages, BaseTimestamp),
    MaxTimestamp = lists:foldl(
        fun(#{timestamp := Timestamp}, Max) ->
            case Timestamp > Max of
                true -> Timestamp;
                false -> Max
            end
        end,
        BaseTimestamp,
        Messages
    ),
    #{
        % When producing, we don't know the offset, so it's always zero.
        base_offset => 0,
        % While we _could_ get the partition leader epoch from the metadata, kafire doesn't bother. Is that a problem?
        partition_leader_epoch => 0,
        magic => 2,
        attributes => BatchAttributes,
        last_offset_delta => LastOffsetDelta,
        base_timestamp => BaseTimestamp,
        max_timestamp => MaxTimestamp,
        producer_id => -1,
        producer_epoch => -1,
        base_sequence => -1,
        records => Records
    }.

-spec prepare_records(Messages :: nonempty_list(message()), BaseTimestamp :: non_neg_integer()) ->
    {Records :: [kafcod_record:record()], LastOffsetDelta :: non_neg_integer()}.

prepare_records(Messages = [_ | _], BaseTimestamp) when is_list(Messages) ->
    OffsetDelta0 = 0,
    {Records, OffsetDelta} = lists:mapfoldl(
        fun(M, D) -> prepare_record(M, D, BaseTimestamp) end,
        OffsetDelta0,
        Messages
    ),
    % OffsetDelta is the offset of the *next* record; we want the most-recent, so decrement it.
    {Records, OffsetDelta - 1}.

%% Convert a message -- Key, Value, Headers -- into a record. Called from lists:mapfoldl. Returns the record (for the
%% map) and the next offset delta (for the fold).

-spec prepare_record(
    Message :: message(), OffsetDelta :: non_neg_integer(), BaseTimestamp :: non_neg_integer()
) ->
    {Record :: kafcod_record:record(), NextOffsetDelta :: non_neg_integer()}.

prepare_record(
    _Message = #{timestamp := Timestamp, key := Key, value := Value, headers := Headers},
    OffsetDelta,
    BaseTimestamp
) when
    is_integer(Timestamp),
    Timestamp >= 0,
    is_binary(Key) orelse Key =:= null,
    is_binary(Value) orelse Value =:= null,
    is_list(Headers)
->
    {
        #{
            % Record attributes are always zero.
            attributes => 0,
            % This can result in a negative timestamp delta. AFAICT that can also happen in the
            % official java client, and timestamp delta is a signed varlong, so this seems fine
            timestamp_delta => Timestamp - BaseTimestamp,
            offset_delta => OffsetDelta,
            key => Key,
            value => Value,
            headers => Headers
        },
        OffsetDelta + 1
    };
prepare_record(Message, OffsetDelta, BaseTimestamp) ->
    error(badarg, [Message, OffsetDelta, BaseTimestamp]).

-spec uncompressed_size(Messages :: [message()]) -> non_neg_integer().

uncompressed_size(Messages) when is_list(Messages) ->
    {Size, _, _} =
        lists:foldl(
            fun uncompressed_size_add/2,
            uncompressed_size_init(),
            Messages
        ),
    Size.

-spec uncompressed_size_init() -> uncompressed_size_acc().

uncompressed_size_init() ->
    % baseOffset: int64 - 8 bytes
    % batchLength: int32 - 4 bytes
    % partitionLeaderEpoch: int32 - 4 bytes
    % magic: int8 (current magic value is 2) - 1 byte
    % crc: uint32 - 4 bytes
    % attributes: int16 - 2 bytes
    %     bit 0~2:
    %         0: no compression
    %         1: gzip
    %         2: snappy
    %         3: lz4
    %         4: zstd
    %     bit 3: timestampType
    %     bit 4: isTransactional (0 means not transactional)
    %     bit 5: isControlBatch (0 means not a control batch)
    %     bit 6: hasDeleteHorizonMs (0 means baseTimestamp is not set as the delete horizon for compaction)
    %     bit 7~15: unused
    % lastOffsetDelta: int32 - 4 bytes
    % baseTimestamp: int64 - 8 bytes
    % maxTimestamp: int64 - 8 bytes
    % producerId: int64 - 8 bytes
    % producerEpoch: int16 - 2 bytes
    % baseSequence: int32 - 4 bytes
    % recordsCount: int32 - 4 bytes
    % records: [Record]
    % 8 + 4 + 4 + 1 + 4 + 2 + 4 + 8 + 8 + 8 + 2 + 4 + 4 = 61 bytes + size of records
    {61, undefined, 0}.

-spec uncompressed_size_add(Message :: message(), Acc :: uncompressed_size_acc()) ->
    uncompressed_size_acc().

uncompressed_size_add(Message = #{timestamp := Timestamp}, {CurrentTotal, undefined, 0}) ->
    % Extract the timestamp of the first message for use as the base timestamp
    uncompressed_size_add(Message, {CurrentTotal, Timestamp, 0});
uncompressed_size_add(Message, {CurrentTotal, BaseTimestamp, OffsetDelta}) ->
    RecordSize = uncompressed_record_size(Message, BaseTimestamp, OffsetDelta),
    {CurrentTotal + RecordSize, BaseTimestamp, OffsetDelta + 1}.

uncompressed_record_size(
    #{timestamp := Timestamp, key := Key, value := Value, headers := Headers},
    BaseTimestamp,
    OffsetDelta
) ->
    % length: varint
    % attributes: int8 - 1 byte
    % timestampDelta: varlong - 1-10 bytes
    % offsetDelta: varint - 1-5 bytes
    % keyLength: varint - 1-5 bytes
    % key: byte[] - byte_size(Key) bytes
    % valueLength: varint - 1-5 bytes
    % value: byte[] - byte_size(Value) bytes
    % headersCount: varint - 1-5 bytes
    % Headers => [Header]
    BodySize =
        1 +
            kafcod_primitives:sizeof_signed_varint(Timestamp - BaseTimestamp) +
            kafcod_primitives:sizeof_signed_varint(OffsetDelta) +
            length_and_bin_size(Key) +
            length_and_bin_size(Value) +
            kafcod_primitives:sizeof_signed_varint(length(Headers)) +
            lists:sum([uncompressed_header_size(Header) || Header <- Headers]),
    kafcod_primitives:sizeof_signed_varint(BodySize) + BodySize.

uncompressed_header_size({Key, Value}) ->
    % headerKeyLength: varint - 1-5 bytes
    % headerKey: String - byte_size(Key) bytes
    % headerValueLength: varint - 1-5 bytes
    % Value: byte[] - byte_size(Value) bytes
    length_and_bin_size(Key) + length_and_bin_size(Value).

length_and_bin_size(null) ->
    1;
length_and_bin_size(Bin) when is_binary(Bin) ->
    kafcod_primitives:sizeof_signed_varint(byte_size(Bin)) + byte_size(Bin).
