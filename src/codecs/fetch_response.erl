-module(fetch_response).
-export([
    encode_fetch_response_0/1,
    decode_fetch_response_0/1,
    encode_fetch_response_1/1,
    decode_fetch_response_1/1,
    encode_fetch_response_2/1,
    decode_fetch_response_2/1,
    encode_fetch_response_3/1,
    decode_fetch_response_3/1,
    encode_fetch_response_4/1,
    decode_fetch_response_4/1,
    encode_fetch_response_5/1,
    decode_fetch_response_5/1,
    encode_fetch_response_6/1,
    decode_fetch_response_6/1,
    encode_fetch_response_7/1,
    decode_fetch_response_7/1,
    encode_fetch_response_8/1,
    decode_fetch_response_8/1,
    encode_fetch_response_9/1,
    decode_fetch_response_9/1,
    encode_fetch_response_10/1,
    decode_fetch_response_10/1,
    encode_fetch_response_11/1,
    decode_fetch_response_11/1,
    encode_fetch_response_12/1,
    decode_fetch_response_12/1,
    encode_fetch_response_13/1,
    decode_fetch_response_13/1,
    encode_fetch_response_14/1,
    decode_fetch_response_14/1,
    encode_fetch_response_15/1,
    decode_fetch_response_15/1,
    encode_fetch_response_16/1,
    decode_fetch_response_16/1
]).
-export_type([
    fetch_response_0/0,
    partition_data_0/0,
    fetchable_topic_response_0/0,
    fetch_response_1/0,
    partition_data_1/0,
    fetchable_topic_response_1/0,
    fetch_response_2/0,
    partition_data_2/0,
    fetchable_topic_response_2/0,
    fetch_response_3/0,
    partition_data_3/0,
    fetchable_topic_response_3/0,
    fetch_response_4/0,
    aborted_transaction_4/0,
    partition_data_4/0,
    fetchable_topic_response_4/0,
    fetch_response_5/0,
    aborted_transaction_5/0,
    partition_data_5/0,
    fetchable_topic_response_5/0,
    fetch_response_6/0,
    aborted_transaction_6/0,
    partition_data_6/0,
    fetchable_topic_response_6/0,
    fetch_response_7/0,
    aborted_transaction_7/0,
    partition_data_7/0,
    fetchable_topic_response_7/0,
    fetch_response_8/0,
    aborted_transaction_8/0,
    partition_data_8/0,
    fetchable_topic_response_8/0,
    fetch_response_9/0,
    aborted_transaction_9/0,
    partition_data_9/0,
    fetchable_topic_response_9/0,
    fetch_response_10/0,
    aborted_transaction_10/0,
    partition_data_10/0,
    fetchable_topic_response_10/0,
    fetch_response_11/0,
    aborted_transaction_11/0,
    partition_data_11/0,
    fetchable_topic_response_11/0,
    fetch_response_12/0,
    epoch_end_offset_12/0,
    leader_id_and_epoch_12/0,
    snapshot_id_12/0,
    aborted_transaction_12/0,
    partition_data_12/0,
    fetchable_topic_response_12/0,
    fetch_response_13/0,
    epoch_end_offset_13/0,
    leader_id_and_epoch_13/0,
    snapshot_id_13/0,
    aborted_transaction_13/0,
    partition_data_13/0,
    fetchable_topic_response_13/0,
    fetch_response_14/0,
    epoch_end_offset_14/0,
    leader_id_and_epoch_14/0,
    snapshot_id_14/0,
    aborted_transaction_14/0,
    partition_data_14/0,
    fetchable_topic_response_14/0,
    fetch_response_15/0,
    epoch_end_offset_15/0,
    leader_id_and_epoch_15/0,
    snapshot_id_15/0,
    aborted_transaction_15/0,
    partition_data_15/0,
    fetchable_topic_response_15/0,
    fetch_response_16/0,
    epoch_end_offset_16/0,
    leader_id_and_epoch_16/0,
    snapshot_id_16/0,
    aborted_transaction_16/0,
    partition_data_16/0,
    fetchable_topic_response_16/0,
    node_endpoint_16/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_fetch_response_0(fetch_response_0()) -> iodata().

encode_fetch_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_fetchable_topic_response_0/1)
    ];
encode_fetch_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, fetchable_topic_response_0}
    }).

-spec decode_fetch_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_0(),
    Rest :: binary().

decode_fetch_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_fetchable_topic_response_0)),
    {
        Header#{
            responses => Responses
        },
        Bin1
    }.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        records => nullable_records
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_nullable_records(Records, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            records => Records
        },
        Bin4
    }.

-spec encode_fetchable_topic_response_0(fetchable_topic_response_0()) -> iodata().

encode_fetchable_topic_response_0(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_0/1)
    ];
encode_fetchable_topic_response_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_0}
    }).

-spec decode_fetchable_topic_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_0(),
    Rest :: binary().

decode_fetchable_topic_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_0)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_1(fetch_response_1()) -> iodata().

encode_fetch_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_fetchable_topic_response_1/1)
    ];
encode_fetch_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, fetchable_topic_response_1}
    }).

-spec decode_fetch_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_1(),
    Rest :: binary().

decode_fetch_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_fetchable_topic_response_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_partition_data_1(partition_data_1()) -> iodata().

encode_partition_data_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        records => nullable_records
    }).

-spec decode_partition_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_1(),
    Rest :: binary().

decode_partition_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_nullable_records(Records, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            records => Records
        },
        Bin4
    }.

-spec encode_fetchable_topic_response_1(fetchable_topic_response_1()) -> iodata().

encode_fetchable_topic_response_1(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_1/1)
    ];
encode_fetchable_topic_response_1(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_1}
    }).

-spec decode_fetchable_topic_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_1(),
    Rest :: binary().

decode_fetchable_topic_response_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_1)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_2(fetch_response_2()) -> iodata().

encode_fetch_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_fetchable_topic_response_2/1)
    ];
encode_fetch_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, fetchable_topic_response_2}
    }).

-spec decode_fetch_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_2(),
    Rest :: binary().

decode_fetch_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_fetchable_topic_response_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_partition_data_2(partition_data_2()) -> iodata().

encode_partition_data_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        records => nullable_records
    }).

-spec decode_partition_data_2(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_2(),
    Rest :: binary().

decode_partition_data_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_nullable_records(Records, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            records => Records
        },
        Bin4
    }.

-spec encode_fetchable_topic_response_2(fetchable_topic_response_2()) -> iodata().

encode_fetchable_topic_response_2(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_2/1)
    ];
encode_fetchable_topic_response_2(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_2}
    }).

-spec decode_fetchable_topic_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_2(),
    Rest :: binary().

decode_fetchable_topic_response_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_2)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_3(fetch_response_3()) -> iodata().

encode_fetch_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_fetchable_topic_response_3/1)
    ];
encode_fetch_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, fetchable_topic_response_3}
    }).

-spec decode_fetch_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_3(),
    Rest :: binary().

decode_fetch_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_fetchable_topic_response_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_partition_data_3(partition_data_3()) -> iodata().

encode_partition_data_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        records => nullable_records
    }).

-spec decode_partition_data_3(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_3(),
    Rest :: binary().

decode_partition_data_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_nullable_records(Records, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            records => Records
        },
        Bin4
    }.

-spec encode_fetchable_topic_response_3(fetchable_topic_response_3()) -> iodata().

encode_fetchable_topic_response_3(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_3/1)
    ];
encode_fetchable_topic_response_3(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_3}
    }).

-spec decode_fetchable_topic_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_3(),
    Rest :: binary().

decode_fetchable_topic_response_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_3)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_4(fetch_response_4()) -> iodata().

encode_fetch_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_fetchable_topic_response_4/1)
    ];
encode_fetch_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, fetchable_topic_response_4}
    }).

-spec decode_fetch_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_4(),
    Rest :: binary().

decode_fetch_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_fetchable_topic_response_4)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_aborted_transaction_4(aborted_transaction_4()) -> iodata().

encode_aborted_transaction_4(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset)
    ];
encode_aborted_transaction_4(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_4(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_4(),
    Rest :: binary().

decode_aborted_transaction_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    {
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    }.

-spec encode_partition_data_4(partition_data_4()) -> iodata().

encode_partition_data_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_nullable_array(AbortedTransactions, fun encode_aborted_transaction_4/1),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_4},
        records => nullable_records
    }).

-spec decode_partition_data_4(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_4(),
    Rest :: binary().

decode_partition_data_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_nullable_array(AbortedTransactions, Bin4, Bin5, ?_decode_element(decode_aborted_transaction_4)),
    ?_decode_nullable_records(Records, Bin5, Bin6),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            aborted_transactions => AbortedTransactions,
            records => Records
        },
        Bin6
    }.

-spec encode_fetchable_topic_response_4(fetchable_topic_response_4()) -> iodata().

encode_fetchable_topic_response_4(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_4/1)
    ];
encode_fetchable_topic_response_4(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_4}
    }).

-spec decode_fetchable_topic_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_4(),
    Rest :: binary().

decode_fetchable_topic_response_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_4)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_5(fetch_response_5()) -> iodata().

encode_fetch_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_fetchable_topic_response_5/1)
    ];
encode_fetch_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, fetchable_topic_response_5}
    }).

-spec decode_fetch_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_5(),
    Rest :: binary().

decode_fetch_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_fetchable_topic_response_5)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_aborted_transaction_5(aborted_transaction_5()) -> iodata().

encode_aborted_transaction_5(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset)
    ];
encode_aborted_transaction_5(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_5(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_5(),
    Rest :: binary().

decode_aborted_transaction_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    {
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    }.

-spec encode_partition_data_5(partition_data_5()) -> iodata().

encode_partition_data_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_nullable_array(AbortedTransactions, fun encode_aborted_transaction_5/1),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_5},
        records => nullable_records
    }).

-spec decode_partition_data_5(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_5(),
    Rest :: binary().

decode_partition_data_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_5)),
    ?_decode_nullable_records(Records, Bin6, Bin7),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            records => Records
        },
        Bin7
    }.

-spec encode_fetchable_topic_response_5(fetchable_topic_response_5()) -> iodata().

encode_fetchable_topic_response_5(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_5/1)
    ];
encode_fetchable_topic_response_5(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_5}
    }).

-spec decode_fetchable_topic_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_5(),
    Rest :: binary().

decode_fetchable_topic_response_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_5)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_6(fetch_response_6()) -> iodata().

encode_fetch_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_fetchable_topic_response_6/1)
    ];
encode_fetch_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, fetchable_topic_response_6}
    }).

-spec decode_fetch_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_6(),
    Rest :: binary().

decode_fetch_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_fetchable_topic_response_6)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_aborted_transaction_6(aborted_transaction_6()) -> iodata().

encode_aborted_transaction_6(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset)
    ];
encode_aborted_transaction_6(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_6(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_6(),
    Rest :: binary().

decode_aborted_transaction_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    {
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    }.

-spec encode_partition_data_6(partition_data_6()) -> iodata().

encode_partition_data_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_nullable_array(AbortedTransactions, fun encode_aborted_transaction_6/1),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_6},
        records => nullable_records
    }).

-spec decode_partition_data_6(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_6(),
    Rest :: binary().

decode_partition_data_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_6)),
    ?_decode_nullable_records(Records, Bin6, Bin7),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            records => Records
        },
        Bin7
    }.

-spec encode_fetchable_topic_response_6(fetchable_topic_response_6()) -> iodata().

encode_fetchable_topic_response_6(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_6/1)
    ];
encode_fetchable_topic_response_6(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_6}
    }).

-spec decode_fetchable_topic_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_6(),
    Rest :: binary().

decode_fetchable_topic_response_6(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_6)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_7(fetch_response_7()) -> iodata().

encode_fetch_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_array(Responses, fun encode_fetchable_topic_response_7/1)
    ];
encode_fetch_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_7}
    }).

-spec decode_fetch_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_7(),
    Rest :: binary().

decode_fetch_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_7)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    }.

-spec encode_aborted_transaction_7(aborted_transaction_7()) -> iodata().

encode_aborted_transaction_7(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset)
    ];
encode_aborted_transaction_7(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_7(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_7(),
    Rest :: binary().

decode_aborted_transaction_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    {
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    }.

-spec encode_partition_data_7(partition_data_7()) -> iodata().

encode_partition_data_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_nullable_array(AbortedTransactions, fun encode_aborted_transaction_7/1),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_7},
        records => nullable_records
    }).

-spec decode_partition_data_7(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_7(),
    Rest :: binary().

decode_partition_data_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_7)),
    ?_decode_nullable_records(Records, Bin6, Bin7),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            records => Records
        },
        Bin7
    }.

-spec encode_fetchable_topic_response_7(fetchable_topic_response_7()) -> iodata().

encode_fetchable_topic_response_7(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_7/1)
    ];
encode_fetchable_topic_response_7(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_7}
    }).

-spec decode_fetchable_topic_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_7(),
    Rest :: binary().

decode_fetchable_topic_response_7(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_7)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_8(fetch_response_8()) -> iodata().

encode_fetch_response_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_array(Responses, fun encode_fetchable_topic_response_8/1)
    ];
encode_fetch_response_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_8}
    }).

-spec decode_fetch_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_8(),
    Rest :: binary().

decode_fetch_response_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_8)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    }.

-spec encode_aborted_transaction_8(aborted_transaction_8()) -> iodata().

encode_aborted_transaction_8(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset)
    ];
encode_aborted_transaction_8(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_8(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_8(),
    Rest :: binary().

decode_aborted_transaction_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    {
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    }.

-spec encode_partition_data_8(partition_data_8()) -> iodata().

encode_partition_data_8(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_nullable_array(AbortedTransactions, fun encode_aborted_transaction_8/1),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_8(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_8},
        records => nullable_records
    }).

-spec decode_partition_data_8(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_8(),
    Rest :: binary().

decode_partition_data_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_8)),
    ?_decode_nullable_records(Records, Bin6, Bin7),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            records => Records
        },
        Bin7
    }.

-spec encode_fetchable_topic_response_8(fetchable_topic_response_8()) -> iodata().

encode_fetchable_topic_response_8(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_8/1)
    ];
encode_fetchable_topic_response_8(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_8}
    }).

-spec decode_fetchable_topic_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_8(),
    Rest :: binary().

decode_fetchable_topic_response_8(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_8)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_9(fetch_response_9()) -> iodata().

encode_fetch_response_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_array(Responses, fun encode_fetchable_topic_response_9/1)
    ];
encode_fetch_response_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_9}
    }).

-spec decode_fetch_response_9(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_9(),
    Rest :: binary().

decode_fetch_response_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_9)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    }.

-spec encode_aborted_transaction_9(aborted_transaction_9()) -> iodata().

encode_aborted_transaction_9(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset)
    ];
encode_aborted_transaction_9(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_9(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_9(),
    Rest :: binary().

decode_aborted_transaction_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    {
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    }.

-spec encode_partition_data_9(partition_data_9()) -> iodata().

encode_partition_data_9(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_nullable_array(AbortedTransactions, fun encode_aborted_transaction_9/1),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_9(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_9},
        records => nullable_records
    }).

-spec decode_partition_data_9(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_9(),
    Rest :: binary().

decode_partition_data_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_9)),
    ?_decode_nullable_records(Records, Bin6, Bin7),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            records => Records
        },
        Bin7
    }.

-spec encode_fetchable_topic_response_9(fetchable_topic_response_9()) -> iodata().

encode_fetchable_topic_response_9(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_9/1)
    ];
encode_fetchable_topic_response_9(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_9}
    }).

-spec decode_fetchable_topic_response_9(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_9(),
    Rest :: binary().

decode_fetchable_topic_response_9(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_9)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_10(fetch_response_10()) -> iodata().

encode_fetch_response_10(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_array(Responses, fun encode_fetchable_topic_response_10/1)
    ];
encode_fetch_response_10(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_10}
    }).

-spec decode_fetch_response_10(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_10(),
    Rest :: binary().

decode_fetch_response_10(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_10)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    }.

-spec encode_aborted_transaction_10(aborted_transaction_10()) -> iodata().

encode_aborted_transaction_10(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset)
    ];
encode_aborted_transaction_10(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_10(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_10(),
    Rest :: binary().

decode_aborted_transaction_10(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    {
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    }.

-spec encode_partition_data_10(partition_data_10()) -> iodata().

encode_partition_data_10(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_nullable_array(AbortedTransactions, fun encode_aborted_transaction_10/1),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_10(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_10},
        records => nullable_records
    }).

-spec decode_partition_data_10(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_10(),
    Rest :: binary().

decode_partition_data_10(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_10)),
    ?_decode_nullable_records(Records, Bin6, Bin7),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            records => Records
        },
        Bin7
    }.

-spec encode_fetchable_topic_response_10(fetchable_topic_response_10()) -> iodata().

encode_fetchable_topic_response_10(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_10/1)
    ];
encode_fetchable_topic_response_10(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_10}
    }).

-spec decode_fetchable_topic_response_10(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_10(),
    Rest :: binary().

decode_fetchable_topic_response_10(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_10)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_11(fetch_response_11()) -> iodata().

encode_fetch_response_11(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_array(Responses, fun encode_fetchable_topic_response_11/1)
    ];
encode_fetch_response_11(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_11}
    }).

-spec decode_fetch_response_11(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_11(),
    Rest :: binary().

decode_fetch_response_11(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_11)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    }.

-spec encode_aborted_transaction_11(aborted_transaction_11()) -> iodata().

encode_aborted_transaction_11(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset)
    ];
encode_aborted_transaction_11(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_11(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_11(),
    Rest :: binary().

decode_aborted_transaction_11(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    {
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    }.

-spec encode_partition_data_11(partition_data_11()) -> iodata().

encode_partition_data_11(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The preferred read replica for the consumer to use on its next fetch request
        preferred_read_replica := PreferredReadReplica,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_int32(PreferredReadReplica),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_nullable_array(AbortedTransactions, fun encode_aborted_transaction_11/1),
        ?encode_int32(PreferredReadReplica),
        ?encode_nullable_records(Records)
    ];
encode_partition_data_11(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_11},
        preferred_read_replica => int32,
        records => nullable_records
    }).

-spec decode_partition_data_11(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_11(),
    Rest :: binary().

decode_partition_data_11(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_11)),
    ?_decode_int32(PreferredReadReplica, Bin6, Bin7),
    ?_decode_nullable_records(Records, Bin7, Bin8),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            preferred_read_replica => PreferredReadReplica,
            records => Records
        },
        Bin8
    }.

-spec encode_fetchable_topic_response_11(fetchable_topic_response_11()) -> iodata().

encode_fetchable_topic_response_11(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_partition_data_11/1)
    ];
encode_fetchable_topic_response_11(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_11}
    }).

-spec decode_fetchable_topic_response_11(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_11(),
    Rest :: binary().

decode_fetchable_topic_response_11(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_11)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_response_12(fetch_response_12()) -> iodata().

encode_fetch_response_12(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_compact_array(Responses, fun encode_fetchable_topic_response_12/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_response_12(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_12}
    }).

-spec decode_fetch_response_12(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_12(),
    Rest :: binary().

decode_fetch_response_12(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_compact_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_12)),
    ?decode_tagged_fields(
        fun decode_fetch_response_12_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    ).

-spec decode_fetch_response_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetch_response_12().

decode_fetch_response_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_epoch_end_offset_12(epoch_end_offset_12()) -> iodata().

encode_epoch_end_offset_12(
    _Args = #{
        epoch := Epoch,
        end_offset := EndOffset
    }
) when
    ?is_int32(Epoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int32(Epoch),
        ?encode_int64(EndOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_epoch_end_offset_12(Args) ->
    ?encoder_error(Args, #{
        epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_12(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_12(),
    Rest :: binary().

decode_epoch_end_offset_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Epoch, Bin0, Bin1),
    ?_decode_int64(EndOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_epoch_end_offset_12_tagged_field/3,
        #{
            epoch => Epoch,
            end_offset => EndOffset
        },
        Bin2
    ).

-spec decode_epoch_end_offset_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: epoch_end_offset_12().

decode_epoch_end_offset_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_id_and_epoch_12(leader_id_and_epoch_12()) -> iodata().

encode_leader_id_and_epoch_12(
    _Args = #{
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_id_and_epoch_12(Args) ->
    ?encoder_error(Args, #{
        leader_id => int32,
        leader_epoch => int32
    }).

-spec decode_leader_id_and_epoch_12(binary()) -> {Decoded, Rest} when
    Decoded :: leader_id_and_epoch_12(),
    Rest :: binary().

decode_leader_id_and_epoch_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(LeaderId, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_id_and_epoch_12_tagged_field/3,
        #{
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch
        },
        Bin2
    ).

-spec decode_leader_id_and_epoch_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_id_and_epoch_12().

decode_leader_id_and_epoch_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_snapshot_id_12(snapshot_id_12()) -> iodata().

encode_snapshot_id_12(
    _Args = #{
        end_offset := EndOffset,
        epoch := Epoch
    }
) when
    ?is_int64(EndOffset),
    ?is_int32(Epoch)
->
    [
        ?encode_int64(EndOffset),
        ?encode_int32(Epoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_snapshot_id_12(Args) ->
    ?encoder_error(Args, #{
        end_offset => int64,
        epoch => int32
    }).

-spec decode_snapshot_id_12(binary()) -> {Decoded, Rest} when
    Decoded :: snapshot_id_12(),
    Rest :: binary().

decode_snapshot_id_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(EndOffset, Bin0, Bin1),
    ?_decode_int32(Epoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_snapshot_id_12_tagged_field/3,
        #{
            end_offset => EndOffset,
            epoch => Epoch
        },
        Bin2
    ).

-spec decode_snapshot_id_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: snapshot_id_12().

decode_snapshot_id_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_aborted_transaction_12(aborted_transaction_12()) -> iodata().

encode_aborted_transaction_12(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_aborted_transaction_12(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_12(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_12(),
    Rest :: binary().

decode_aborted_transaction_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_aborted_transaction_12_tagged_field/3,
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    ).

-spec decode_aborted_transaction_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: aborted_transaction_12().

decode_aborted_transaction_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_12(partition_data_12()) -> iodata().

encode_partition_data_12(
    Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The preferred read replica for the consumer to use on its next fetch request
        preferred_read_replica := PreferredReadReplica,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_int32(PreferredReadReplica),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_compact_nullable_array(AbortedTransactions, fun encode_aborted_transaction_12/1),
        ?encode_int32(PreferredReadReplica),
        ?encode_compact_nullable_records(Records),
        ?encode_tagged_fields(
            fun encode_partition_data_12_tagged_field/2,
            Args
        )
    ];
encode_partition_data_12(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_12},
        preferred_read_replica => int32,
        records => nullable_records
    }).

-spec encode_partition_data_12_tagged_field(
    Key :: atom(), Value :: epoch_end_offset_12() | leader_id_and_epoch_12() | snapshot_id_12()
) -> {non_neg_integer(), iodata()} | ignore.

encode_partition_data_12_tagged_field(_Key = diverging_epoch, DivergingEpoch) ->
    {0, encode_epoch_end_offset_12(DivergingEpoch)};
encode_partition_data_12_tagged_field(_Key = current_leader, CurrentLeader) ->
    {1, encode_leader_id_and_epoch_12(CurrentLeader)};
encode_partition_data_12_tagged_field(_Key = snapshot_id, SnapshotId) ->
    {2, encode_snapshot_id_12(SnapshotId)};
encode_partition_data_12_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_partition_data_12(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_12(),
    Rest :: binary().

decode_partition_data_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_compact_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_12)),
    ?_decode_int32(PreferredReadReplica, Bin6, Bin7),
    ?_decode_compact_nullable_records(Records, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_partition_data_12_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            preferred_read_replica => PreferredReadReplica,
            records => Records
        },
        Bin8
    ).

-spec decode_partition_data_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_12().

%% DivergingEpoch
%% In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, this field indicates the largest epoch and its end offset such that subsequent records are known to diverge
decode_partition_data_12_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_entity(DivergingEpoch, Bin0, Bin1, decode_epoch_end_offset_12),
    <<>> = Bin1,
    Acc#{diverging_epoch => DivergingEpoch};
%% CurrentLeader
decode_partition_data_12_tagged_field(_Tag = 1, Bin0, Acc) ->
    ?_decode_entity(CurrentLeader, Bin0, Bin1, decode_leader_id_and_epoch_12),
    <<>> = Bin1,
    Acc#{current_leader => CurrentLeader};
%% SnapshotId
%% In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch that should be used in the FetchSnapshot request.
decode_partition_data_12_tagged_field(_Tag = 2, Bin0, Acc) ->
    ?_decode_entity(SnapshotId, Bin0, Bin1, decode_snapshot_id_12),
    <<>> = Bin1,
    Acc#{snapshot_id => SnapshotId};
decode_partition_data_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetchable_topic_response_12(fetchable_topic_response_12()) -> iodata().

encode_fetchable_topic_response_12(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, fun encode_partition_data_12/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetchable_topic_response_12(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, partition_data_12}
    }).

-spec decode_fetchable_topic_response_12(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_12(),
    Rest :: binary().

decode_fetchable_topic_response_12(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_12)),
    ?decode_tagged_fields(
        fun decode_fetchable_topic_response_12_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetchable_topic_response_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetchable_topic_response_12().

decode_fetchable_topic_response_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_response_13(fetch_response_13()) -> iodata().

encode_fetch_response_13(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_compact_array(Responses, fun encode_fetchable_topic_response_13/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_response_13(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_13}
    }).

-spec decode_fetch_response_13(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_13(),
    Rest :: binary().

decode_fetch_response_13(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_compact_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_13)),
    ?decode_tagged_fields(
        fun decode_fetch_response_13_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    ).

-spec decode_fetch_response_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetch_response_13().

decode_fetch_response_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_epoch_end_offset_13(epoch_end_offset_13()) -> iodata().

encode_epoch_end_offset_13(
    _Args = #{
        epoch := Epoch,
        end_offset := EndOffset
    }
) when
    ?is_int32(Epoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int32(Epoch),
        ?encode_int64(EndOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_epoch_end_offset_13(Args) ->
    ?encoder_error(Args, #{
        epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_13(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_13(),
    Rest :: binary().

decode_epoch_end_offset_13(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Epoch, Bin0, Bin1),
    ?_decode_int64(EndOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_epoch_end_offset_13_tagged_field/3,
        #{
            epoch => Epoch,
            end_offset => EndOffset
        },
        Bin2
    ).

-spec decode_epoch_end_offset_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: epoch_end_offset_13().

decode_epoch_end_offset_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_id_and_epoch_13(leader_id_and_epoch_13()) -> iodata().

encode_leader_id_and_epoch_13(
    _Args = #{
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_id_and_epoch_13(Args) ->
    ?encoder_error(Args, #{
        leader_id => int32,
        leader_epoch => int32
    }).

-spec decode_leader_id_and_epoch_13(binary()) -> {Decoded, Rest} when
    Decoded :: leader_id_and_epoch_13(),
    Rest :: binary().

decode_leader_id_and_epoch_13(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(LeaderId, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_id_and_epoch_13_tagged_field/3,
        #{
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch
        },
        Bin2
    ).

-spec decode_leader_id_and_epoch_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_id_and_epoch_13().

decode_leader_id_and_epoch_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_snapshot_id_13(snapshot_id_13()) -> iodata().

encode_snapshot_id_13(
    _Args = #{
        end_offset := EndOffset,
        epoch := Epoch
    }
) when
    ?is_int64(EndOffset),
    ?is_int32(Epoch)
->
    [
        ?encode_int64(EndOffset),
        ?encode_int32(Epoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_snapshot_id_13(Args) ->
    ?encoder_error(Args, #{
        end_offset => int64,
        epoch => int32
    }).

-spec decode_snapshot_id_13(binary()) -> {Decoded, Rest} when
    Decoded :: snapshot_id_13(),
    Rest :: binary().

decode_snapshot_id_13(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(EndOffset, Bin0, Bin1),
    ?_decode_int32(Epoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_snapshot_id_13_tagged_field/3,
        #{
            end_offset => EndOffset,
            epoch => Epoch
        },
        Bin2
    ).

-spec decode_snapshot_id_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: snapshot_id_13().

decode_snapshot_id_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_aborted_transaction_13(aborted_transaction_13()) -> iodata().

encode_aborted_transaction_13(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_aborted_transaction_13(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_13(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_13(),
    Rest :: binary().

decode_aborted_transaction_13(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_aborted_transaction_13_tagged_field/3,
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    ).

-spec decode_aborted_transaction_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: aborted_transaction_13().

decode_aborted_transaction_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_13(partition_data_13()) -> iodata().

encode_partition_data_13(
    Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The preferred read replica for the consumer to use on its next fetch request
        preferred_read_replica := PreferredReadReplica,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_int32(PreferredReadReplica),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_compact_nullable_array(AbortedTransactions, fun encode_aborted_transaction_13/1),
        ?encode_int32(PreferredReadReplica),
        ?encode_compact_nullable_records(Records),
        ?encode_tagged_fields(
            fun encode_partition_data_13_tagged_field/2,
            Args
        )
    ];
encode_partition_data_13(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_13},
        preferred_read_replica => int32,
        records => nullable_records
    }).

-spec encode_partition_data_13_tagged_field(
    Key :: atom(), Value :: epoch_end_offset_13() | leader_id_and_epoch_13() | snapshot_id_13()
) -> {non_neg_integer(), iodata()} | ignore.

encode_partition_data_13_tagged_field(_Key = diverging_epoch, DivergingEpoch) ->
    {0, encode_epoch_end_offset_13(DivergingEpoch)};
encode_partition_data_13_tagged_field(_Key = current_leader, CurrentLeader) ->
    {1, encode_leader_id_and_epoch_13(CurrentLeader)};
encode_partition_data_13_tagged_field(_Key = snapshot_id, SnapshotId) ->
    {2, encode_snapshot_id_13(SnapshotId)};
encode_partition_data_13_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_partition_data_13(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_13(),
    Rest :: binary().

decode_partition_data_13(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_compact_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_13)),
    ?_decode_int32(PreferredReadReplica, Bin6, Bin7),
    ?_decode_compact_nullable_records(Records, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_partition_data_13_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            preferred_read_replica => PreferredReadReplica,
            records => Records
        },
        Bin8
    ).

-spec decode_partition_data_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_13().

%% DivergingEpoch
%% In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, this field indicates the largest epoch and its end offset such that subsequent records are known to diverge
decode_partition_data_13_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_entity(DivergingEpoch, Bin0, Bin1, decode_epoch_end_offset_13),
    <<>> = Bin1,
    Acc#{diverging_epoch => DivergingEpoch};
%% CurrentLeader
decode_partition_data_13_tagged_field(_Tag = 1, Bin0, Acc) ->
    ?_decode_entity(CurrentLeader, Bin0, Bin1, decode_leader_id_and_epoch_13),
    <<>> = Bin1,
    Acc#{current_leader => CurrentLeader};
%% SnapshotId
%% In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch that should be used in the FetchSnapshot request.
decode_partition_data_13_tagged_field(_Tag = 2, Bin0, Acc) ->
    ?_decode_entity(SnapshotId, Bin0, Bin1, decode_snapshot_id_13),
    <<>> = Bin1,
    Acc#{snapshot_id => SnapshotId};
decode_partition_data_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetchable_topic_response_13(fetchable_topic_response_13()) -> iodata().

encode_fetchable_topic_response_13(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_partition_data_13/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetchable_topic_response_13(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, partition_data_13}
    }).

-spec decode_fetchable_topic_response_13(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_13(),
    Rest :: binary().

decode_fetchable_topic_response_13(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_13)),
    ?decode_tagged_fields(
        fun decode_fetchable_topic_response_13_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetchable_topic_response_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetchable_topic_response_13().

decode_fetchable_topic_response_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_response_14(fetch_response_14()) -> iodata().

encode_fetch_response_14(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_compact_array(Responses, fun encode_fetchable_topic_response_14/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_response_14(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_14}
    }).

-spec decode_fetch_response_14(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_14(),
    Rest :: binary().

decode_fetch_response_14(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_compact_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_14)),
    ?decode_tagged_fields(
        fun decode_fetch_response_14_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    ).

-spec decode_fetch_response_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetch_response_14().

decode_fetch_response_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_epoch_end_offset_14(epoch_end_offset_14()) -> iodata().

encode_epoch_end_offset_14(
    _Args = #{
        epoch := Epoch,
        end_offset := EndOffset
    }
) when
    ?is_int32(Epoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int32(Epoch),
        ?encode_int64(EndOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_epoch_end_offset_14(Args) ->
    ?encoder_error(Args, #{
        epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_14(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_14(),
    Rest :: binary().

decode_epoch_end_offset_14(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Epoch, Bin0, Bin1),
    ?_decode_int64(EndOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_epoch_end_offset_14_tagged_field/3,
        #{
            epoch => Epoch,
            end_offset => EndOffset
        },
        Bin2
    ).

-spec decode_epoch_end_offset_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: epoch_end_offset_14().

decode_epoch_end_offset_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_id_and_epoch_14(leader_id_and_epoch_14()) -> iodata().

encode_leader_id_and_epoch_14(
    _Args = #{
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_id_and_epoch_14(Args) ->
    ?encoder_error(Args, #{
        leader_id => int32,
        leader_epoch => int32
    }).

-spec decode_leader_id_and_epoch_14(binary()) -> {Decoded, Rest} when
    Decoded :: leader_id_and_epoch_14(),
    Rest :: binary().

decode_leader_id_and_epoch_14(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(LeaderId, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_id_and_epoch_14_tagged_field/3,
        #{
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch
        },
        Bin2
    ).

-spec decode_leader_id_and_epoch_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_id_and_epoch_14().

decode_leader_id_and_epoch_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_snapshot_id_14(snapshot_id_14()) -> iodata().

encode_snapshot_id_14(
    _Args = #{
        end_offset := EndOffset,
        epoch := Epoch
    }
) when
    ?is_int64(EndOffset),
    ?is_int32(Epoch)
->
    [
        ?encode_int64(EndOffset),
        ?encode_int32(Epoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_snapshot_id_14(Args) ->
    ?encoder_error(Args, #{
        end_offset => int64,
        epoch => int32
    }).

-spec decode_snapshot_id_14(binary()) -> {Decoded, Rest} when
    Decoded :: snapshot_id_14(),
    Rest :: binary().

decode_snapshot_id_14(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(EndOffset, Bin0, Bin1),
    ?_decode_int32(Epoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_snapshot_id_14_tagged_field/3,
        #{
            end_offset => EndOffset,
            epoch => Epoch
        },
        Bin2
    ).

-spec decode_snapshot_id_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: snapshot_id_14().

decode_snapshot_id_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_aborted_transaction_14(aborted_transaction_14()) -> iodata().

encode_aborted_transaction_14(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_aborted_transaction_14(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_14(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_14(),
    Rest :: binary().

decode_aborted_transaction_14(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_aborted_transaction_14_tagged_field/3,
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    ).

-spec decode_aborted_transaction_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: aborted_transaction_14().

decode_aborted_transaction_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_14(partition_data_14()) -> iodata().

encode_partition_data_14(
    Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The preferred read replica for the consumer to use on its next fetch request
        preferred_read_replica := PreferredReadReplica,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_int32(PreferredReadReplica),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_compact_nullable_array(AbortedTransactions, fun encode_aborted_transaction_14/1),
        ?encode_int32(PreferredReadReplica),
        ?encode_compact_nullable_records(Records),
        ?encode_tagged_fields(
            fun encode_partition_data_14_tagged_field/2,
            Args
        )
    ];
encode_partition_data_14(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_14},
        preferred_read_replica => int32,
        records => nullable_records
    }).

-spec encode_partition_data_14_tagged_field(
    Key :: atom(), Value :: epoch_end_offset_14() | leader_id_and_epoch_14() | snapshot_id_14()
) -> {non_neg_integer(), iodata()} | ignore.

encode_partition_data_14_tagged_field(_Key = diverging_epoch, DivergingEpoch) ->
    {0, encode_epoch_end_offset_14(DivergingEpoch)};
encode_partition_data_14_tagged_field(_Key = current_leader, CurrentLeader) ->
    {1, encode_leader_id_and_epoch_14(CurrentLeader)};
encode_partition_data_14_tagged_field(_Key = snapshot_id, SnapshotId) ->
    {2, encode_snapshot_id_14(SnapshotId)};
encode_partition_data_14_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_partition_data_14(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_14(),
    Rest :: binary().

decode_partition_data_14(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_compact_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_14)),
    ?_decode_int32(PreferredReadReplica, Bin6, Bin7),
    ?_decode_compact_nullable_records(Records, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_partition_data_14_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            preferred_read_replica => PreferredReadReplica,
            records => Records
        },
        Bin8
    ).

-spec decode_partition_data_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_14().

%% DivergingEpoch
%% In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, this field indicates the largest epoch and its end offset such that subsequent records are known to diverge
decode_partition_data_14_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_entity(DivergingEpoch, Bin0, Bin1, decode_epoch_end_offset_14),
    <<>> = Bin1,
    Acc#{diverging_epoch => DivergingEpoch};
%% CurrentLeader
decode_partition_data_14_tagged_field(_Tag = 1, Bin0, Acc) ->
    ?_decode_entity(CurrentLeader, Bin0, Bin1, decode_leader_id_and_epoch_14),
    <<>> = Bin1,
    Acc#{current_leader => CurrentLeader};
%% SnapshotId
%% In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch that should be used in the FetchSnapshot request.
decode_partition_data_14_tagged_field(_Tag = 2, Bin0, Acc) ->
    ?_decode_entity(SnapshotId, Bin0, Bin1, decode_snapshot_id_14),
    <<>> = Bin1,
    Acc#{snapshot_id => SnapshotId};
decode_partition_data_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetchable_topic_response_14(fetchable_topic_response_14()) -> iodata().

encode_fetchable_topic_response_14(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_partition_data_14/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetchable_topic_response_14(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, partition_data_14}
    }).

-spec decode_fetchable_topic_response_14(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_14(),
    Rest :: binary().

decode_fetchable_topic_response_14(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_14)),
    ?decode_tagged_fields(
        fun decode_fetchable_topic_response_14_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetchable_topic_response_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetchable_topic_response_14().

decode_fetchable_topic_response_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_response_15(fetch_response_15()) -> iodata().

encode_fetch_response_15(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_compact_array(Responses, fun encode_fetchable_topic_response_15/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_response_15(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_15}
    }).

-spec decode_fetch_response_15(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_15(),
    Rest :: binary().

decode_fetch_response_15(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_compact_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_15)),
    ?decode_tagged_fields(
        fun decode_fetch_response_15_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    ).

-spec decode_fetch_response_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetch_response_15().

decode_fetch_response_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_epoch_end_offset_15(epoch_end_offset_15()) -> iodata().

encode_epoch_end_offset_15(
    _Args = #{
        epoch := Epoch,
        end_offset := EndOffset
    }
) when
    ?is_int32(Epoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int32(Epoch),
        ?encode_int64(EndOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_epoch_end_offset_15(Args) ->
    ?encoder_error(Args, #{
        epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_15(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_15(),
    Rest :: binary().

decode_epoch_end_offset_15(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Epoch, Bin0, Bin1),
    ?_decode_int64(EndOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_epoch_end_offset_15_tagged_field/3,
        #{
            epoch => Epoch,
            end_offset => EndOffset
        },
        Bin2
    ).

-spec decode_epoch_end_offset_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: epoch_end_offset_15().

decode_epoch_end_offset_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_id_and_epoch_15(leader_id_and_epoch_15()) -> iodata().

encode_leader_id_and_epoch_15(
    _Args = #{
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_id_and_epoch_15(Args) ->
    ?encoder_error(Args, #{
        leader_id => int32,
        leader_epoch => int32
    }).

-spec decode_leader_id_and_epoch_15(binary()) -> {Decoded, Rest} when
    Decoded :: leader_id_and_epoch_15(),
    Rest :: binary().

decode_leader_id_and_epoch_15(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(LeaderId, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_id_and_epoch_15_tagged_field/3,
        #{
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch
        },
        Bin2
    ).

-spec decode_leader_id_and_epoch_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_id_and_epoch_15().

decode_leader_id_and_epoch_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_snapshot_id_15(snapshot_id_15()) -> iodata().

encode_snapshot_id_15(
    _Args = #{
        end_offset := EndOffset,
        epoch := Epoch
    }
) when
    ?is_int64(EndOffset),
    ?is_int32(Epoch)
->
    [
        ?encode_int64(EndOffset),
        ?encode_int32(Epoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_snapshot_id_15(Args) ->
    ?encoder_error(Args, #{
        end_offset => int64,
        epoch => int32
    }).

-spec decode_snapshot_id_15(binary()) -> {Decoded, Rest} when
    Decoded :: snapshot_id_15(),
    Rest :: binary().

decode_snapshot_id_15(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(EndOffset, Bin0, Bin1),
    ?_decode_int32(Epoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_snapshot_id_15_tagged_field/3,
        #{
            end_offset => EndOffset,
            epoch => Epoch
        },
        Bin2
    ).

-spec decode_snapshot_id_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: snapshot_id_15().

decode_snapshot_id_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_aborted_transaction_15(aborted_transaction_15()) -> iodata().

encode_aborted_transaction_15(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_aborted_transaction_15(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_15(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_15(),
    Rest :: binary().

decode_aborted_transaction_15(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_aborted_transaction_15_tagged_field/3,
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    ).

-spec decode_aborted_transaction_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: aborted_transaction_15().

decode_aborted_transaction_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_15(partition_data_15()) -> iodata().

encode_partition_data_15(
    Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The preferred read replica for the consumer to use on its next fetch request
        preferred_read_replica := PreferredReadReplica,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_int32(PreferredReadReplica),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_compact_nullable_array(AbortedTransactions, fun encode_aborted_transaction_15/1),
        ?encode_int32(PreferredReadReplica),
        ?encode_compact_nullable_records(Records),
        ?encode_tagged_fields(
            fun encode_partition_data_15_tagged_field/2,
            Args
        )
    ];
encode_partition_data_15(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_15},
        preferred_read_replica => int32,
        records => nullable_records
    }).

-spec encode_partition_data_15_tagged_field(
    Key :: atom(), Value :: epoch_end_offset_15() | leader_id_and_epoch_15() | snapshot_id_15()
) -> {non_neg_integer(), iodata()} | ignore.

encode_partition_data_15_tagged_field(_Key = diverging_epoch, DivergingEpoch) ->
    {0, encode_epoch_end_offset_15(DivergingEpoch)};
encode_partition_data_15_tagged_field(_Key = current_leader, CurrentLeader) ->
    {1, encode_leader_id_and_epoch_15(CurrentLeader)};
encode_partition_data_15_tagged_field(_Key = snapshot_id, SnapshotId) ->
    {2, encode_snapshot_id_15(SnapshotId)};
encode_partition_data_15_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_partition_data_15(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_15(),
    Rest :: binary().

decode_partition_data_15(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_compact_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_15)),
    ?_decode_int32(PreferredReadReplica, Bin6, Bin7),
    ?_decode_compact_nullable_records(Records, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_partition_data_15_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            preferred_read_replica => PreferredReadReplica,
            records => Records
        },
        Bin8
    ).

-spec decode_partition_data_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_15().

%% DivergingEpoch
%% In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, this field indicates the largest epoch and its end offset such that subsequent records are known to diverge
decode_partition_data_15_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_entity(DivergingEpoch, Bin0, Bin1, decode_epoch_end_offset_15),
    <<>> = Bin1,
    Acc#{diverging_epoch => DivergingEpoch};
%% CurrentLeader
decode_partition_data_15_tagged_field(_Tag = 1, Bin0, Acc) ->
    ?_decode_entity(CurrentLeader, Bin0, Bin1, decode_leader_id_and_epoch_15),
    <<>> = Bin1,
    Acc#{current_leader => CurrentLeader};
%% SnapshotId
%% In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch that should be used in the FetchSnapshot request.
decode_partition_data_15_tagged_field(_Tag = 2, Bin0, Acc) ->
    ?_decode_entity(SnapshotId, Bin0, Bin1, decode_snapshot_id_15),
    <<>> = Bin1,
    Acc#{snapshot_id => SnapshotId};
decode_partition_data_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetchable_topic_response_15(fetchable_topic_response_15()) -> iodata().

encode_fetchable_topic_response_15(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_partition_data_15/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetchable_topic_response_15(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, partition_data_15}
    }).

-spec decode_fetchable_topic_response_15(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_15(),
    Rest :: binary().

decode_fetchable_topic_response_15(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_15)),
    ?decode_tagged_fields(
        fun decode_fetchable_topic_response_15_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetchable_topic_response_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetchable_topic_response_15().

decode_fetchable_topic_response_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_response_16(fetch_response_16()) -> iodata().

encode_fetch_response_16(
    Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The fetch session ID, or 0 if this is not part of a fetch session.
        session_id := SessionId,
        % The response topics.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(SessionId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(SessionId),
        ?encode_compact_array(Responses, fun encode_fetchable_topic_response_16/1),
        ?encode_tagged_fields(
            fun encode_fetch_response_16_tagged_field/2,
            Args
        )
    ];
encode_fetch_response_16(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        session_id => int32,
        responses => {array, fetchable_topic_response_16}
    }).

-spec encode_fetch_response_16_tagged_field(
    Key :: atom(), Value :: list(node_endpoint_16())
) -> {non_neg_integer(), iodata()} | ignore.

encode_fetch_response_16_tagged_field(_Key = node_endpoints, NodeEndpoints) ->
    {0, ?encode_compact_array(NodeEndpoints, fun encode_node_endpoint_16/1)};
encode_fetch_response_16_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_fetch_response_16(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_response_16(),
    Rest :: binary().

decode_fetch_response_16(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(SessionId, Bin2, Bin3),
    ?_decode_compact_array(Responses, Bin3, Bin4, ?_decode_element(decode_fetchable_topic_response_16)),
    ?decode_tagged_fields(
        fun decode_fetch_response_16_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            session_id => SessionId,
            responses => Responses
        },
        Bin4
    ).

-spec decode_fetch_response_16_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetch_response_16().

%% NodeEndpoints
%% Endpoints for all current-leaders enumerated in PartitionData, with errors NOT_LEADER_OR_FOLLOWER & FENCED_LEADER_EPOCH.
decode_fetch_response_16_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_compact_array(NodeEndpoints, Bin0, Bin1, ?_decode_element(decode_node_endpoint_16)),
    <<>> = Bin1,
    Acc#{node_endpoints => NodeEndpoints};
decode_fetch_response_16_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_epoch_end_offset_16(epoch_end_offset_16()) -> iodata().

encode_epoch_end_offset_16(
    _Args = #{
        epoch := Epoch,
        end_offset := EndOffset
    }
) when
    ?is_int32(Epoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int32(Epoch),
        ?encode_int64(EndOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_epoch_end_offset_16(Args) ->
    ?encoder_error(Args, #{
        epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_16(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_16(),
    Rest :: binary().

decode_epoch_end_offset_16(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Epoch, Bin0, Bin1),
    ?_decode_int64(EndOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_epoch_end_offset_16_tagged_field/3,
        #{
            epoch => Epoch,
            end_offset => EndOffset
        },
        Bin2
    ).

-spec decode_epoch_end_offset_16_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: epoch_end_offset_16().

decode_epoch_end_offset_16_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_id_and_epoch_16(leader_id_and_epoch_16()) -> iodata().

encode_leader_id_and_epoch_16(
    _Args = #{
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_id_and_epoch_16(Args) ->
    ?encoder_error(Args, #{
        leader_id => int32,
        leader_epoch => int32
    }).

-spec decode_leader_id_and_epoch_16(binary()) -> {Decoded, Rest} when
    Decoded :: leader_id_and_epoch_16(),
    Rest :: binary().

decode_leader_id_and_epoch_16(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(LeaderId, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_id_and_epoch_16_tagged_field/3,
        #{
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch
        },
        Bin2
    ).

-spec decode_leader_id_and_epoch_16_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_id_and_epoch_16().

decode_leader_id_and_epoch_16_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_snapshot_id_16(snapshot_id_16()) -> iodata().

encode_snapshot_id_16(
    _Args = #{
        end_offset := EndOffset,
        epoch := Epoch
    }
) when
    ?is_int64(EndOffset),
    ?is_int32(Epoch)
->
    [
        ?encode_int64(EndOffset),
        ?encode_int32(Epoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_snapshot_id_16(Args) ->
    ?encoder_error(Args, #{
        end_offset => int64,
        epoch => int32
    }).

-spec decode_snapshot_id_16(binary()) -> {Decoded, Rest} when
    Decoded :: snapshot_id_16(),
    Rest :: binary().

decode_snapshot_id_16(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(EndOffset, Bin0, Bin1),
    ?_decode_int32(Epoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_snapshot_id_16_tagged_field/3,
        #{
            end_offset => EndOffset,
            epoch => Epoch
        },
        Bin2
    ).

-spec decode_snapshot_id_16_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: snapshot_id_16().

decode_snapshot_id_16_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_aborted_transaction_16(aborted_transaction_16()) -> iodata().

encode_aborted_transaction_16(
    _Args = #{
        % The producer id associated with the aborted transaction.
        producer_id := ProducerId,
        % The first offset in the aborted transaction.
        first_offset := FirstOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int64(FirstOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int64(FirstOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_aborted_transaction_16(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        first_offset => int64
    }).

-spec decode_aborted_transaction_16(binary()) -> {Decoded, Rest} when
    Decoded :: aborted_transaction_16(),
    Rest :: binary().

decode_aborted_transaction_16(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int64(FirstOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_aborted_transaction_16_tagged_field/3,
        #{
            producer_id => ProducerId,
            first_offset => FirstOffset
        },
        Bin2
    ).

-spec decode_aborted_transaction_16_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: aborted_transaction_16().

decode_aborted_transaction_16_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_16(partition_data_16()) -> iodata().

encode_partition_data_16(
    Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The current high water mark.
        high_watermark := HighWatermark,
        % The last stable offset (or LSO) of the partition. This is the last offset such that the state of all transactional records prior to this offset have been decided (ABORTED or COMMITTED)
        last_stable_offset := LastStableOffset,
        % The current log start offset.
        log_start_offset := LogStartOffset,
        % The aborted transactions.
        aborted_transactions := AbortedTransactions,
        % The preferred read replica for the consumer to use on its next fetch request
        preferred_read_replica := PreferredReadReplica,
        % The record data.
        records := Records
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(HighWatermark),
    ?is_int64(LastStableOffset),
    ?is_int64(LogStartOffset),
    ?is_nullable_array(AbortedTransactions),
    ?is_int32(PreferredReadReplica),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(HighWatermark),
        ?encode_int64(LastStableOffset),
        ?encode_int64(LogStartOffset),
        ?encode_compact_nullable_array(AbortedTransactions, fun encode_aborted_transaction_16/1),
        ?encode_int32(PreferredReadReplica),
        ?encode_compact_nullable_records(Records),
        ?encode_tagged_fields(
            fun encode_partition_data_16_tagged_field/2,
            Args
        )
    ];
encode_partition_data_16(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        high_watermark => int64,
        last_stable_offset => int64,
        log_start_offset => int64,
        aborted_transactions => {nullable_array, aborted_transaction_16},
        preferred_read_replica => int32,
        records => nullable_records
    }).

-spec encode_partition_data_16_tagged_field(
    Key :: atom(), Value :: epoch_end_offset_16() | leader_id_and_epoch_16() | snapshot_id_16()
) -> {non_neg_integer(), iodata()} | ignore.

encode_partition_data_16_tagged_field(_Key = diverging_epoch, DivergingEpoch) ->
    {0, encode_epoch_end_offset_16(DivergingEpoch)};
encode_partition_data_16_tagged_field(_Key = current_leader, CurrentLeader) ->
    {1, encode_leader_id_and_epoch_16(CurrentLeader)};
encode_partition_data_16_tagged_field(_Key = snapshot_id, SnapshotId) ->
    {2, encode_snapshot_id_16(SnapshotId)};
encode_partition_data_16_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_partition_data_16(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_16(),
    Rest :: binary().

decode_partition_data_16(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(HighWatermark, Bin2, Bin3),
    ?_decode_int64(LastStableOffset, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_compact_nullable_array(AbortedTransactions, Bin5, Bin6, ?_decode_element(decode_aborted_transaction_16)),
    ?_decode_int32(PreferredReadReplica, Bin6, Bin7),
    ?_decode_compact_nullable_records(Records, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_partition_data_16_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            high_watermark => HighWatermark,
            last_stable_offset => LastStableOffset,
            log_start_offset => LogStartOffset,
            aborted_transactions => AbortedTransactions,
            preferred_read_replica => PreferredReadReplica,
            records => Records
        },
        Bin8
    ).

-spec decode_partition_data_16_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_16().

%% DivergingEpoch
%% In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, this field indicates the largest epoch and its end offset such that subsequent records are known to diverge
decode_partition_data_16_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_entity(DivergingEpoch, Bin0, Bin1, decode_epoch_end_offset_16),
    <<>> = Bin1,
    Acc#{diverging_epoch => DivergingEpoch};
%% CurrentLeader
decode_partition_data_16_tagged_field(_Tag = 1, Bin0, Acc) ->
    ?_decode_entity(CurrentLeader, Bin0, Bin1, decode_leader_id_and_epoch_16),
    <<>> = Bin1,
    Acc#{current_leader => CurrentLeader};
%% SnapshotId
%% In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch that should be used in the FetchSnapshot request.
decode_partition_data_16_tagged_field(_Tag = 2, Bin0, Acc) ->
    ?_decode_entity(SnapshotId, Bin0, Bin1, decode_snapshot_id_16),
    <<>> = Bin1,
    Acc#{snapshot_id => SnapshotId};
decode_partition_data_16_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetchable_topic_response_16(fetchable_topic_response_16()) -> iodata().

encode_fetchable_topic_response_16(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The topic partitions.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_partition_data_16/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetchable_topic_response_16(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, partition_data_16}
    }).

-spec decode_fetchable_topic_response_16(binary()) -> {Decoded, Rest} when
    Decoded :: fetchable_topic_response_16(),
    Rest :: binary().

decode_fetchable_topic_response_16(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_16)),
    ?decode_tagged_fields(
        fun decode_fetchable_topic_response_16_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetchable_topic_response_16_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetchable_topic_response_16().

decode_fetchable_topic_response_16_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_node_endpoint_16(node_endpoint_16()) -> iodata().

encode_node_endpoint_16(
    _Args = #{
        % The ID of the associated node.
        node_id := NodeId,
        % The node's hostname.
        host := Host,
        % The node's port.
        port := Port,
        % The rack of the node, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_node_endpoint_16(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_node_endpoint_16(binary()) -> {Decoded, Rest} when
    Decoded :: node_endpoint_16(),
    Rest :: binary().

decode_node_endpoint_16(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_compact_nullable_string(Rack, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_node_endpoint_16_tagged_field/3,
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    ).

-spec decode_node_endpoint_16_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: node_endpoint_16().

decode_node_endpoint_16_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type fetch_response_0() :: #{
    correlation_id => integer(),
    responses := list(fetchable_topic_response_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_0() :: #{
    topic := binary(),
    partitions := list(partition_data_0())
}.
-type fetch_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(fetchable_topic_response_1())
}.
-type partition_data_1() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_1() :: #{
    topic := binary(),
    partitions := list(partition_data_1())
}.
-type fetch_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(fetchable_topic_response_2())
}.
-type partition_data_2() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_2() :: #{
    topic := binary(),
    partitions := list(partition_data_2())
}.
-type fetch_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(fetchable_topic_response_3())
}.
-type partition_data_3() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_3() :: #{
    topic := binary(),
    partitions := list(partition_data_3())
}.
-type fetch_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(fetchable_topic_response_4())
}.
-type aborted_transaction_4() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_4() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    aborted_transactions := list(aborted_transaction_4()) | null,
    records := kafcod_records:records()
}.
-type fetchable_topic_response_4() :: #{
    topic := binary(),
    partitions := list(partition_data_4())
}.
-type fetch_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(fetchable_topic_response_5())
}.
-type aborted_transaction_5() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_5() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    aborted_transactions := list(aborted_transaction_5()) | null,
    records := kafcod_records:records()
}.
-type fetchable_topic_response_5() :: #{
    topic := binary(),
    partitions := list(partition_data_5())
}.
-type fetch_response_6() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(fetchable_topic_response_6())
}.
-type aborted_transaction_6() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_6() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    aborted_transactions := list(aborted_transaction_6()) | null,
    records := kafcod_records:records()
}.
-type fetchable_topic_response_6() :: #{
    topic := binary(),
    partitions := list(partition_data_6())
}.
-type fetch_response_7() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_7())
}.
-type aborted_transaction_7() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_7() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    aborted_transactions := list(aborted_transaction_7()) | null,
    records := kafcod_records:records()
}.
-type fetchable_topic_response_7() :: #{
    topic := binary(),
    partitions := list(partition_data_7())
}.
-type fetch_response_8() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_8())
}.
-type aborted_transaction_8() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_8() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    aborted_transactions := list(aborted_transaction_8()) | null,
    records := kafcod_records:records()
}.
-type fetchable_topic_response_8() :: #{
    topic := binary(),
    partitions := list(partition_data_8())
}.
-type fetch_response_9() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_9())
}.
-type aborted_transaction_9() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_9() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    aborted_transactions := list(aborted_transaction_9()) | null,
    records := kafcod_records:records()
}.
-type fetchable_topic_response_9() :: #{
    topic := binary(),
    partitions := list(partition_data_9())
}.
-type fetch_response_10() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_10())
}.
-type aborted_transaction_10() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_10() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    aborted_transactions := list(aborted_transaction_10()) | null,
    records := kafcod_records:records()
}.
-type fetchable_topic_response_10() :: #{
    topic := binary(),
    partitions := list(partition_data_10())
}.
-type fetch_response_11() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_11())
}.
-type aborted_transaction_11() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_11() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    aborted_transactions := list(aborted_transaction_11()) | null,
    preferred_read_replica := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_11() :: #{
    topic := binary(),
    partitions := list(partition_data_11())
}.
-type fetch_response_12() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_12())
}.
-type epoch_end_offset_12() :: #{
    epoch := integer(),
    end_offset := integer()
}.
-type leader_id_and_epoch_12() :: #{
    leader_id := integer(),
    leader_epoch := integer()
}.
-type snapshot_id_12() :: #{
    end_offset := integer(),
    epoch := integer()
}.
-type aborted_transaction_12() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_12() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    diverging_epoch => epoch_end_offset_12(),
    current_leader => leader_id_and_epoch_12(),
    snapshot_id => snapshot_id_12(),
    aborted_transactions := list(aborted_transaction_12()) | null,
    preferred_read_replica := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_12() :: #{
    topic := binary(),
    partitions := list(partition_data_12())
}.
-type fetch_response_13() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_13())
}.
-type epoch_end_offset_13() :: #{
    epoch := integer(),
    end_offset := integer()
}.
-type leader_id_and_epoch_13() :: #{
    leader_id := integer(),
    leader_epoch := integer()
}.
-type snapshot_id_13() :: #{
    end_offset := integer(),
    epoch := integer()
}.
-type aborted_transaction_13() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_13() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    diverging_epoch => epoch_end_offset_13(),
    current_leader => leader_id_and_epoch_13(),
    snapshot_id => snapshot_id_13(),
    aborted_transactions := list(aborted_transaction_13()) | null,
    preferred_read_replica := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_13() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_13())
}.
-type fetch_response_14() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_14())
}.
-type epoch_end_offset_14() :: #{
    epoch := integer(),
    end_offset := integer()
}.
-type leader_id_and_epoch_14() :: #{
    leader_id := integer(),
    leader_epoch := integer()
}.
-type snapshot_id_14() :: #{
    end_offset := integer(),
    epoch := integer()
}.
-type aborted_transaction_14() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_14() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    diverging_epoch => epoch_end_offset_14(),
    current_leader => leader_id_and_epoch_14(),
    snapshot_id => snapshot_id_14(),
    aborted_transactions := list(aborted_transaction_14()) | null,
    preferred_read_replica := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_14() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_14())
}.
-type fetch_response_15() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_15())
}.
-type epoch_end_offset_15() :: #{
    epoch := integer(),
    end_offset := integer()
}.
-type leader_id_and_epoch_15() :: #{
    leader_id := integer(),
    leader_epoch := integer()
}.
-type snapshot_id_15() :: #{
    end_offset := integer(),
    epoch := integer()
}.
-type aborted_transaction_15() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_15() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    diverging_epoch => epoch_end_offset_15(),
    current_leader => leader_id_and_epoch_15(),
    snapshot_id => snapshot_id_15(),
    aborted_transactions := list(aborted_transaction_15()) | null,
    preferred_read_replica := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_15() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_15())
}.
-type fetch_response_16() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    session_id := integer(),
    responses := list(fetchable_topic_response_16()),
    node_endpoints => list(node_endpoint_16())
}.
-type epoch_end_offset_16() :: #{
    epoch := integer(),
    end_offset := integer()
}.
-type leader_id_and_epoch_16() :: #{
    leader_id := integer(),
    leader_epoch := integer()
}.
-type snapshot_id_16() :: #{
    end_offset := integer(),
    epoch := integer()
}.
-type aborted_transaction_16() :: #{
    producer_id := integer(),
    first_offset := integer()
}.
-type partition_data_16() :: #{
    partition_index := integer(),
    error_code := integer(),
    high_watermark := integer(),
    last_stable_offset := integer(),
    log_start_offset := integer(),
    diverging_epoch => epoch_end_offset_16(),
    current_leader => leader_id_and_epoch_16(),
    snapshot_id => snapshot_id_16(),
    aborted_transactions := list(aborted_transaction_16()) | null,
    preferred_read_replica := integer(),
    records := kafcod_records:records()
}.
-type fetchable_topic_response_16() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_16())
}.
-type node_endpoint_16() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
