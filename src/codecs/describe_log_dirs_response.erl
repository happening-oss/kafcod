-module(describe_log_dirs_response).
-export([
    encode_describe_log_dirs_response_0/1,
    decode_describe_log_dirs_response_0/1,
    encode_describe_log_dirs_response_1/1,
    decode_describe_log_dirs_response_1/1,
    encode_describe_log_dirs_response_2/1,
    decode_describe_log_dirs_response_2/1,
    encode_describe_log_dirs_response_3/1,
    decode_describe_log_dirs_response_3/1,
    encode_describe_log_dirs_response_4/1,
    decode_describe_log_dirs_response_4/1
]).
-export_type([
    describe_log_dirs_response_0/0,
    describe_log_dirs_partition_0/0,
    describe_log_dirs_topic_0/0,
    describe_log_dirs_result_0/0,
    describe_log_dirs_response_1/0,
    describe_log_dirs_partition_1/0,
    describe_log_dirs_topic_1/0,
    describe_log_dirs_result_1/0,
    describe_log_dirs_response_2/0,
    describe_log_dirs_partition_2/0,
    describe_log_dirs_topic_2/0,
    describe_log_dirs_result_2/0,
    describe_log_dirs_response_3/0,
    describe_log_dirs_partition_3/0,
    describe_log_dirs_topic_3/0,
    describe_log_dirs_result_3/0,
    describe_log_dirs_response_4/0,
    describe_log_dirs_partition_4/0,
    describe_log_dirs_topic_4/0,
    describe_log_dirs_result_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_log_dirs_response_0(describe_log_dirs_response_0()) -> iodata().

encode_describe_log_dirs_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The log directories.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Results, fun encode_describe_log_dirs_result_0/1)
    ];
encode_describe_log_dirs_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, describe_log_dirs_result_0}
    }).

-spec decode_describe_log_dirs_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_response_0(),
    Rest :: binary().

decode_describe_log_dirs_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_describe_log_dirs_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_describe_log_dirs_partition_0(describe_log_dirs_partition_0()) -> iodata().

encode_describe_log_dirs_partition_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The size of the log segments in this partition in bytes.
        partition_size := PartitionSize,
        % The lag of the log's LEO w.r.t. partition's HW (if it is the current log for the partition) or current replica's LEO (if it is the future log for the partition)
        offset_lag := OffsetLag,
        % True if this log is created by AlterReplicaLogDirsRequest and will replace the current log of the replica in the future.
        is_future_key := IsFutureKey
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(PartitionSize),
    ?is_int64(OffsetLag),
    ?is_bool(IsFutureKey)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(PartitionSize),
        ?encode_int64(OffsetLag),
        ?encode_bool(IsFutureKey)
    ];
encode_describe_log_dirs_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_size => int64,
        offset_lag => int64,
        is_future_key => bool
    }).

-spec decode_describe_log_dirs_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_partition_0(),
    Rest :: binary().

decode_describe_log_dirs_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(PartitionSize, Bin1, Bin2),
    ?_decode_int64(OffsetLag, Bin2, Bin3),
    ?_decode_bool(IsFutureKey, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            partition_size => PartitionSize,
            offset_lag => OffsetLag,
            is_future_key => IsFutureKey
        },
        Bin4
    }.

-spec encode_describe_log_dirs_topic_0(describe_log_dirs_topic_0()) -> iodata().

encode_describe_log_dirs_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_describe_log_dirs_partition_0/1)
    ];
encode_describe_log_dirs_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, describe_log_dirs_partition_0}
    }).

-spec decode_describe_log_dirs_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_topic_0(),
    Rest :: binary().

decode_describe_log_dirs_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_describe_log_dirs_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_describe_log_dirs_result_0(describe_log_dirs_result_0()) -> iodata().

encode_describe_log_dirs_result_0(
    _Args = #{
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The absolute log directory path.
        log_dir := LogDir,
        % Each topic.
        topics := Topics
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(LogDir),
    ?is_array(Topics)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(LogDir),
        ?encode_array(Topics, fun encode_describe_log_dirs_topic_0/1)
    ];
encode_describe_log_dirs_result_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        log_dir => string,
        topics => {array, describe_log_dirs_topic_0}
    }).

-spec decode_describe_log_dirs_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_result_0(),
    Rest :: binary().

decode_describe_log_dirs_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(LogDir, Bin1, Bin2),
    ?_decode_array(Topics, Bin2, Bin3, ?_decode_element(decode_describe_log_dirs_topic_0)),
    {
        #{
            error_code => ErrorCode,
            log_dir => LogDir,
            topics => Topics
        },
        Bin3
    }.

-spec encode_describe_log_dirs_response_1(describe_log_dirs_response_1()) -> iodata().

encode_describe_log_dirs_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The log directories.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Results, fun encode_describe_log_dirs_result_1/1)
    ];
encode_describe_log_dirs_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, describe_log_dirs_result_1}
    }).

-spec decode_describe_log_dirs_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_response_1(),
    Rest :: binary().

decode_describe_log_dirs_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_describe_log_dirs_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_describe_log_dirs_partition_1(describe_log_dirs_partition_1()) -> iodata().

encode_describe_log_dirs_partition_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The size of the log segments in this partition in bytes.
        partition_size := PartitionSize,
        % The lag of the log's LEO w.r.t. partition's HW (if it is the current log for the partition) or current replica's LEO (if it is the future log for the partition)
        offset_lag := OffsetLag,
        % True if this log is created by AlterReplicaLogDirsRequest and will replace the current log of the replica in the future.
        is_future_key := IsFutureKey
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(PartitionSize),
    ?is_int64(OffsetLag),
    ?is_bool(IsFutureKey)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(PartitionSize),
        ?encode_int64(OffsetLag),
        ?encode_bool(IsFutureKey)
    ];
encode_describe_log_dirs_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_size => int64,
        offset_lag => int64,
        is_future_key => bool
    }).

-spec decode_describe_log_dirs_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_partition_1(),
    Rest :: binary().

decode_describe_log_dirs_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(PartitionSize, Bin1, Bin2),
    ?_decode_int64(OffsetLag, Bin2, Bin3),
    ?_decode_bool(IsFutureKey, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            partition_size => PartitionSize,
            offset_lag => OffsetLag,
            is_future_key => IsFutureKey
        },
        Bin4
    }.

-spec encode_describe_log_dirs_topic_1(describe_log_dirs_topic_1()) -> iodata().

encode_describe_log_dirs_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_describe_log_dirs_partition_1/1)
    ];
encode_describe_log_dirs_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, describe_log_dirs_partition_1}
    }).

-spec decode_describe_log_dirs_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_topic_1(),
    Rest :: binary().

decode_describe_log_dirs_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_describe_log_dirs_partition_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_describe_log_dirs_result_1(describe_log_dirs_result_1()) -> iodata().

encode_describe_log_dirs_result_1(
    _Args = #{
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The absolute log directory path.
        log_dir := LogDir,
        % Each topic.
        topics := Topics
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(LogDir),
    ?is_array(Topics)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(LogDir),
        ?encode_array(Topics, fun encode_describe_log_dirs_topic_1/1)
    ];
encode_describe_log_dirs_result_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        log_dir => string,
        topics => {array, describe_log_dirs_topic_1}
    }).

-spec decode_describe_log_dirs_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_result_1(),
    Rest :: binary().

decode_describe_log_dirs_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(LogDir, Bin1, Bin2),
    ?_decode_array(Topics, Bin2, Bin3, ?_decode_element(decode_describe_log_dirs_topic_1)),
    {
        #{
            error_code => ErrorCode,
            log_dir => LogDir,
            topics => Topics
        },
        Bin3
    }.

-spec encode_describe_log_dirs_response_2(describe_log_dirs_response_2()) -> iodata().

encode_describe_log_dirs_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The log directories.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Results, fun encode_describe_log_dirs_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, describe_log_dirs_result_2}
    }).

-spec decode_describe_log_dirs_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_response_2(),
    Rest :: binary().

decode_describe_log_dirs_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Results, Bin1, Bin2, ?_decode_element(decode_describe_log_dirs_result_2)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    ).

-spec decode_describe_log_dirs_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_response_2().

decode_describe_log_dirs_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_partition_2(describe_log_dirs_partition_2()) -> iodata().

encode_describe_log_dirs_partition_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The size of the log segments in this partition in bytes.
        partition_size := PartitionSize,
        % The lag of the log's LEO w.r.t. partition's HW (if it is the current log for the partition) or current replica's LEO (if it is the future log for the partition)
        offset_lag := OffsetLag,
        % True if this log is created by AlterReplicaLogDirsRequest and will replace the current log of the replica in the future.
        is_future_key := IsFutureKey
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(PartitionSize),
    ?is_int64(OffsetLag),
    ?is_bool(IsFutureKey)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(PartitionSize),
        ?encode_int64(OffsetLag),
        ?encode_bool(IsFutureKey),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_size => int64,
        offset_lag => int64,
        is_future_key => bool
    }).

-spec decode_describe_log_dirs_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_partition_2(),
    Rest :: binary().

decode_describe_log_dirs_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(PartitionSize, Bin1, Bin2),
    ?_decode_int64(OffsetLag, Bin2, Bin3),
    ?_decode_bool(IsFutureKey, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_partition_2_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            partition_size => PartitionSize,
            offset_lag => OffsetLag,
            is_future_key => IsFutureKey
        },
        Bin4
    ).

-spec decode_describe_log_dirs_partition_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_partition_2().

decode_describe_log_dirs_partition_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_topic_2(describe_log_dirs_topic_2()) -> iodata().

encode_describe_log_dirs_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_describe_log_dirs_partition_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, describe_log_dirs_partition_2}
    }).

-spec decode_describe_log_dirs_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_topic_2(),
    Rest :: binary().

decode_describe_log_dirs_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_describe_log_dirs_partition_2)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_topic_2_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_describe_log_dirs_topic_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_topic_2().

decode_describe_log_dirs_topic_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_result_2(describe_log_dirs_result_2()) -> iodata().

encode_describe_log_dirs_result_2(
    _Args = #{
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The absolute log directory path.
        log_dir := LogDir,
        % Each topic.
        topics := Topics
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(LogDir),
    ?is_array(Topics)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_string(LogDir),
        ?encode_compact_array(Topics, fun encode_describe_log_dirs_topic_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_result_2(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        log_dir => string,
        topics => {array, describe_log_dirs_topic_2}
    }).

-spec decode_describe_log_dirs_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_result_2(),
    Rest :: binary().

decode_describe_log_dirs_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(LogDir, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_describe_log_dirs_topic_2)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_result_2_tagged_field/3,
        #{
            error_code => ErrorCode,
            log_dir => LogDir,
            topics => Topics
        },
        Bin3
    ).

-spec decode_describe_log_dirs_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_result_2().

decode_describe_log_dirs_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_response_3(describe_log_dirs_response_3()) -> iodata().

encode_describe_log_dirs_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The log directories.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Results)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Results, fun encode_describe_log_dirs_result_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        results => {array, describe_log_dirs_result_3}
    }).

-spec decode_describe_log_dirs_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_response_3(),
    Rest :: binary().

decode_describe_log_dirs_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Results, Bin2, Bin3, ?_decode_element(decode_describe_log_dirs_result_3)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            results => Results
        },
        Bin3
    ).

-spec decode_describe_log_dirs_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_response_3().

decode_describe_log_dirs_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_partition_3(describe_log_dirs_partition_3()) -> iodata().

encode_describe_log_dirs_partition_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The size of the log segments in this partition in bytes.
        partition_size := PartitionSize,
        % The lag of the log's LEO w.r.t. partition's HW (if it is the current log for the partition) or current replica's LEO (if it is the future log for the partition)
        offset_lag := OffsetLag,
        % True if this log is created by AlterReplicaLogDirsRequest and will replace the current log of the replica in the future.
        is_future_key := IsFutureKey
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(PartitionSize),
    ?is_int64(OffsetLag),
    ?is_bool(IsFutureKey)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(PartitionSize),
        ?encode_int64(OffsetLag),
        ?encode_bool(IsFutureKey),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_partition_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_size => int64,
        offset_lag => int64,
        is_future_key => bool
    }).

-spec decode_describe_log_dirs_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_partition_3(),
    Rest :: binary().

decode_describe_log_dirs_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(PartitionSize, Bin1, Bin2),
    ?_decode_int64(OffsetLag, Bin2, Bin3),
    ?_decode_bool(IsFutureKey, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_partition_3_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            partition_size => PartitionSize,
            offset_lag => OffsetLag,
            is_future_key => IsFutureKey
        },
        Bin4
    ).

-spec decode_describe_log_dirs_partition_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_partition_3().

decode_describe_log_dirs_partition_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_topic_3(describe_log_dirs_topic_3()) -> iodata().

encode_describe_log_dirs_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_describe_log_dirs_partition_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, describe_log_dirs_partition_3}
    }).

-spec decode_describe_log_dirs_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_topic_3(),
    Rest :: binary().

decode_describe_log_dirs_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_describe_log_dirs_partition_3)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_topic_3_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_describe_log_dirs_topic_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_topic_3().

decode_describe_log_dirs_topic_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_result_3(describe_log_dirs_result_3()) -> iodata().

encode_describe_log_dirs_result_3(
    _Args = #{
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The absolute log directory path.
        log_dir := LogDir,
        % Each topic.
        topics := Topics
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(LogDir),
    ?is_array(Topics)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_string(LogDir),
        ?encode_compact_array(Topics, fun encode_describe_log_dirs_topic_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_result_3(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        log_dir => string,
        topics => {array, describe_log_dirs_topic_3}
    }).

-spec decode_describe_log_dirs_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_result_3(),
    Rest :: binary().

decode_describe_log_dirs_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(LogDir, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_describe_log_dirs_topic_3)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_result_3_tagged_field/3,
        #{
            error_code => ErrorCode,
            log_dir => LogDir,
            topics => Topics
        },
        Bin3
    ).

-spec decode_describe_log_dirs_result_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_result_3().

decode_describe_log_dirs_result_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_response_4(describe_log_dirs_response_4()) -> iodata().

encode_describe_log_dirs_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The log directories.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Results)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Results, fun encode_describe_log_dirs_result_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        results => {array, describe_log_dirs_result_4}
    }).

-spec decode_describe_log_dirs_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_response_4(),
    Rest :: binary().

decode_describe_log_dirs_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Results, Bin2, Bin3, ?_decode_element(decode_describe_log_dirs_result_4)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            results => Results
        },
        Bin3
    ).

-spec decode_describe_log_dirs_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_response_4().

decode_describe_log_dirs_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_partition_4(describe_log_dirs_partition_4()) -> iodata().

encode_describe_log_dirs_partition_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The size of the log segments in this partition in bytes.
        partition_size := PartitionSize,
        % The lag of the log's LEO w.r.t. partition's HW (if it is the current log for the partition) or current replica's LEO (if it is the future log for the partition)
        offset_lag := OffsetLag,
        % True if this log is created by AlterReplicaLogDirsRequest and will replace the current log of the replica in the future.
        is_future_key := IsFutureKey
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(PartitionSize),
    ?is_int64(OffsetLag),
    ?is_bool(IsFutureKey)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(PartitionSize),
        ?encode_int64(OffsetLag),
        ?encode_bool(IsFutureKey),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_partition_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_size => int64,
        offset_lag => int64,
        is_future_key => bool
    }).

-spec decode_describe_log_dirs_partition_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_partition_4(),
    Rest :: binary().

decode_describe_log_dirs_partition_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(PartitionSize, Bin1, Bin2),
    ?_decode_int64(OffsetLag, Bin2, Bin3),
    ?_decode_bool(IsFutureKey, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_partition_4_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            partition_size => PartitionSize,
            offset_lag => OffsetLag,
            is_future_key => IsFutureKey
        },
        Bin4
    ).

-spec decode_describe_log_dirs_partition_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_partition_4().

decode_describe_log_dirs_partition_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_topic_4(describe_log_dirs_topic_4()) -> iodata().

encode_describe_log_dirs_topic_4(
    _Args = #{
        % The topic name.
        name := Name,
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_describe_log_dirs_partition_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, describe_log_dirs_partition_4}
    }).

-spec decode_describe_log_dirs_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_topic_4(),
    Rest :: binary().

decode_describe_log_dirs_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_describe_log_dirs_partition_4)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_topic_4_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_describe_log_dirs_topic_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_topic_4().

decode_describe_log_dirs_topic_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_result_4(describe_log_dirs_result_4()) -> iodata().

encode_describe_log_dirs_result_4(
    _Args = #{
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The absolute log directory path.
        log_dir := LogDir,
        % Each topic.
        topics := Topics,
        % The total size in bytes of the volume the log directory is in.
        total_bytes := TotalBytes,
        % The usable size in bytes of the volume the log directory is in.
        usable_bytes := UsableBytes
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(LogDir),
    ?is_array(Topics),
    ?is_int64(TotalBytes),
    ?is_int64(UsableBytes)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_string(LogDir),
        ?encode_compact_array(Topics, fun encode_describe_log_dirs_topic_4/1),
        ?encode_int64(TotalBytes),
        ?encode_int64(UsableBytes),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_result_4(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        log_dir => string,
        topics => {array, describe_log_dirs_topic_4},
        total_bytes => int64,
        usable_bytes => int64
    }).

-spec decode_describe_log_dirs_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_result_4(),
    Rest :: binary().

decode_describe_log_dirs_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(LogDir, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_describe_log_dirs_topic_4)),
    ?_decode_int64(TotalBytes, Bin3, Bin4),
    ?_decode_int64(UsableBytes, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_result_4_tagged_field/3,
        #{
            error_code => ErrorCode,
            log_dir => LogDir,
            topics => Topics,
            total_bytes => TotalBytes,
            usable_bytes => UsableBytes
        },
        Bin5
    ).

-spec decode_describe_log_dirs_result_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_log_dirs_result_4().

decode_describe_log_dirs_result_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_log_dirs_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(describe_log_dirs_result_0())
}.
-type describe_log_dirs_partition_0() :: #{
    partition_index := integer(),
    partition_size := integer(),
    offset_lag := integer(),
    is_future_key := boolean()
}.
-type describe_log_dirs_topic_0() :: #{
    name := binary(),
    partitions := list(describe_log_dirs_partition_0())
}.
-type describe_log_dirs_result_0() :: #{
    error_code := integer(),
    log_dir := binary(),
    topics := list(describe_log_dirs_topic_0())
}.
-type describe_log_dirs_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(describe_log_dirs_result_1())
}.
-type describe_log_dirs_partition_1() :: #{
    partition_index := integer(),
    partition_size := integer(),
    offset_lag := integer(),
    is_future_key := boolean()
}.
-type describe_log_dirs_topic_1() :: #{
    name := binary(),
    partitions := list(describe_log_dirs_partition_1())
}.
-type describe_log_dirs_result_1() :: #{
    error_code := integer(),
    log_dir := binary(),
    topics := list(describe_log_dirs_topic_1())
}.
-type describe_log_dirs_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(describe_log_dirs_result_2())
}.
-type describe_log_dirs_partition_2() :: #{
    partition_index := integer(),
    partition_size := integer(),
    offset_lag := integer(),
    is_future_key := boolean()
}.
-type describe_log_dirs_topic_2() :: #{
    name := binary(),
    partitions := list(describe_log_dirs_partition_2())
}.
-type describe_log_dirs_result_2() :: #{
    error_code := integer(),
    log_dir := binary(),
    topics := list(describe_log_dirs_topic_2())
}.
-type describe_log_dirs_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    results := list(describe_log_dirs_result_3())
}.
-type describe_log_dirs_partition_3() :: #{
    partition_index := integer(),
    partition_size := integer(),
    offset_lag := integer(),
    is_future_key := boolean()
}.
-type describe_log_dirs_topic_3() :: #{
    name := binary(),
    partitions := list(describe_log_dirs_partition_3())
}.
-type describe_log_dirs_result_3() :: #{
    error_code := integer(),
    log_dir := binary(),
    topics := list(describe_log_dirs_topic_3())
}.
-type describe_log_dirs_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    results := list(describe_log_dirs_result_4())
}.
-type describe_log_dirs_partition_4() :: #{
    partition_index := integer(),
    partition_size := integer(),
    offset_lag := integer(),
    is_future_key := boolean()
}.
-type describe_log_dirs_topic_4() :: #{
    name := binary(),
    partitions := list(describe_log_dirs_partition_4())
}.
-type describe_log_dirs_result_4() :: #{
    error_code := integer(),
    log_dir := binary(),
    topics := list(describe_log_dirs_topic_4()),
    total_bytes := integer(),
    usable_bytes := integer()
}.
