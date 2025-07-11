-module(list_offsets_response).
-export([
    encode_list_offsets_response_0/1,
    decode_list_offsets_response_0/1,
    encode_list_offsets_response_1/1,
    decode_list_offsets_response_1/1,
    encode_list_offsets_response_2/1,
    decode_list_offsets_response_2/1,
    encode_list_offsets_response_3/1,
    decode_list_offsets_response_3/1,
    encode_list_offsets_response_4/1,
    decode_list_offsets_response_4/1,
    encode_list_offsets_response_5/1,
    decode_list_offsets_response_5/1,
    encode_list_offsets_response_6/1,
    decode_list_offsets_response_6/1,
    encode_list_offsets_response_7/1,
    decode_list_offsets_response_7/1,
    encode_list_offsets_response_8/1,
    decode_list_offsets_response_8/1
]).
-export_type([
    list_offsets_response_0/0,
    list_offsets_partition_response_0/0,
    list_offsets_topic_response_0/0,
    list_offsets_response_1/0,
    list_offsets_partition_response_1/0,
    list_offsets_topic_response_1/0,
    list_offsets_response_2/0,
    list_offsets_partition_response_2/0,
    list_offsets_topic_response_2/0,
    list_offsets_response_3/0,
    list_offsets_partition_response_3/0,
    list_offsets_topic_response_3/0,
    list_offsets_response_4/0,
    list_offsets_partition_response_4/0,
    list_offsets_topic_response_4/0,
    list_offsets_response_5/0,
    list_offsets_partition_response_5/0,
    list_offsets_topic_response_5/0,
    list_offsets_response_6/0,
    list_offsets_partition_response_6/0,
    list_offsets_topic_response_6/0,
    list_offsets_response_7/0,
    list_offsets_partition_response_7/0,
    list_offsets_topic_response_7/0,
    list_offsets_response_8/0,
    list_offsets_partition_response_8/0,
    list_offsets_topic_response_8/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_offsets_response_0(list_offsets_response_0()) -> iodata().

encode_list_offsets_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_list_offsets_topic_response_0/1)
    ];
encode_list_offsets_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, list_offsets_topic_response_0}
    }).

-spec decode_list_offsets_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_0(),
    Rest :: binary().

decode_list_offsets_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_list_offsets_topic_response_0)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_list_offsets_partition_response_0(list_offsets_partition_response_0()) -> iodata().

encode_list_offsets_partition_response_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The result offsets.
        old_style_offsets := OldStyleOffsets
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_array(OldStyleOffsets)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_array(OldStyleOffsets, ?encode_int64_)
    ];
encode_list_offsets_partition_response_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        old_style_offsets => {array, int64}
    }).

-spec decode_list_offsets_partition_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_0(),
    Rest :: binary().

decode_list_offsets_partition_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_array(OldStyleOffsets, Bin2, Bin3, ?decode_int64_),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            old_style_offsets => OldStyleOffsets
        },
        Bin3
    }.

-spec encode_list_offsets_topic_response_0(list_offsets_topic_response_0()) -> iodata().

encode_list_offsets_topic_response_0(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_response_0/1)
    ];
encode_list_offsets_topic_response_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_0}
    }).

-spec decode_list_offsets_topic_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_0(),
    Rest :: binary().

decode_list_offsets_topic_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_response_1(list_offsets_response_1()) -> iodata().

encode_list_offsets_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_list_offsets_topic_response_1/1)
    ];
encode_list_offsets_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, list_offsets_topic_response_1}
    }).

-spec decode_list_offsets_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_1(),
    Rest :: binary().

decode_list_offsets_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_list_offsets_topic_response_1)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_list_offsets_partition_response_1(list_offsets_partition_response_1()) -> iodata().

encode_list_offsets_partition_response_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp associated with the returned offset.
        timestamp := Timestamp,
        % The returned offset.
        offset := Offset
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(Timestamp),
    ?is_int64(Offset)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(Timestamp),
        ?encode_int64(Offset)
    ];
encode_list_offsets_partition_response_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        timestamp => int64,
        offset => int64
    }).

-spec decode_list_offsets_partition_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_1(),
    Rest :: binary().

decode_list_offsets_partition_response_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?_decode_int64(Offset, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            timestamp => Timestamp,
            offset => Offset
        },
        Bin4
    }.

-spec encode_list_offsets_topic_response_1(list_offsets_topic_response_1()) -> iodata().

encode_list_offsets_topic_response_1(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_response_1/1)
    ];
encode_list_offsets_topic_response_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_1}
    }).

-spec decode_list_offsets_topic_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_1(),
    Rest :: binary().

decode_list_offsets_topic_response_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_response_2(list_offsets_response_2()) -> iodata().

encode_list_offsets_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_list_offsets_topic_response_2/1)
    ];
encode_list_offsets_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, list_offsets_topic_response_2}
    }).

-spec decode_list_offsets_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_2(),
    Rest :: binary().

decode_list_offsets_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_response_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_list_offsets_partition_response_2(list_offsets_partition_response_2()) -> iodata().

encode_list_offsets_partition_response_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp associated with the returned offset.
        timestamp := Timestamp,
        % The returned offset.
        offset := Offset
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(Timestamp),
    ?is_int64(Offset)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(Timestamp),
        ?encode_int64(Offset)
    ];
encode_list_offsets_partition_response_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        timestamp => int64,
        offset => int64
    }).

-spec decode_list_offsets_partition_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_2(),
    Rest :: binary().

decode_list_offsets_partition_response_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?_decode_int64(Offset, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            timestamp => Timestamp,
            offset => Offset
        },
        Bin4
    }.

-spec encode_list_offsets_topic_response_2(list_offsets_topic_response_2()) -> iodata().

encode_list_offsets_topic_response_2(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_response_2/1)
    ];
encode_list_offsets_topic_response_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_2}
    }).

-spec decode_list_offsets_topic_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_2(),
    Rest :: binary().

decode_list_offsets_topic_response_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_2)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_response_3(list_offsets_response_3()) -> iodata().

encode_list_offsets_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_list_offsets_topic_response_3/1)
    ];
encode_list_offsets_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, list_offsets_topic_response_3}
    }).

-spec decode_list_offsets_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_3(),
    Rest :: binary().

decode_list_offsets_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_response_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_list_offsets_partition_response_3(list_offsets_partition_response_3()) -> iodata().

encode_list_offsets_partition_response_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp associated with the returned offset.
        timestamp := Timestamp,
        % The returned offset.
        offset := Offset
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(Timestamp),
    ?is_int64(Offset)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(Timestamp),
        ?encode_int64(Offset)
    ];
encode_list_offsets_partition_response_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        timestamp => int64,
        offset => int64
    }).

-spec decode_list_offsets_partition_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_3(),
    Rest :: binary().

decode_list_offsets_partition_response_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?_decode_int64(Offset, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            timestamp => Timestamp,
            offset => Offset
        },
        Bin4
    }.

-spec encode_list_offsets_topic_response_3(list_offsets_topic_response_3()) -> iodata().

encode_list_offsets_topic_response_3(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_response_3/1)
    ];
encode_list_offsets_topic_response_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_3}
    }).

-spec decode_list_offsets_topic_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_3(),
    Rest :: binary().

decode_list_offsets_topic_response_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_3)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_response_4(list_offsets_response_4()) -> iodata().

encode_list_offsets_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_list_offsets_topic_response_4/1)
    ];
encode_list_offsets_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, list_offsets_topic_response_4}
    }).

-spec decode_list_offsets_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_4(),
    Rest :: binary().

decode_list_offsets_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_response_4)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_list_offsets_partition_response_4(list_offsets_partition_response_4()) -> iodata().

encode_list_offsets_partition_response_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp associated with the returned offset.
        timestamp := Timestamp,
        % The returned offset.
        offset := Offset,
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(Timestamp),
    ?is_int64(Offset),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(Timestamp),
        ?encode_int64(Offset),
        ?encode_int32(LeaderEpoch)
    ];
encode_list_offsets_partition_response_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        timestamp => int64,
        offset => int64,
        leader_epoch => int32
    }).

-spec decode_list_offsets_partition_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_4(),
    Rest :: binary().

decode_list_offsets_partition_response_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?_decode_int64(Offset, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            timestamp => Timestamp,
            offset => Offset,
            leader_epoch => LeaderEpoch
        },
        Bin5
    }.

-spec encode_list_offsets_topic_response_4(list_offsets_topic_response_4()) -> iodata().

encode_list_offsets_topic_response_4(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_response_4/1)
    ];
encode_list_offsets_topic_response_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_4}
    }).

-spec decode_list_offsets_topic_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_4(),
    Rest :: binary().

decode_list_offsets_topic_response_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_4)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_response_5(list_offsets_response_5()) -> iodata().

encode_list_offsets_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_list_offsets_topic_response_5/1)
    ];
encode_list_offsets_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, list_offsets_topic_response_5}
    }).

-spec decode_list_offsets_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_5(),
    Rest :: binary().

decode_list_offsets_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_response_5)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_list_offsets_partition_response_5(list_offsets_partition_response_5()) -> iodata().

encode_list_offsets_partition_response_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp associated with the returned offset.
        timestamp := Timestamp,
        % The returned offset.
        offset := Offset,
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(Timestamp),
    ?is_int64(Offset),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(Timestamp),
        ?encode_int64(Offset),
        ?encode_int32(LeaderEpoch)
    ];
encode_list_offsets_partition_response_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        timestamp => int64,
        offset => int64,
        leader_epoch => int32
    }).

-spec decode_list_offsets_partition_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_5(),
    Rest :: binary().

decode_list_offsets_partition_response_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?_decode_int64(Offset, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            timestamp => Timestamp,
            offset => Offset,
            leader_epoch => LeaderEpoch
        },
        Bin5
    }.

-spec encode_list_offsets_topic_response_5(list_offsets_topic_response_5()) -> iodata().

encode_list_offsets_topic_response_5(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_response_5/1)
    ];
encode_list_offsets_topic_response_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_5}
    }).

-spec decode_list_offsets_topic_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_5(),
    Rest :: binary().

decode_list_offsets_topic_response_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_5)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_response_6(list_offsets_response_6()) -> iodata().

encode_list_offsets_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_list_offsets_topic_response_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, list_offsets_topic_response_6}
    }).

-spec decode_list_offsets_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_6(),
    Rest :: binary().

decode_list_offsets_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_response_6)),
    ?decode_tagged_fields(
        fun decode_list_offsets_response_6_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_list_offsets_response_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_response_6().

decode_list_offsets_response_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_partition_response_6(list_offsets_partition_response_6()) -> iodata().

encode_list_offsets_partition_response_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp associated with the returned offset.
        timestamp := Timestamp,
        % The returned offset.
        offset := Offset,
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(Timestamp),
    ?is_int64(Offset),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(Timestamp),
        ?encode_int64(Offset),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_partition_response_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        timestamp => int64,
        offset => int64,
        leader_epoch => int32
    }).

-spec decode_list_offsets_partition_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_6(),
    Rest :: binary().

decode_list_offsets_partition_response_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?_decode_int64(Offset, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_list_offsets_partition_response_6_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            timestamp => Timestamp,
            offset => Offset,
            leader_epoch => LeaderEpoch
        },
        Bin5
    ).

-spec decode_list_offsets_partition_response_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_partition_response_6().

decode_list_offsets_partition_response_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_topic_response_6(list_offsets_topic_response_6()) -> iodata().

encode_list_offsets_topic_response_6(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_list_offsets_partition_response_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_topic_response_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_6}
    }).

-spec decode_list_offsets_topic_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_6(),
    Rest :: binary().

decode_list_offsets_topic_response_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_6)),
    ?decode_tagged_fields(
        fun decode_list_offsets_topic_response_6_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_list_offsets_topic_response_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_topic_response_6().

decode_list_offsets_topic_response_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_response_7(list_offsets_response_7()) -> iodata().

encode_list_offsets_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_list_offsets_topic_response_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, list_offsets_topic_response_7}
    }).

-spec decode_list_offsets_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_7(),
    Rest :: binary().

decode_list_offsets_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_response_7)),
    ?decode_tagged_fields(
        fun decode_list_offsets_response_7_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_list_offsets_response_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_response_7().

decode_list_offsets_response_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_partition_response_7(list_offsets_partition_response_7()) -> iodata().

encode_list_offsets_partition_response_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp associated with the returned offset.
        timestamp := Timestamp,
        % The returned offset.
        offset := Offset,
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(Timestamp),
    ?is_int64(Offset),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(Timestamp),
        ?encode_int64(Offset),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_partition_response_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        timestamp => int64,
        offset => int64,
        leader_epoch => int32
    }).

-spec decode_list_offsets_partition_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_7(),
    Rest :: binary().

decode_list_offsets_partition_response_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?_decode_int64(Offset, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_list_offsets_partition_response_7_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            timestamp => Timestamp,
            offset => Offset,
            leader_epoch => LeaderEpoch
        },
        Bin5
    ).

-spec decode_list_offsets_partition_response_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_partition_response_7().

decode_list_offsets_partition_response_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_topic_response_7(list_offsets_topic_response_7()) -> iodata().

encode_list_offsets_topic_response_7(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_list_offsets_partition_response_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_topic_response_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_7}
    }).

-spec decode_list_offsets_topic_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_7(),
    Rest :: binary().

decode_list_offsets_topic_response_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_7)),
    ?decode_tagged_fields(
        fun decode_list_offsets_topic_response_7_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_list_offsets_topic_response_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_topic_response_7().

decode_list_offsets_topic_response_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_response_8(list_offsets_response_8()) -> iodata().

encode_list_offsets_response_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_list_offsets_topic_response_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_response_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, list_offsets_topic_response_8}
    }).

-spec decode_list_offsets_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_response_8(),
    Rest :: binary().

decode_list_offsets_response_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_response_8)),
    ?decode_tagged_fields(
        fun decode_list_offsets_response_8_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_list_offsets_response_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_response_8().

decode_list_offsets_response_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_partition_response_8(list_offsets_partition_response_8()) -> iodata().

encode_list_offsets_partition_response_8(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp associated with the returned offset.
        timestamp := Timestamp,
        % The returned offset.
        offset := Offset,
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int64(Timestamp),
    ?is_int64(Offset),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int64(Timestamp),
        ?encode_int64(Offset),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_partition_response_8(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        timestamp => int64,
        offset => int64,
        leader_epoch => int32
    }).

-spec decode_list_offsets_partition_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_response_8(),
    Rest :: binary().

decode_list_offsets_partition_response_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?_decode_int64(Offset, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_list_offsets_partition_response_8_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            timestamp => Timestamp,
            offset => Offset,
            leader_epoch => LeaderEpoch
        },
        Bin5
    ).

-spec decode_list_offsets_partition_response_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_partition_response_8().

decode_list_offsets_partition_response_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_topic_response_8(list_offsets_topic_response_8()) -> iodata().

encode_list_offsets_topic_response_8(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition in the response.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_list_offsets_partition_response_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_topic_response_8(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_response_8}
    }).

-spec decode_list_offsets_topic_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_response_8(),
    Rest :: binary().

decode_list_offsets_topic_response_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_response_8)),
    ?decode_tagged_fields(
        fun decode_list_offsets_topic_response_8_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_list_offsets_topic_response_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_topic_response_8().

decode_list_offsets_topic_response_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_offsets_response_0() :: #{
    correlation_id => integer(),
    topics := list(list_offsets_topic_response_0())
}.
-type list_offsets_partition_response_0() :: #{
    partition_index := integer(),
    error_code := integer(),
    old_style_offsets := list(integer())
}.
-type list_offsets_topic_response_0() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_0())
}.
-type list_offsets_response_1() :: #{
    correlation_id => integer(),
    topics := list(list_offsets_topic_response_1())
}.
-type list_offsets_partition_response_1() :: #{
    partition_index := integer(),
    error_code := integer(),
    timestamp := integer(),
    offset := integer()
}.
-type list_offsets_topic_response_1() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_1())
}.
-type list_offsets_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(list_offsets_topic_response_2())
}.
-type list_offsets_partition_response_2() :: #{
    partition_index := integer(),
    error_code := integer(),
    timestamp := integer(),
    offset := integer()
}.
-type list_offsets_topic_response_2() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_2())
}.
-type list_offsets_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(list_offsets_topic_response_3())
}.
-type list_offsets_partition_response_3() :: #{
    partition_index := integer(),
    error_code := integer(),
    timestamp := integer(),
    offset := integer()
}.
-type list_offsets_topic_response_3() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_3())
}.
-type list_offsets_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(list_offsets_topic_response_4())
}.
-type list_offsets_partition_response_4() :: #{
    partition_index := integer(),
    error_code := integer(),
    timestamp := integer(),
    offset := integer(),
    leader_epoch := integer()
}.
-type list_offsets_topic_response_4() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_4())
}.
-type list_offsets_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(list_offsets_topic_response_5())
}.
-type list_offsets_partition_response_5() :: #{
    partition_index := integer(),
    error_code := integer(),
    timestamp := integer(),
    offset := integer(),
    leader_epoch := integer()
}.
-type list_offsets_topic_response_5() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_5())
}.
-type list_offsets_response_6() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(list_offsets_topic_response_6())
}.
-type list_offsets_partition_response_6() :: #{
    partition_index := integer(),
    error_code := integer(),
    timestamp := integer(),
    offset := integer(),
    leader_epoch := integer()
}.
-type list_offsets_topic_response_6() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_6())
}.
-type list_offsets_response_7() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(list_offsets_topic_response_7())
}.
-type list_offsets_partition_response_7() :: #{
    partition_index := integer(),
    error_code := integer(),
    timestamp := integer(),
    offset := integer(),
    leader_epoch := integer()
}.
-type list_offsets_topic_response_7() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_7())
}.
-type list_offsets_response_8() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(list_offsets_topic_response_8())
}.
-type list_offsets_partition_response_8() :: #{
    partition_index := integer(),
    error_code := integer(),
    timestamp := integer(),
    offset := integer(),
    leader_epoch := integer()
}.
-type list_offsets_topic_response_8() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_response_8())
}.
