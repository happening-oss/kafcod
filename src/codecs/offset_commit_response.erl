-module(offset_commit_response).
-export([
    encode_offset_commit_response_0/1,
    decode_offset_commit_response_0/1,
    encode_offset_commit_response_1/1,
    decode_offset_commit_response_1/1,
    encode_offset_commit_response_2/1,
    decode_offset_commit_response_2/1,
    encode_offset_commit_response_3/1,
    decode_offset_commit_response_3/1,
    encode_offset_commit_response_4/1,
    decode_offset_commit_response_4/1,
    encode_offset_commit_response_5/1,
    decode_offset_commit_response_5/1,
    encode_offset_commit_response_6/1,
    decode_offset_commit_response_6/1,
    encode_offset_commit_response_7/1,
    decode_offset_commit_response_7/1,
    encode_offset_commit_response_8/1,
    decode_offset_commit_response_8/1,
    encode_offset_commit_response_9/1,
    decode_offset_commit_response_9/1
]).
-export_type([
    offset_commit_response_0/0,
    offset_commit_response_partition_0/0,
    offset_commit_response_topic_0/0,
    offset_commit_response_1/0,
    offset_commit_response_partition_1/0,
    offset_commit_response_topic_1/0,
    offset_commit_response_2/0,
    offset_commit_response_partition_2/0,
    offset_commit_response_topic_2/0,
    offset_commit_response_3/0,
    offset_commit_response_partition_3/0,
    offset_commit_response_topic_3/0,
    offset_commit_response_4/0,
    offset_commit_response_partition_4/0,
    offset_commit_response_topic_4/0,
    offset_commit_response_5/0,
    offset_commit_response_partition_5/0,
    offset_commit_response_topic_5/0,
    offset_commit_response_6/0,
    offset_commit_response_partition_6/0,
    offset_commit_response_topic_6/0,
    offset_commit_response_7/0,
    offset_commit_response_partition_7/0,
    offset_commit_response_topic_7/0,
    offset_commit_response_8/0,
    offset_commit_response_partition_8/0,
    offset_commit_response_topic_8/0,
    offset_commit_response_9/0,
    offset_commit_response_partition_9/0,
    offset_commit_response_topic_9/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_offset_commit_response_0(offset_commit_response_0()) -> iodata().

encode_offset_commit_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_offset_commit_response_topic_0/1)
    ];
encode_offset_commit_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, offset_commit_response_topic_0}
    }).

-spec decode_offset_commit_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_0(),
    Rest :: binary().

decode_offset_commit_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_offset_commit_response_topic_0)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_offset_commit_response_partition_0(offset_commit_response_partition_0()) -> iodata().

encode_offset_commit_response_partition_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_offset_commit_response_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_0(),
    Rest :: binary().

decode_offset_commit_response_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_commit_response_topic_0(offset_commit_response_topic_0()) -> iodata().

encode_offset_commit_response_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_response_partition_0/1)
    ];
encode_offset_commit_response_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_0}
    }).

-spec decode_offset_commit_response_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_0(),
    Rest :: binary().

decode_offset_commit_response_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_response_1(offset_commit_response_1()) -> iodata().

encode_offset_commit_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_offset_commit_response_topic_1/1)
    ];
encode_offset_commit_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, offset_commit_response_topic_1}
    }).

-spec decode_offset_commit_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_1(),
    Rest :: binary().

decode_offset_commit_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_offset_commit_response_topic_1)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_offset_commit_response_partition_1(offset_commit_response_partition_1()) -> iodata().

encode_offset_commit_response_partition_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_offset_commit_response_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_1(),
    Rest :: binary().

decode_offset_commit_response_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_commit_response_topic_1(offset_commit_response_topic_1()) -> iodata().

encode_offset_commit_response_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_response_partition_1/1)
    ];
encode_offset_commit_response_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_1}
    }).

-spec decode_offset_commit_response_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_1(),
    Rest :: binary().

decode_offset_commit_response_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_response_2(offset_commit_response_2()) -> iodata().

encode_offset_commit_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_offset_commit_response_topic_2/1)
    ];
encode_offset_commit_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, offset_commit_response_topic_2}
    }).

-spec decode_offset_commit_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_2(),
    Rest :: binary().

decode_offset_commit_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_offset_commit_response_topic_2)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_offset_commit_response_partition_2(offset_commit_response_partition_2()) -> iodata().

encode_offset_commit_response_partition_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_offset_commit_response_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_2(),
    Rest :: binary().

decode_offset_commit_response_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_commit_response_topic_2(offset_commit_response_topic_2()) -> iodata().

encode_offset_commit_response_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_response_partition_2/1)
    ];
encode_offset_commit_response_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_2}
    }).

-spec decode_offset_commit_response_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_2(),
    Rest :: binary().

decode_offset_commit_response_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_2)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_response_3(offset_commit_response_3()) -> iodata().

encode_offset_commit_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
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
        ?encode_array(Topics, fun encode_offset_commit_response_topic_3/1)
    ];
encode_offset_commit_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_commit_response_topic_3}
    }).

-spec decode_offset_commit_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_3(),
    Rest :: binary().

decode_offset_commit_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_topic_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_commit_response_partition_3(offset_commit_response_partition_3()) -> iodata().

encode_offset_commit_response_partition_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_offset_commit_response_partition_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_3(),
    Rest :: binary().

decode_offset_commit_response_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_commit_response_topic_3(offset_commit_response_topic_3()) -> iodata().

encode_offset_commit_response_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_response_partition_3/1)
    ];
encode_offset_commit_response_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_3}
    }).

-spec decode_offset_commit_response_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_3(),
    Rest :: binary().

decode_offset_commit_response_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_3)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_response_4(offset_commit_response_4()) -> iodata().

encode_offset_commit_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
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
        ?encode_array(Topics, fun encode_offset_commit_response_topic_4/1)
    ];
encode_offset_commit_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_commit_response_topic_4}
    }).

-spec decode_offset_commit_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_4(),
    Rest :: binary().

decode_offset_commit_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_topic_4)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_commit_response_partition_4(offset_commit_response_partition_4()) -> iodata().

encode_offset_commit_response_partition_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_offset_commit_response_partition_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_4(),
    Rest :: binary().

decode_offset_commit_response_partition_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_commit_response_topic_4(offset_commit_response_topic_4()) -> iodata().

encode_offset_commit_response_topic_4(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_response_partition_4/1)
    ];
encode_offset_commit_response_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_4}
    }).

-spec decode_offset_commit_response_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_4(),
    Rest :: binary().

decode_offset_commit_response_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_4)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_response_5(offset_commit_response_5()) -> iodata().

encode_offset_commit_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
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
        ?encode_array(Topics, fun encode_offset_commit_response_topic_5/1)
    ];
encode_offset_commit_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_commit_response_topic_5}
    }).

-spec decode_offset_commit_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_5(),
    Rest :: binary().

decode_offset_commit_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_topic_5)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_commit_response_partition_5(offset_commit_response_partition_5()) -> iodata().

encode_offset_commit_response_partition_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_offset_commit_response_partition_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_5(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_5(),
    Rest :: binary().

decode_offset_commit_response_partition_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_commit_response_topic_5(offset_commit_response_topic_5()) -> iodata().

encode_offset_commit_response_topic_5(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_response_partition_5/1)
    ];
encode_offset_commit_response_topic_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_5}
    }).

-spec decode_offset_commit_response_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_5(),
    Rest :: binary().

decode_offset_commit_response_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_5)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_response_6(offset_commit_response_6()) -> iodata().

encode_offset_commit_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
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
        ?encode_array(Topics, fun encode_offset_commit_response_topic_6/1)
    ];
encode_offset_commit_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_commit_response_topic_6}
    }).

-spec decode_offset_commit_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_6(),
    Rest :: binary().

decode_offset_commit_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_topic_6)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_commit_response_partition_6(offset_commit_response_partition_6()) -> iodata().

encode_offset_commit_response_partition_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_offset_commit_response_partition_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_6(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_6(),
    Rest :: binary().

decode_offset_commit_response_partition_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_commit_response_topic_6(offset_commit_response_topic_6()) -> iodata().

encode_offset_commit_response_topic_6(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_response_partition_6/1)
    ];
encode_offset_commit_response_topic_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_6}
    }).

-spec decode_offset_commit_response_topic_6(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_6(),
    Rest :: binary().

decode_offset_commit_response_topic_6(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_6)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_response_7(offset_commit_response_7()) -> iodata().

encode_offset_commit_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
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
        ?encode_array(Topics, fun encode_offset_commit_response_topic_7/1)
    ];
encode_offset_commit_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_commit_response_topic_7}
    }).

-spec decode_offset_commit_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_7(),
    Rest :: binary().

decode_offset_commit_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_topic_7)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_commit_response_partition_7(offset_commit_response_partition_7()) -> iodata().

encode_offset_commit_response_partition_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_offset_commit_response_partition_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_7(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_7(),
    Rest :: binary().

decode_offset_commit_response_partition_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_commit_response_topic_7(offset_commit_response_topic_7()) -> iodata().

encode_offset_commit_response_topic_7(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_response_partition_7/1)
    ];
encode_offset_commit_response_topic_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_7}
    }).

-spec decode_offset_commit_response_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_7(),
    Rest :: binary().

decode_offset_commit_response_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_7)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_response_8(offset_commit_response_8()) -> iodata().

encode_offset_commit_response_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
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
        ?encode_compact_array(Topics, fun encode_offset_commit_response_topic_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_response_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_commit_response_topic_8}
    }).

-spec decode_offset_commit_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_8(),
    Rest :: binary().

decode_offset_commit_response_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_topic_8)),
    ?decode_tagged_fields(
        fun decode_offset_commit_response_8_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_offset_commit_response_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_commit_response_8().

decode_offset_commit_response_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_response_partition_8(offset_commit_response_partition_8()) -> iodata().

encode_offset_commit_response_partition_8(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_response_partition_8(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_8(),
    Rest :: binary().

decode_offset_commit_response_partition_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_offset_commit_response_partition_8_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_offset_commit_response_partition_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_commit_response_partition_8().

decode_offset_commit_response_partition_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_response_topic_8(offset_commit_response_topic_8()) -> iodata().

encode_offset_commit_response_topic_8(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_offset_commit_response_partition_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_response_topic_8(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_8}
    }).

-spec decode_offset_commit_response_topic_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_8(),
    Rest :: binary().

decode_offset_commit_response_topic_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_8)),
    ?decode_tagged_fields(
        fun decode_offset_commit_response_topic_8_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_offset_commit_response_topic_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_commit_response_topic_8().

decode_offset_commit_response_topic_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_response_9(offset_commit_response_9()) -> iodata().

encode_offset_commit_response_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
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
        ?encode_compact_array(Topics, fun encode_offset_commit_response_topic_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_response_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_commit_response_topic_9}
    }).

-spec decode_offset_commit_response_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_9(),
    Rest :: binary().

decode_offset_commit_response_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_topic_9)),
    ?decode_tagged_fields(
        fun decode_offset_commit_response_9_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_offset_commit_response_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_commit_response_9().

decode_offset_commit_response_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_response_partition_9(offset_commit_response_partition_9()) -> iodata().

encode_offset_commit_response_partition_9(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_response_partition_9(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_commit_response_partition_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_partition_9(),
    Rest :: binary().

decode_offset_commit_response_partition_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_offset_commit_response_partition_9_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_offset_commit_response_partition_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_commit_response_partition_9().

decode_offset_commit_response_partition_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_response_topic_9(offset_commit_response_topic_9()) -> iodata().

encode_offset_commit_response_topic_9(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_offset_commit_response_partition_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_response_topic_9(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_response_partition_9}
    }).

-spec decode_offset_commit_response_topic_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_response_topic_9(),
    Rest :: binary().

decode_offset_commit_response_topic_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_response_partition_9)),
    ?decode_tagged_fields(
        fun decode_offset_commit_response_topic_9_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_offset_commit_response_topic_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_commit_response_topic_9().

decode_offset_commit_response_topic_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type offset_commit_response_0() :: #{
    correlation_id => integer(),
    topics := list(offset_commit_response_topic_0())
}.
-type offset_commit_response_partition_0() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_0() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_0())
}.
-type offset_commit_response_1() :: #{
    correlation_id => integer(),
    topics := list(offset_commit_response_topic_1())
}.
-type offset_commit_response_partition_1() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_1() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_1())
}.
-type offset_commit_response_2() :: #{
    correlation_id => integer(),
    topics := list(offset_commit_response_topic_2())
}.
-type offset_commit_response_partition_2() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_2() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_2())
}.
-type offset_commit_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_commit_response_topic_3())
}.
-type offset_commit_response_partition_3() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_3() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_3())
}.
-type offset_commit_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_commit_response_topic_4())
}.
-type offset_commit_response_partition_4() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_4() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_4())
}.
-type offset_commit_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_commit_response_topic_5())
}.
-type offset_commit_response_partition_5() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_5() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_5())
}.
-type offset_commit_response_6() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_commit_response_topic_6())
}.
-type offset_commit_response_partition_6() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_6() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_6())
}.
-type offset_commit_response_7() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_commit_response_topic_7())
}.
-type offset_commit_response_partition_7() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_7() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_7())
}.
-type offset_commit_response_8() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_commit_response_topic_8())
}.
-type offset_commit_response_partition_8() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_8() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_8())
}.
-type offset_commit_response_9() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_commit_response_topic_9())
}.
-type offset_commit_response_partition_9() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_commit_response_topic_9() :: #{
    name := binary(),
    partitions := list(offset_commit_response_partition_9())
}.
