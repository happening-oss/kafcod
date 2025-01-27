-module(offset_delete_response).
-export([
    encode_offset_delete_response_0/1,
    decode_offset_delete_response_0/1
]).
-export_type([
    offset_delete_response_0/0,
    offset_delete_response_partition_0/0,
    offset_delete_response_topic_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_offset_delete_response_0(offset_delete_response_0()) -> iodata().

encode_offset_delete_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_offset_delete_response_topic_0/1)
    ];
encode_offset_delete_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        throttle_time_ms => int32,
        topics => {array, offset_delete_response_topic_0}
    }).

-spec decode_offset_delete_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_delete_response_0(),
    Rest :: binary().

decode_offset_delete_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    ?_decode_array(Topics, Bin2, Bin3, ?_decode_element(decode_offset_delete_response_topic_0)),
    {
        Header#{
            error_code => ErrorCode,
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin3
    }.

-spec encode_offset_delete_response_partition_0(offset_delete_response_partition_0()) -> iodata().

encode_offset_delete_response_partition_0(
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
encode_offset_delete_response_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_offset_delete_response_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_delete_response_partition_0(),
    Rest :: binary().

decode_offset_delete_response_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_offset_delete_response_topic_0(offset_delete_response_topic_0()) -> iodata().

encode_offset_delete_response_topic_0(
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
        ?encode_array(Partitions, fun encode_offset_delete_response_partition_0/1)
    ];
encode_offset_delete_response_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_delete_response_partition_0}
    }).

-spec decode_offset_delete_response_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_delete_response_topic_0(),
    Rest :: binary().

decode_offset_delete_response_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_delete_response_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-type offset_delete_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    throttle_time_ms := integer(),
    topics := list(offset_delete_response_topic_0())
}.
-type offset_delete_response_partition_0() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type offset_delete_response_topic_0() :: #{
    name := binary(),
    partitions := list(offset_delete_response_partition_0())
}.
