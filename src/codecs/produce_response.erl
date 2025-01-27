-module(produce_response).
-export([
    encode_produce_response_0/1,
    decode_produce_response_0/1,
    encode_produce_response_1/1,
    decode_produce_response_1/1,
    encode_produce_response_2/1,
    decode_produce_response_2/1,
    encode_produce_response_3/1,
    decode_produce_response_3/1,
    encode_produce_response_4/1,
    decode_produce_response_4/1,
    encode_produce_response_5/1,
    decode_produce_response_5/1,
    encode_produce_response_6/1,
    decode_produce_response_6/1,
    encode_produce_response_7/1,
    decode_produce_response_7/1,
    encode_produce_response_8/1,
    decode_produce_response_8/1,
    encode_produce_response_9/1,
    decode_produce_response_9/1
]).
-export_type([
    produce_response_0/0,
    partition_produce_response_0/0,
    topic_produce_response_0/0,
    produce_response_1/0,
    partition_produce_response_1/0,
    topic_produce_response_1/0,
    produce_response_2/0,
    partition_produce_response_2/0,
    topic_produce_response_2/0,
    produce_response_3/0,
    partition_produce_response_3/0,
    topic_produce_response_3/0,
    produce_response_4/0,
    partition_produce_response_4/0,
    topic_produce_response_4/0,
    produce_response_5/0,
    partition_produce_response_5/0,
    topic_produce_response_5/0,
    produce_response_6/0,
    partition_produce_response_6/0,
    topic_produce_response_6/0,
    produce_response_7/0,
    partition_produce_response_7/0,
    topic_produce_response_7/0,
    produce_response_8/0,
    batch_index_and_error_message_8/0,
    partition_produce_response_8/0,
    topic_produce_response_8/0,
    produce_response_9/0,
    batch_index_and_error_message_9/0,
    partition_produce_response_9/0,
    topic_produce_response_9/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_produce_response_0(produce_response_0()) -> iodata().

encode_produce_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_0/1)
    ];
encode_produce_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_0}
    }).

-spec decode_produce_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_0(),
    Rest :: binary().

decode_produce_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_0)),
    {
        Header#{
            responses => Responses
        },
        Bin1
    }.

-spec encode_partition_produce_response_0(partition_produce_response_0()) -> iodata().

encode_partition_produce_response_0(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset)
    ];
encode_partition_produce_response_0(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64
    }).

-spec decode_partition_produce_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_0(),
    Rest :: binary().

decode_partition_produce_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset
        },
        Bin3
    }.

-spec encode_topic_produce_response_0(topic_produce_response_0()) -> iodata().

encode_topic_produce_response_0(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_0/1)
    ];
encode_topic_produce_response_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_0}
    }).

-spec decode_topic_produce_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_0(),
    Rest :: binary().

decode_topic_produce_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_0)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_1(produce_response_1()) -> iodata().

encode_produce_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_1/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_produce_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_1},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_1(),
    Rest :: binary().

decode_produce_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_1)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    {
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    }.

-spec encode_partition_produce_response_1(partition_produce_response_1()) -> iodata().

encode_partition_produce_response_1(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset)
    ];
encode_partition_produce_response_1(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64
    }).

-spec decode_partition_produce_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_1(),
    Rest :: binary().

decode_partition_produce_response_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset
        },
        Bin3
    }.

-spec encode_topic_produce_response_1(topic_produce_response_1()) -> iodata().

encode_topic_produce_response_1(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_1/1)
    ];
encode_topic_produce_response_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_1}
    }).

-spec decode_topic_produce_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_1(),
    Rest :: binary().

decode_topic_produce_response_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_1)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_2(produce_response_2()) -> iodata().

encode_produce_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_2/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_produce_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_2},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_2(),
    Rest :: binary().

decode_produce_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_2)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    {
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    }.

-spec encode_partition_produce_response_2(partition_produce_response_2()) -> iodata().

encode_partition_produce_response_2(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset,
        % The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, the timestamp will be -1.  If LogAppendTime is used for the topic, the timestamp will be the broker local time when the messages are appended.
        log_append_time_ms := LogAppendTimeMs
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset),
    ?is_int64(LogAppendTimeMs)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset),
        ?encode_int64(LogAppendTimeMs)
    ];
encode_partition_produce_response_2(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64,
        log_append_time_ms => int64
    }).

-spec decode_partition_produce_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_2(),
    Rest :: binary().

decode_partition_produce_response_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    ?_decode_int64(LogAppendTimeMs, Bin3, Bin4),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset,
            log_append_time_ms => LogAppendTimeMs
        },
        Bin4
    }.

-spec encode_topic_produce_response_2(topic_produce_response_2()) -> iodata().

encode_topic_produce_response_2(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_2/1)
    ];
encode_topic_produce_response_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_2}
    }).

-spec decode_topic_produce_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_2(),
    Rest :: binary().

decode_topic_produce_response_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_2)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_3(produce_response_3()) -> iodata().

encode_produce_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_3/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_produce_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_3},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_3(),
    Rest :: binary().

decode_produce_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_3)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    {
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    }.

-spec encode_partition_produce_response_3(partition_produce_response_3()) -> iodata().

encode_partition_produce_response_3(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset,
        % The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, the timestamp will be -1.  If LogAppendTime is used for the topic, the timestamp will be the broker local time when the messages are appended.
        log_append_time_ms := LogAppendTimeMs
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset),
    ?is_int64(LogAppendTimeMs)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset),
        ?encode_int64(LogAppendTimeMs)
    ];
encode_partition_produce_response_3(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64,
        log_append_time_ms => int64
    }).

-spec decode_partition_produce_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_3(),
    Rest :: binary().

decode_partition_produce_response_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    ?_decode_int64(LogAppendTimeMs, Bin3, Bin4),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset,
            log_append_time_ms => LogAppendTimeMs
        },
        Bin4
    }.

-spec encode_topic_produce_response_3(topic_produce_response_3()) -> iodata().

encode_topic_produce_response_3(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_3/1)
    ];
encode_topic_produce_response_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_3}
    }).

-spec decode_topic_produce_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_3(),
    Rest :: binary().

decode_topic_produce_response_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_3)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_4(produce_response_4()) -> iodata().

encode_produce_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_4/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_produce_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_4},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_4(),
    Rest :: binary().

decode_produce_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_4)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    {
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    }.

-spec encode_partition_produce_response_4(partition_produce_response_4()) -> iodata().

encode_partition_produce_response_4(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset,
        % The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, the timestamp will be -1.  If LogAppendTime is used for the topic, the timestamp will be the broker local time when the messages are appended.
        log_append_time_ms := LogAppendTimeMs
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset),
    ?is_int64(LogAppendTimeMs)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset),
        ?encode_int64(LogAppendTimeMs)
    ];
encode_partition_produce_response_4(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64,
        log_append_time_ms => int64
    }).

-spec decode_partition_produce_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_4(),
    Rest :: binary().

decode_partition_produce_response_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    ?_decode_int64(LogAppendTimeMs, Bin3, Bin4),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset,
            log_append_time_ms => LogAppendTimeMs
        },
        Bin4
    }.

-spec encode_topic_produce_response_4(topic_produce_response_4()) -> iodata().

encode_topic_produce_response_4(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_4/1)
    ];
encode_topic_produce_response_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_4}
    }).

-spec decode_topic_produce_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_4(),
    Rest :: binary().

decode_topic_produce_response_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_4)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_5(produce_response_5()) -> iodata().

encode_produce_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_5/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_produce_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_5},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_5(),
    Rest :: binary().

decode_produce_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_5)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    {
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    }.

-spec encode_partition_produce_response_5(partition_produce_response_5()) -> iodata().

encode_partition_produce_response_5(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset,
        % The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, the timestamp will be -1.  If LogAppendTime is used for the topic, the timestamp will be the broker local time when the messages are appended.
        log_append_time_ms := LogAppendTimeMs,
        % The log start offset.
        log_start_offset := LogStartOffset
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset),
    ?is_int64(LogAppendTimeMs),
    ?is_int64(LogStartOffset)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset),
        ?encode_int64(LogAppendTimeMs),
        ?encode_int64(LogStartOffset)
    ];
encode_partition_produce_response_5(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64,
        log_append_time_ms => int64,
        log_start_offset => int64
    }).

-spec decode_partition_produce_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_5(),
    Rest :: binary().

decode_partition_produce_response_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    ?_decode_int64(LogAppendTimeMs, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset,
            log_append_time_ms => LogAppendTimeMs,
            log_start_offset => LogStartOffset
        },
        Bin5
    }.

-spec encode_topic_produce_response_5(topic_produce_response_5()) -> iodata().

encode_topic_produce_response_5(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_5/1)
    ];
encode_topic_produce_response_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_5}
    }).

-spec decode_topic_produce_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_5(),
    Rest :: binary().

decode_topic_produce_response_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_5)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_6(produce_response_6()) -> iodata().

encode_produce_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_6/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_produce_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_6},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_6(),
    Rest :: binary().

decode_produce_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_6)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    {
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    }.

-spec encode_partition_produce_response_6(partition_produce_response_6()) -> iodata().

encode_partition_produce_response_6(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset,
        % The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, the timestamp will be -1.  If LogAppendTime is used for the topic, the timestamp will be the broker local time when the messages are appended.
        log_append_time_ms := LogAppendTimeMs,
        % The log start offset.
        log_start_offset := LogStartOffset
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset),
    ?is_int64(LogAppendTimeMs),
    ?is_int64(LogStartOffset)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset),
        ?encode_int64(LogAppendTimeMs),
        ?encode_int64(LogStartOffset)
    ];
encode_partition_produce_response_6(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64,
        log_append_time_ms => int64,
        log_start_offset => int64
    }).

-spec decode_partition_produce_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_6(),
    Rest :: binary().

decode_partition_produce_response_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    ?_decode_int64(LogAppendTimeMs, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset,
            log_append_time_ms => LogAppendTimeMs,
            log_start_offset => LogStartOffset
        },
        Bin5
    }.

-spec encode_topic_produce_response_6(topic_produce_response_6()) -> iodata().

encode_topic_produce_response_6(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_6/1)
    ];
encode_topic_produce_response_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_6}
    }).

-spec decode_topic_produce_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_6(),
    Rest :: binary().

decode_topic_produce_response_6(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_6)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_7(produce_response_7()) -> iodata().

encode_produce_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_7/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_produce_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_7},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_7(),
    Rest :: binary().

decode_produce_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_7)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    {
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    }.

-spec encode_partition_produce_response_7(partition_produce_response_7()) -> iodata().

encode_partition_produce_response_7(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset,
        % The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, the timestamp will be -1.  If LogAppendTime is used for the topic, the timestamp will be the broker local time when the messages are appended.
        log_append_time_ms := LogAppendTimeMs,
        % The log start offset.
        log_start_offset := LogStartOffset
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset),
    ?is_int64(LogAppendTimeMs),
    ?is_int64(LogStartOffset)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset),
        ?encode_int64(LogAppendTimeMs),
        ?encode_int64(LogStartOffset)
    ];
encode_partition_produce_response_7(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64,
        log_append_time_ms => int64,
        log_start_offset => int64
    }).

-spec decode_partition_produce_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_7(),
    Rest :: binary().

decode_partition_produce_response_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    ?_decode_int64(LogAppendTimeMs, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset,
            log_append_time_ms => LogAppendTimeMs,
            log_start_offset => LogStartOffset
        },
        Bin5
    }.

-spec encode_topic_produce_response_7(topic_produce_response_7()) -> iodata().

encode_topic_produce_response_7(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_7/1)
    ];
encode_topic_produce_response_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_7}
    }).

-spec decode_topic_produce_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_7(),
    Rest :: binary().

decode_topic_produce_response_7(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_7)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_8(produce_response_8()) -> iodata().

encode_produce_response_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_topic_produce_response_8/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_produce_response_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_8},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_8(),
    Rest :: binary().

decode_produce_response_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_8)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    {
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    }.

-spec encode_batch_index_and_error_message_8(batch_index_and_error_message_8()) -> iodata().

encode_batch_index_and_error_message_8(
    _Args = #{
        % The batch index of the record that cause the batch to be dropped
        batch_index := BatchIndex,
        % The error message of the record that caused the batch to be dropped
        batch_index_error_message := BatchIndexErrorMessage
    }
) when
    ?is_int32(BatchIndex),
    ?is_nullable_string(BatchIndexErrorMessage)
->
    [
        ?encode_int32(BatchIndex),
        ?encode_nullable_string(BatchIndexErrorMessage)
    ];
encode_batch_index_and_error_message_8(Args) ->
    ?encoder_error(Args, #{
        batch_index => int32,
        batch_index_error_message => nullable_string
    }).

-spec decode_batch_index_and_error_message_8(binary()) -> {Decoded, Rest} when
    Decoded :: batch_index_and_error_message_8(),
    Rest :: binary().

decode_batch_index_and_error_message_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BatchIndex, Bin0, Bin1),
    ?_decode_nullable_string(BatchIndexErrorMessage, Bin1, Bin2),
    {
        #{
            batch_index => BatchIndex,
            batch_index_error_message => BatchIndexErrorMessage
        },
        Bin2
    }.

-spec encode_partition_produce_response_8(partition_produce_response_8()) -> iodata().

encode_partition_produce_response_8(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset,
        % The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, the timestamp will be -1.  If LogAppendTime is used for the topic, the timestamp will be the broker local time when the messages are appended.
        log_append_time_ms := LogAppendTimeMs,
        % The log start offset.
        log_start_offset := LogStartOffset,
        % The batch indices of records that caused the batch to be dropped
        record_errors := RecordErrors,
        % The global error message summarizing the common root cause of the records that caused the batch to be dropped
        error_message := ErrorMessage
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset),
    ?is_int64(LogAppendTimeMs),
    ?is_int64(LogStartOffset),
    ?is_array(RecordErrors),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset),
        ?encode_int64(LogAppendTimeMs),
        ?encode_int64(LogStartOffset),
        ?encode_array(RecordErrors, fun encode_batch_index_and_error_message_8/1),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_partition_produce_response_8(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64,
        log_append_time_ms => int64,
        log_start_offset => int64,
        record_errors => {array, batch_index_and_error_message_8},
        error_message => nullable_string
    }).

-spec decode_partition_produce_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_8(),
    Rest :: binary().

decode_partition_produce_response_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    ?_decode_int64(LogAppendTimeMs, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_array(RecordErrors, Bin5, Bin6, ?_decode_element(decode_batch_index_and_error_message_8)),
    ?_decode_nullable_string(ErrorMessage, Bin6, Bin7),
    {
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset,
            log_append_time_ms => LogAppendTimeMs,
            log_start_offset => LogStartOffset,
            record_errors => RecordErrors,
            error_message => ErrorMessage
        },
        Bin7
    }.

-spec encode_topic_produce_response_8(topic_produce_response_8()) -> iodata().

encode_topic_produce_response_8(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionResponses, fun encode_partition_produce_response_8/1)
    ];
encode_topic_produce_response_8(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_8}
    }).

-spec decode_topic_produce_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_8(),
    Rest :: binary().

decode_topic_produce_response_8(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_8)),
    {
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    }.

-spec encode_produce_response_9(produce_response_9()) -> iodata().

encode_produce_response_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each produce response
        responses := Responses,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_compact_array(Responses, fun encode_topic_produce_response_9/1),
        ?encode_int32(ThrottleTimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_produce_response_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, topic_produce_response_9},
        throttle_time_ms => int32
    }).

-spec decode_produce_response_9(binary()) -> {Decoded, Rest} when
    Decoded :: produce_response_9(),
    Rest :: binary().

decode_produce_response_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_compact_array(Responses, Bin0, Bin1, ?_decode_element(decode_topic_produce_response_9)),
    ?_decode_int32(ThrottleTimeMs, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_produce_response_9_tagged_field/3,
        Header#{
            responses => Responses,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin2
    ).

-spec decode_produce_response_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_produce_response_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_batch_index_and_error_message_9(batch_index_and_error_message_9()) -> iodata().

encode_batch_index_and_error_message_9(
    _Args = #{
        % The batch index of the record that cause the batch to be dropped
        batch_index := BatchIndex,
        % The error message of the record that caused the batch to be dropped
        batch_index_error_message := BatchIndexErrorMessage
    }
) when
    ?is_int32(BatchIndex),
    ?is_nullable_string(BatchIndexErrorMessage)
->
    [
        ?encode_int32(BatchIndex),
        ?encode_compact_nullable_string(BatchIndexErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_batch_index_and_error_message_9(Args) ->
    ?encoder_error(Args, #{
        batch_index => int32,
        batch_index_error_message => nullable_string
    }).

-spec decode_batch_index_and_error_message_9(binary()) -> {Decoded, Rest} when
    Decoded :: batch_index_and_error_message_9(),
    Rest :: binary().

decode_batch_index_and_error_message_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BatchIndex, Bin0, Bin1),
    ?_decode_compact_nullable_string(BatchIndexErrorMessage, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_batch_index_and_error_message_9_tagged_field/3,
        #{
            batch_index => BatchIndex,
            batch_index_error_message => BatchIndexErrorMessage
        },
        Bin2
    ).

-spec decode_batch_index_and_error_message_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_batch_index_and_error_message_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_produce_response_9(partition_produce_response_9()) -> iodata().

encode_partition_produce_response_9(
    _Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The base offset.
        base_offset := BaseOffset,
        % The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, the timestamp will be -1.  If LogAppendTime is used for the topic, the timestamp will be the broker local time when the messages are appended.
        log_append_time_ms := LogAppendTimeMs,
        % The log start offset.
        log_start_offset := LogStartOffset,
        % The batch indices of records that caused the batch to be dropped
        record_errors := RecordErrors,
        % The global error message summarizing the common root cause of the records that caused the batch to be dropped
        error_message := ErrorMessage
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_int64(BaseOffset),
    ?is_int64(LogAppendTimeMs),
    ?is_int64(LogStartOffset),
    ?is_array(RecordErrors),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        ?encode_int64(BaseOffset),
        ?encode_int64(LogAppendTimeMs),
        ?encode_int64(LogStartOffset),
        ?encode_compact_array(RecordErrors, fun encode_batch_index_and_error_message_9/1),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_produce_response_9(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        base_offset => int64,
        log_append_time_ms => int64,
        log_start_offset => int64,
        record_errors => {array, batch_index_and_error_message_9},
        error_message => nullable_string
    }).

-spec decode_partition_produce_response_9(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_response_9(),
    Rest :: binary().

decode_partition_produce_response_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(BaseOffset, Bin2, Bin3),
    ?_decode_int64(LogAppendTimeMs, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_compact_array(RecordErrors, Bin5, Bin6, ?_decode_element(decode_batch_index_and_error_message_9)),
    ?_decode_compact_nullable_string(ErrorMessage, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_partition_produce_response_9_tagged_field/3,
        #{
            index => Index,
            error_code => ErrorCode,
            base_offset => BaseOffset,
            log_append_time_ms => LogAppendTimeMs,
            log_start_offset => LogStartOffset,
            record_errors => RecordErrors,
            error_message => ErrorMessage
        },
        Bin7
    ).

-spec decode_partition_produce_response_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_partition_produce_response_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_produce_response_9(topic_produce_response_9()) -> iodata().

encode_topic_produce_response_9(
    _Args = #{
        % The topic name
        name := Name,
        % Each partition that we produced to within the topic.
        partition_responses := PartitionResponses
    }
) when
    ?is_string(Name),
    ?is_array(PartitionResponses)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionResponses, fun encode_partition_produce_response_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_produce_response_9(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_responses => {array, partition_produce_response_9}
    }).

-spec decode_topic_produce_response_9(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_response_9(),
    Rest :: binary().

decode_topic_produce_response_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionResponses, Bin1, Bin2, ?_decode_element(decode_partition_produce_response_9)),
    ?decode_tagged_fields(
        fun decode_topic_produce_response_9_tagged_field/3,
        #{
            name => Name,
            partition_responses => PartitionResponses
        },
        Bin2
    ).

-spec decode_topic_produce_response_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_topic_produce_response_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type produce_response_0() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_0())
}.
-type partition_produce_response_0() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer()
}.
-type topic_produce_response_0() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_0())
}.
-type produce_response_1() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_1()),
    throttle_time_ms := integer()
}.
-type partition_produce_response_1() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer()
}.
-type topic_produce_response_1() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_1())
}.
-type produce_response_2() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_2()),
    throttle_time_ms := integer()
}.
-type partition_produce_response_2() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer(),
    log_append_time_ms := integer()
}.
-type topic_produce_response_2() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_2())
}.
-type produce_response_3() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_3()),
    throttle_time_ms := integer()
}.
-type partition_produce_response_3() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer(),
    log_append_time_ms := integer()
}.
-type topic_produce_response_3() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_3())
}.
-type produce_response_4() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_4()),
    throttle_time_ms := integer()
}.
-type partition_produce_response_4() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer(),
    log_append_time_ms := integer()
}.
-type topic_produce_response_4() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_4())
}.
-type produce_response_5() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_5()),
    throttle_time_ms := integer()
}.
-type partition_produce_response_5() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer(),
    log_append_time_ms := integer(),
    log_start_offset := integer()
}.
-type topic_produce_response_5() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_5())
}.
-type produce_response_6() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_6()),
    throttle_time_ms := integer()
}.
-type partition_produce_response_6() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer(),
    log_append_time_ms := integer(),
    log_start_offset := integer()
}.
-type topic_produce_response_6() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_6())
}.
-type produce_response_7() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_7()),
    throttle_time_ms := integer()
}.
-type partition_produce_response_7() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer(),
    log_append_time_ms := integer(),
    log_start_offset := integer()
}.
-type topic_produce_response_7() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_7())
}.
-type produce_response_8() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_8()),
    throttle_time_ms := integer()
}.
-type batch_index_and_error_message_8() :: #{
    batch_index := integer(),
    batch_index_error_message := binary() | null
}.
-type partition_produce_response_8() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer(),
    log_append_time_ms := integer(),
    log_start_offset := integer(),
    record_errors := list(batch_index_and_error_message_8()),
    error_message := binary() | null
}.
-type topic_produce_response_8() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_8())
}.
-type produce_response_9() :: #{
    correlation_id => integer(),
    responses := list(topic_produce_response_9()),
    throttle_time_ms := integer()
}.
-type batch_index_and_error_message_9() :: #{
    batch_index := integer(),
    batch_index_error_message := binary() | null
}.
-type partition_produce_response_9() :: #{
    index := integer(),
    error_code := integer(),
    base_offset := integer(),
    log_append_time_ms := integer(),
    log_start_offset := integer(),
    record_errors := list(batch_index_and_error_message_9()),
    error_message := binary() | null
}.
-type topic_produce_response_9() :: #{
    name := binary(),
    partition_responses := list(partition_produce_response_9())
}.
