-module(offset_for_leader_epoch_response).
-export([
    encode_offset_for_leader_epoch_response_0/1,
    decode_offset_for_leader_epoch_response_0/1,
    encode_offset_for_leader_epoch_response_1/1,
    decode_offset_for_leader_epoch_response_1/1,
    encode_offset_for_leader_epoch_response_2/1,
    decode_offset_for_leader_epoch_response_2/1,
    encode_offset_for_leader_epoch_response_3/1,
    decode_offset_for_leader_epoch_response_3/1,
    encode_offset_for_leader_epoch_response_4/1,
    decode_offset_for_leader_epoch_response_4/1
]).
-export_type([
    offset_for_leader_epoch_response_0/0,
    epoch_end_offset_0/0,
    offset_for_leader_topic_result_0/0,
    offset_for_leader_epoch_response_1/0,
    epoch_end_offset_1/0,
    offset_for_leader_topic_result_1/0,
    offset_for_leader_epoch_response_2/0,
    epoch_end_offset_2/0,
    offset_for_leader_topic_result_2/0,
    offset_for_leader_epoch_response_3/0,
    epoch_end_offset_3/0,
    offset_for_leader_topic_result_3/0,
    offset_for_leader_epoch_response_4/0,
    epoch_end_offset_4/0,
    offset_for_leader_topic_result_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_offset_for_leader_epoch_response_0(offset_for_leader_epoch_response_0()) -> iodata().

encode_offset_for_leader_epoch_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each topic we fetched offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_offset_for_leader_topic_result_0/1)
    ];
encode_offset_for_leader_epoch_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, offset_for_leader_topic_result_0}
    }).

-spec decode_offset_for_leader_epoch_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_response_0(),
    Rest :: binary().

decode_offset_for_leader_epoch_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_offset_for_leader_topic_result_0)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_epoch_end_offset_0(epoch_end_offset_0()) -> iodata().

encode_epoch_end_offset_0(
    _Args = #{
        % The error code 0, or if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition := Partition,
        % The end offset of the epoch.
        end_offset := EndOffset
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(Partition),
    ?is_int64(EndOffset)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(Partition),
        ?encode_int64(EndOffset)
    ];
encode_epoch_end_offset_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_0(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_0(),
    Rest :: binary().

decode_epoch_end_offset_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(Partition, Bin1, Bin2),
    ?_decode_int64(EndOffset, Bin2, Bin3),
    {
        #{
            error_code => ErrorCode,
            partition => Partition,
            end_offset => EndOffset
        },
        Bin3
    }.

-spec encode_offset_for_leader_topic_result_0(offset_for_leader_topic_result_0()) -> iodata().

encode_offset_for_leader_topic_result_0(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition in the topic we fetched offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_epoch_end_offset_0/1)
    ];
encode_offset_for_leader_topic_result_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, epoch_end_offset_0}
    }).

-spec decode_offset_for_leader_topic_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_result_0(),
    Rest :: binary().

decode_offset_for_leader_topic_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_epoch_end_offset_0)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_for_leader_epoch_response_1(offset_for_leader_epoch_response_1()) -> iodata().

encode_offset_for_leader_epoch_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each topic we fetched offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_offset_for_leader_topic_result_1/1)
    ];
encode_offset_for_leader_epoch_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, offset_for_leader_topic_result_1}
    }).

-spec decode_offset_for_leader_epoch_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_response_1(),
    Rest :: binary().

decode_offset_for_leader_epoch_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_offset_for_leader_topic_result_1)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_epoch_end_offset_1(epoch_end_offset_1()) -> iodata().

encode_epoch_end_offset_1(
    _Args = #{
        % The error code 0, or if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition := Partition,
        % The leader epoch of the partition.
        leader_epoch := LeaderEpoch,
        % The end offset of the epoch.
        end_offset := EndOffset
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(Partition),
    ?is_int32(LeaderEpoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(Partition),
        ?encode_int32(LeaderEpoch),
        ?encode_int64(EndOffset)
    ];
encode_epoch_end_offset_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition => int32,
        leader_epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_1(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_1(),
    Rest :: binary().

decode_epoch_end_offset_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(Partition, Bin1, Bin2),
    ?_decode_int32(LeaderEpoch, Bin2, Bin3),
    ?_decode_int64(EndOffset, Bin3, Bin4),
    {
        #{
            error_code => ErrorCode,
            partition => Partition,
            leader_epoch => LeaderEpoch,
            end_offset => EndOffset
        },
        Bin4
    }.

-spec encode_offset_for_leader_topic_result_1(offset_for_leader_topic_result_1()) -> iodata().

encode_offset_for_leader_topic_result_1(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition in the topic we fetched offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_epoch_end_offset_1/1)
    ];
encode_offset_for_leader_topic_result_1(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, epoch_end_offset_1}
    }).

-spec decode_offset_for_leader_topic_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_result_1(),
    Rest :: binary().

decode_offset_for_leader_topic_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_epoch_end_offset_1)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_for_leader_epoch_response_2(offset_for_leader_epoch_response_2()) -> iodata().

encode_offset_for_leader_epoch_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic we fetched offsets for.
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
        ?encode_array(Topics, fun encode_offset_for_leader_topic_result_2/1)
    ];
encode_offset_for_leader_epoch_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_for_leader_topic_result_2}
    }).

-spec decode_offset_for_leader_epoch_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_response_2(),
    Rest :: binary().

decode_offset_for_leader_epoch_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_topic_result_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_epoch_end_offset_2(epoch_end_offset_2()) -> iodata().

encode_epoch_end_offset_2(
    _Args = #{
        % The error code 0, or if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition := Partition,
        % The leader epoch of the partition.
        leader_epoch := LeaderEpoch,
        % The end offset of the epoch.
        end_offset := EndOffset
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(Partition),
    ?is_int32(LeaderEpoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(Partition),
        ?encode_int32(LeaderEpoch),
        ?encode_int64(EndOffset)
    ];
encode_epoch_end_offset_2(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition => int32,
        leader_epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_2(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_2(),
    Rest :: binary().

decode_epoch_end_offset_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(Partition, Bin1, Bin2),
    ?_decode_int32(LeaderEpoch, Bin2, Bin3),
    ?_decode_int64(EndOffset, Bin3, Bin4),
    {
        #{
            error_code => ErrorCode,
            partition => Partition,
            leader_epoch => LeaderEpoch,
            end_offset => EndOffset
        },
        Bin4
    }.

-spec encode_offset_for_leader_topic_result_2(offset_for_leader_topic_result_2()) -> iodata().

encode_offset_for_leader_topic_result_2(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition in the topic we fetched offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_epoch_end_offset_2/1)
    ];
encode_offset_for_leader_topic_result_2(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, epoch_end_offset_2}
    }).

-spec decode_offset_for_leader_topic_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_result_2(),
    Rest :: binary().

decode_offset_for_leader_topic_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_epoch_end_offset_2)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_for_leader_epoch_response_3(offset_for_leader_epoch_response_3()) -> iodata().

encode_offset_for_leader_epoch_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic we fetched offsets for.
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
        ?encode_array(Topics, fun encode_offset_for_leader_topic_result_3/1)
    ];
encode_offset_for_leader_epoch_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_for_leader_topic_result_3}
    }).

-spec decode_offset_for_leader_epoch_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_response_3(),
    Rest :: binary().

decode_offset_for_leader_epoch_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_topic_result_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_epoch_end_offset_3(epoch_end_offset_3()) -> iodata().

encode_epoch_end_offset_3(
    _Args = #{
        % The error code 0, or if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition := Partition,
        % The leader epoch of the partition.
        leader_epoch := LeaderEpoch,
        % The end offset of the epoch.
        end_offset := EndOffset
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(Partition),
    ?is_int32(LeaderEpoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(Partition),
        ?encode_int32(LeaderEpoch),
        ?encode_int64(EndOffset)
    ];
encode_epoch_end_offset_3(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition => int32,
        leader_epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_3(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_3(),
    Rest :: binary().

decode_epoch_end_offset_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(Partition, Bin1, Bin2),
    ?_decode_int32(LeaderEpoch, Bin2, Bin3),
    ?_decode_int64(EndOffset, Bin3, Bin4),
    {
        #{
            error_code => ErrorCode,
            partition => Partition,
            leader_epoch => LeaderEpoch,
            end_offset => EndOffset
        },
        Bin4
    }.

-spec encode_offset_for_leader_topic_result_3(offset_for_leader_topic_result_3()) -> iodata().

encode_offset_for_leader_topic_result_3(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition in the topic we fetched offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_epoch_end_offset_3/1)
    ];
encode_offset_for_leader_topic_result_3(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, epoch_end_offset_3}
    }).

-spec decode_offset_for_leader_topic_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_result_3(),
    Rest :: binary().

decode_offset_for_leader_topic_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_epoch_end_offset_3)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_for_leader_epoch_response_4(offset_for_leader_epoch_response_4()) -> iodata().

encode_offset_for_leader_epoch_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic we fetched offsets for.
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
        ?encode_compact_array(Topics, fun encode_offset_for_leader_topic_result_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_for_leader_epoch_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, offset_for_leader_topic_result_4}
    }).

-spec decode_offset_for_leader_epoch_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_response_4(),
    Rest :: binary().

decode_offset_for_leader_epoch_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_topic_result_4)),
    ?decode_tagged_fields(
        fun decode_offset_for_leader_epoch_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_offset_for_leader_epoch_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_offset_for_leader_epoch_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_epoch_end_offset_4(epoch_end_offset_4()) -> iodata().

encode_epoch_end_offset_4(
    _Args = #{
        % The error code 0, or if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition := Partition,
        % The leader epoch of the partition.
        leader_epoch := LeaderEpoch,
        % The end offset of the epoch.
        end_offset := EndOffset
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(Partition),
    ?is_int32(LeaderEpoch),
    ?is_int64(EndOffset)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(Partition),
        ?encode_int32(LeaderEpoch),
        ?encode_int64(EndOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_epoch_end_offset_4(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition => int32,
        leader_epoch => int32,
        end_offset => int64
    }).

-spec decode_epoch_end_offset_4(binary()) -> {Decoded, Rest} when
    Decoded :: epoch_end_offset_4(),
    Rest :: binary().

decode_epoch_end_offset_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(Partition, Bin1, Bin2),
    ?_decode_int32(LeaderEpoch, Bin2, Bin3),
    ?_decode_int64(EndOffset, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_epoch_end_offset_4_tagged_field/3,
        #{
            error_code => ErrorCode,
            partition => Partition,
            leader_epoch => LeaderEpoch,
            end_offset => EndOffset
        },
        Bin4
    ).

-spec decode_epoch_end_offset_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_epoch_end_offset_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_for_leader_topic_result_4(offset_for_leader_topic_result_4()) -> iodata().

encode_offset_for_leader_topic_result_4(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition in the topic we fetched offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, fun encode_epoch_end_offset_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_for_leader_topic_result_4(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, epoch_end_offset_4}
    }).

-spec decode_offset_for_leader_topic_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_result_4(),
    Rest :: binary().

decode_offset_for_leader_topic_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_epoch_end_offset_4)),
    ?decode_tagged_fields(
        fun decode_offset_for_leader_topic_result_4_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_offset_for_leader_topic_result_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_offset_for_leader_topic_result_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type offset_for_leader_epoch_response_0() :: #{
    correlation_id => integer(),
    topics := list(offset_for_leader_topic_result_0())
}.
-type epoch_end_offset_0() :: #{
    error_code := integer(),
    partition := integer(),
    end_offset := integer()
}.
-type offset_for_leader_topic_result_0() :: #{
    topic := binary(),
    partitions := list(epoch_end_offset_0())
}.
-type offset_for_leader_epoch_response_1() :: #{
    correlation_id => integer(),
    topics := list(offset_for_leader_topic_result_1())
}.
-type epoch_end_offset_1() :: #{
    error_code := integer(),
    partition := integer(),
    leader_epoch := integer(),
    end_offset := integer()
}.
-type offset_for_leader_topic_result_1() :: #{
    topic := binary(),
    partitions := list(epoch_end_offset_1())
}.
-type offset_for_leader_epoch_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_for_leader_topic_result_2())
}.
-type epoch_end_offset_2() :: #{
    error_code := integer(),
    partition := integer(),
    leader_epoch := integer(),
    end_offset := integer()
}.
-type offset_for_leader_topic_result_2() :: #{
    topic := binary(),
    partitions := list(epoch_end_offset_2())
}.
-type offset_for_leader_epoch_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_for_leader_topic_result_3())
}.
-type epoch_end_offset_3() :: #{
    error_code := integer(),
    partition := integer(),
    leader_epoch := integer(),
    end_offset := integer()
}.
-type offset_for_leader_topic_result_3() :: #{
    topic := binary(),
    partitions := list(epoch_end_offset_3())
}.
-type offset_for_leader_epoch_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(offset_for_leader_topic_result_4())
}.
-type epoch_end_offset_4() :: #{
    error_code := integer(),
    partition := integer(),
    leader_epoch := integer(),
    end_offset := integer()
}.
-type offset_for_leader_topic_result_4() :: #{
    topic := binary(),
    partitions := list(epoch_end_offset_4())
}.
