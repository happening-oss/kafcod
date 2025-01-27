-module(delete_records_response).
-export([
    encode_delete_records_response_0/1,
    decode_delete_records_response_0/1,
    encode_delete_records_response_1/1,
    decode_delete_records_response_1/1,
    encode_delete_records_response_2/1,
    decode_delete_records_response_2/1
]).
-export_type([
    delete_records_response_0/0,
    delete_records_partition_result_0/0,
    delete_records_topic_result_0/0,
    delete_records_response_1/0,
    delete_records_partition_result_1/0,
    delete_records_topic_result_1/0,
    delete_records_response_2/0,
    delete_records_partition_result_2/0,
    delete_records_topic_result_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_delete_records_response_0(delete_records_response_0()) -> iodata().

encode_delete_records_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic that we wanted to delete records from.
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
        ?encode_array(Topics, fun encode_delete_records_topic_result_0/1)
    ];
encode_delete_records_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, delete_records_topic_result_0}
    }).

-spec decode_delete_records_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_response_0(),
    Rest :: binary().

decode_delete_records_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_delete_records_topic_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_delete_records_partition_result_0(delete_records_partition_result_0()) -> iodata().

encode_delete_records_partition_result_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition low water mark.
        low_watermark := LowWatermark,
        % The deletion error code, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(LowWatermark),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(LowWatermark),
        ?encode_int16(ErrorCode)
    ];
encode_delete_records_partition_result_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        low_watermark => int64,
        error_code => int16
    }).

-spec decode_delete_records_partition_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_partition_result_0(),
    Rest :: binary().

decode_delete_records_partition_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(LowWatermark, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            low_watermark => LowWatermark,
            error_code => ErrorCode
        },
        Bin3
    }.

-spec encode_delete_records_topic_result_0(delete_records_topic_result_0()) -> iodata().

encode_delete_records_topic_result_0(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition that we wanted to delete records from.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_delete_records_partition_result_0/1)
    ];
encode_delete_records_topic_result_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, delete_records_partition_result_0}
    }).

-spec decode_delete_records_topic_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_topic_result_0(),
    Rest :: binary().

decode_delete_records_topic_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_delete_records_partition_result_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_delete_records_response_1(delete_records_response_1()) -> iodata().

encode_delete_records_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic that we wanted to delete records from.
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
        ?encode_array(Topics, fun encode_delete_records_topic_result_1/1)
    ];
encode_delete_records_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, delete_records_topic_result_1}
    }).

-spec decode_delete_records_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_response_1(),
    Rest :: binary().

decode_delete_records_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_delete_records_topic_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_delete_records_partition_result_1(delete_records_partition_result_1()) -> iodata().

encode_delete_records_partition_result_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition low water mark.
        low_watermark := LowWatermark,
        % The deletion error code, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(LowWatermark),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(LowWatermark),
        ?encode_int16(ErrorCode)
    ];
encode_delete_records_partition_result_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        low_watermark => int64,
        error_code => int16
    }).

-spec decode_delete_records_partition_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_partition_result_1(),
    Rest :: binary().

decode_delete_records_partition_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(LowWatermark, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            low_watermark => LowWatermark,
            error_code => ErrorCode
        },
        Bin3
    }.

-spec encode_delete_records_topic_result_1(delete_records_topic_result_1()) -> iodata().

encode_delete_records_topic_result_1(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition that we wanted to delete records from.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_delete_records_partition_result_1/1)
    ];
encode_delete_records_topic_result_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, delete_records_partition_result_1}
    }).

-spec decode_delete_records_topic_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_topic_result_1(),
    Rest :: binary().

decode_delete_records_topic_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_delete_records_partition_result_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_delete_records_response_2(delete_records_response_2()) -> iodata().

encode_delete_records_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic that we wanted to delete records from.
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
        ?encode_compact_array(Topics, fun encode_delete_records_topic_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_records_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, delete_records_topic_result_2}
    }).

-spec decode_delete_records_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_response_2(),
    Rest :: binary().

decode_delete_records_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_delete_records_topic_result_2)),
    ?decode_tagged_fields(
        fun decode_delete_records_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_delete_records_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_records_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_records_partition_result_2(delete_records_partition_result_2()) -> iodata().

encode_delete_records_partition_result_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition low water mark.
        low_watermark := LowWatermark,
        % The deletion error code, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(LowWatermark),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(LowWatermark),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_records_partition_result_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        low_watermark => int64,
        error_code => int16
    }).

-spec decode_delete_records_partition_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_partition_result_2(),
    Rest :: binary().

decode_delete_records_partition_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(LowWatermark, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_delete_records_partition_result_2_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            low_watermark => LowWatermark,
            error_code => ErrorCode
        },
        Bin3
    ).

-spec decode_delete_records_partition_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_records_partition_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_records_topic_result_2(delete_records_topic_result_2()) -> iodata().

encode_delete_records_topic_result_2(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition that we wanted to delete records from.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_delete_records_partition_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_records_topic_result_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, delete_records_partition_result_2}
    }).

-spec decode_delete_records_topic_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_topic_result_2(),
    Rest :: binary().

decode_delete_records_topic_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_delete_records_partition_result_2)),
    ?decode_tagged_fields(
        fun decode_delete_records_topic_result_2_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_delete_records_topic_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_records_topic_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type delete_records_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(delete_records_topic_result_0())
}.
-type delete_records_partition_result_0() :: #{
    partition_index := integer(),
    low_watermark := integer(),
    error_code := integer()
}.
-type delete_records_topic_result_0() :: #{
    name := binary(),
    partitions := list(delete_records_partition_result_0())
}.
-type delete_records_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(delete_records_topic_result_1())
}.
-type delete_records_partition_result_1() :: #{
    partition_index := integer(),
    low_watermark := integer(),
    error_code := integer()
}.
-type delete_records_topic_result_1() :: #{
    name := binary(),
    partitions := list(delete_records_partition_result_1())
}.
-type delete_records_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(delete_records_topic_result_2())
}.
-type delete_records_partition_result_2() :: #{
    partition_index := integer(),
    low_watermark := integer(),
    error_code := integer()
}.
-type delete_records_topic_result_2() :: #{
    name := binary(),
    partitions := list(delete_records_partition_result_2())
}.
