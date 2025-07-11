-module(controlled_shutdown_response).
-export([
    encode_controlled_shutdown_response_0/1,
    decode_controlled_shutdown_response_0/1,
    encode_controlled_shutdown_response_1/1,
    decode_controlled_shutdown_response_1/1,
    encode_controlled_shutdown_response_2/1,
    decode_controlled_shutdown_response_2/1,
    encode_controlled_shutdown_response_3/1,
    decode_controlled_shutdown_response_3/1
]).
-export_type([
    controlled_shutdown_response_0/0,
    remaining_partition_0/0,
    controlled_shutdown_response_1/0,
    remaining_partition_1/0,
    controlled_shutdown_response_2/0,
    remaining_partition_2/0,
    controlled_shutdown_response_3/0,
    remaining_partition_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_controlled_shutdown_response_0(controlled_shutdown_response_0()) -> iodata().

encode_controlled_shutdown_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code.
        error_code := ErrorCode,
        % The partitions that the broker still leads.
        remaining_partitions := RemainingPartitions
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(RemainingPartitions)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(RemainingPartitions, fun encode_remaining_partition_0/1)
    ];
encode_controlled_shutdown_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        remaining_partitions => {array, remaining_partition_0}
    }).

-spec decode_controlled_shutdown_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: controlled_shutdown_response_0(),
    Rest :: binary().

decode_controlled_shutdown_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(RemainingPartitions, Bin1, Bin2, ?_decode_element(decode_remaining_partition_0)),
    {
        Header#{
            error_code => ErrorCode,
            remaining_partitions => RemainingPartitions
        },
        Bin2
    }.

-spec encode_remaining_partition_0(remaining_partition_0()) -> iodata().

encode_remaining_partition_0(
    _Args = #{
        % The name of the topic.
        topic_name := TopicName,
        % The index of the partition.
        partition_index := PartitionIndex
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex)
    ];
encode_remaining_partition_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32
    }).

-spec decode_remaining_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: remaining_partition_0(),
    Rest :: binary().

decode_remaining_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex
        },
        Bin2
    }.

-spec encode_controlled_shutdown_response_1(controlled_shutdown_response_1()) -> iodata().

encode_controlled_shutdown_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code.
        error_code := ErrorCode,
        % The partitions that the broker still leads.
        remaining_partitions := RemainingPartitions
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(RemainingPartitions)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(RemainingPartitions, fun encode_remaining_partition_1/1)
    ];
encode_controlled_shutdown_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        remaining_partitions => {array, remaining_partition_1}
    }).

-spec decode_controlled_shutdown_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: controlled_shutdown_response_1(),
    Rest :: binary().

decode_controlled_shutdown_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(RemainingPartitions, Bin1, Bin2, ?_decode_element(decode_remaining_partition_1)),
    {
        Header#{
            error_code => ErrorCode,
            remaining_partitions => RemainingPartitions
        },
        Bin2
    }.

-spec encode_remaining_partition_1(remaining_partition_1()) -> iodata().

encode_remaining_partition_1(
    _Args = #{
        % The name of the topic.
        topic_name := TopicName,
        % The index of the partition.
        partition_index := PartitionIndex
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex)
    ];
encode_remaining_partition_1(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32
    }).

-spec decode_remaining_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: remaining_partition_1(),
    Rest :: binary().

decode_remaining_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex
        },
        Bin2
    }.

-spec encode_controlled_shutdown_response_2(controlled_shutdown_response_2()) -> iodata().

encode_controlled_shutdown_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code.
        error_code := ErrorCode,
        % The partitions that the broker still leads.
        remaining_partitions := RemainingPartitions
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(RemainingPartitions)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(RemainingPartitions, fun encode_remaining_partition_2/1)
    ];
encode_controlled_shutdown_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        remaining_partitions => {array, remaining_partition_2}
    }).

-spec decode_controlled_shutdown_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: controlled_shutdown_response_2(),
    Rest :: binary().

decode_controlled_shutdown_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(RemainingPartitions, Bin1, Bin2, ?_decode_element(decode_remaining_partition_2)),
    {
        Header#{
            error_code => ErrorCode,
            remaining_partitions => RemainingPartitions
        },
        Bin2
    }.

-spec encode_remaining_partition_2(remaining_partition_2()) -> iodata().

encode_remaining_partition_2(
    _Args = #{
        % The name of the topic.
        topic_name := TopicName,
        % The index of the partition.
        partition_index := PartitionIndex
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex)
    ];
encode_remaining_partition_2(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32
    }).

-spec decode_remaining_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: remaining_partition_2(),
    Rest :: binary().

decode_remaining_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex
        },
        Bin2
    }.

-spec encode_controlled_shutdown_response_3(controlled_shutdown_response_3()) -> iodata().

encode_controlled_shutdown_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code.
        error_code := ErrorCode,
        % The partitions that the broker still leads.
        remaining_partitions := RemainingPartitions
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(RemainingPartitions)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(RemainingPartitions, fun encode_remaining_partition_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_controlled_shutdown_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        remaining_partitions => {array, remaining_partition_3}
    }).

-spec decode_controlled_shutdown_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: controlled_shutdown_response_3(),
    Rest :: binary().

decode_controlled_shutdown_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(RemainingPartitions, Bin1, Bin2, ?_decode_element(decode_remaining_partition_3)),
    ?decode_tagged_fields(
        fun decode_controlled_shutdown_response_3_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            remaining_partitions => RemainingPartitions
        },
        Bin2
    ).

-spec decode_controlled_shutdown_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: controlled_shutdown_response_3().

decode_controlled_shutdown_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_remaining_partition_3(remaining_partition_3()) -> iodata().

encode_remaining_partition_3(
    _Args = #{
        % The name of the topic.
        topic_name := TopicName,
        % The index of the partition.
        partition_index := PartitionIndex
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?EMPTY_TAG_BUFFER
    ];
encode_remaining_partition_3(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32
    }).

-spec decode_remaining_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: remaining_partition_3(),
    Rest :: binary().

decode_remaining_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_remaining_partition_3_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex
        },
        Bin2
    ).

-spec decode_remaining_partition_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: remaining_partition_3().

decode_remaining_partition_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type controlled_shutdown_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    remaining_partitions := list(remaining_partition_0())
}.
-type remaining_partition_0() :: #{
    topic_name := binary(),
    partition_index := integer()
}.
-type controlled_shutdown_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    remaining_partitions := list(remaining_partition_1())
}.
-type remaining_partition_1() :: #{
    topic_name := binary(),
    partition_index := integer()
}.
-type controlled_shutdown_response_2() :: #{
    correlation_id => integer(),
    error_code := integer(),
    remaining_partitions := list(remaining_partition_2())
}.
-type remaining_partition_2() :: #{
    topic_name := binary(),
    partition_index := integer()
}.
-type controlled_shutdown_response_3() :: #{
    correlation_id => integer(),
    error_code := integer(),
    remaining_partitions := list(remaining_partition_3())
}.
-type remaining_partition_3() :: #{
    topic_name := binary(),
    partition_index := integer()
}.
