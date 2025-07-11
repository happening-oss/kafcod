-module(describe_producers_response).
-export([
    encode_describe_producers_response_0/1,
    decode_describe_producers_response_0/1
]).
-export_type([
    describe_producers_response_0/0,
    producer_state_0/0,
    partition_response_0/0,
    topic_response_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_producers_response_0(describe_producers_response_0()) -> iodata().

encode_describe_producers_response_0(
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
        ?encode_compact_array(Topics, fun encode_topic_response_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_producers_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, topic_response_0}
    }).

-spec decode_describe_producers_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_producers_response_0(),
    Rest :: binary().

decode_describe_producers_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_topic_response_0)),
    ?decode_tagged_fields(
        fun decode_describe_producers_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_describe_producers_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_producers_response_0().

decode_describe_producers_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_producer_state_0(producer_state_0()) -> iodata().

encode_producer_state_0(
    _Args = #{
        producer_id := ProducerId,
        producer_epoch := ProducerEpoch,
        last_sequence := LastSequence,
        last_timestamp := LastTimestamp,
        coordinator_epoch := CoordinatorEpoch,
        current_txn_start_offset := CurrentTxnStartOffset
    }
) when
    ?is_int64(ProducerId),
    ?is_int32(ProducerEpoch),
    ?is_int32(LastSequence),
    ?is_int64(LastTimestamp),
    ?is_int32(CoordinatorEpoch),
    ?is_int64(CurrentTxnStartOffset)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int32(ProducerEpoch),
        ?encode_int32(LastSequence),
        ?encode_int64(LastTimestamp),
        ?encode_int32(CoordinatorEpoch),
        ?encode_int64(CurrentTxnStartOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_producer_state_0(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        producer_epoch => int32,
        last_sequence => int32,
        last_timestamp => int64,
        coordinator_epoch => int32,
        current_txn_start_offset => int64
    }).

-spec decode_producer_state_0(binary()) -> {Decoded, Rest} when
    Decoded :: producer_state_0(),
    Rest :: binary().

decode_producer_state_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int32(ProducerEpoch, Bin1, Bin2),
    ?_decode_int32(LastSequence, Bin2, Bin3),
    ?_decode_int64(LastTimestamp, Bin3, Bin4),
    ?_decode_int32(CoordinatorEpoch, Bin4, Bin5),
    ?_decode_int64(CurrentTxnStartOffset, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_producer_state_0_tagged_field/3,
        #{
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            last_sequence => LastSequence,
            last_timestamp => LastTimestamp,
            coordinator_epoch => CoordinatorEpoch,
            current_txn_start_offset => CurrentTxnStartOffset
        },
        Bin6
    ).

-spec decode_producer_state_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: producer_state_0().

decode_producer_state_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_response_0(partition_response_0()) -> iodata().

encode_partition_response_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition error message, which may be null if no additional details are available
        error_message := ErrorMessage,
        active_producers := ActiveProducers
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(ActiveProducers)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(ActiveProducers, fun encode_producer_state_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_response_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        error_message => nullable_string,
        active_producers => {array, producer_state_0}
    }).

-spec decode_partition_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_response_0(),
    Rest :: binary().

decode_partition_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(ActiveProducers, Bin3, Bin4, ?_decode_element(decode_producer_state_0)),
    ?decode_tagged_fields(
        fun decode_partition_response_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            active_producers => ActiveProducers
        },
        Bin4
    ).

-spec decode_partition_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_response_0().

decode_partition_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_response_0(topic_response_0()) -> iodata().

encode_topic_response_0(
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
        ?encode_compact_array(Partitions, fun encode_partition_response_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_response_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, partition_response_0}
    }).

-spec decode_topic_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_response_0(),
    Rest :: binary().

decode_topic_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_response_0)),
    ?decode_tagged_fields(
        fun decode_topic_response_0_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_response_0().

decode_topic_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_producers_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(topic_response_0())
}.
-type producer_state_0() :: #{
    producer_id := integer(),
    producer_epoch := integer(),
    last_sequence := integer(),
    last_timestamp := integer(),
    coordinator_epoch := integer(),
    current_txn_start_offset := integer()
}.
-type partition_response_0() :: #{
    partition_index := integer(),
    error_code := integer(),
    error_message := binary() | null,
    active_producers := list(producer_state_0())
}.
-type topic_response_0() :: #{
    name := binary(),
    partitions := list(partition_response_0())
}.
