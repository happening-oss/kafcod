-module(describe_transactions_response).
-export([
    encode_describe_transactions_response_0/1,
    decode_describe_transactions_response_0/1
]).
-export_type([
    describe_transactions_response_0/0,
    topic_data_0/0,
    transaction_state_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_transactions_response_0(describe_transactions_response_0()) -> iodata().

encode_describe_transactions_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        transaction_states := TransactionStates
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(TransactionStates)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(TransactionStates, fun encode_transaction_state_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_transactions_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        transaction_states => {array, transaction_state_0}
    }).

-spec decode_describe_transactions_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_transactions_response_0(),
    Rest :: binary().

decode_describe_transactions_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(TransactionStates, Bin1, Bin2, ?_decode_element(decode_transaction_state_0)),
    ?decode_tagged_fields(
        fun decode_describe_transactions_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            transaction_states => TransactionStates
        },
        Bin2
    ).

-spec decode_describe_transactions_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_transactions_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_0(topic_data_0()) -> iodata().

encode_topic_data_0(
    _Args = #{
        topic := Topic,
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_data_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_topic_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_data_0(),
    Rest :: binary().

decode_topic_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_topic_data_0_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_data_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_topic_data_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_transaction_state_0(transaction_state_0()) -> iodata().

encode_transaction_state_0(
    _Args = #{
        error_code := ErrorCode,
        transactional_id := TransactionalId,
        transaction_state := TransactionState,
        transaction_timeout_ms := TransactionTimeoutMs,
        transaction_start_time_ms := TransactionStartTimeMs,
        producer_id := ProducerId,
        producer_epoch := ProducerEpoch,
        % The set of partitions included in the current transaction (if active). When a transaction is preparing to commit or abort, this will include only partitions which do not have markers.
        topics := Topics
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(TransactionalId),
    ?is_string(TransactionState),
    ?is_int32(TransactionTimeoutMs),
    ?is_int64(TransactionStartTimeMs),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_array(Topics)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_string(TransactionalId),
        ?encode_compact_string(TransactionState),
        ?encode_int32(TransactionTimeoutMs),
        ?encode_int64(TransactionStartTimeMs),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_compact_array(Topics, fun encode_topic_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_transaction_state_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        transactional_id => string,
        transaction_state => string,
        transaction_timeout_ms => int32,
        transaction_start_time_ms => int64,
        producer_id => int64,
        producer_epoch => int16,
        topics => {array, topic_data_0}
    }).

-spec decode_transaction_state_0(binary()) -> {Decoded, Rest} when
    Decoded :: transaction_state_0(),
    Rest :: binary().

decode_transaction_state_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(TransactionalId, Bin1, Bin2),
    ?_decode_compact_string(TransactionState, Bin2, Bin3),
    ?_decode_int32(TransactionTimeoutMs, Bin3, Bin4),
    ?_decode_int64(TransactionStartTimeMs, Bin4, Bin5),
    ?_decode_int64(ProducerId, Bin5, Bin6),
    ?_decode_int16(ProducerEpoch, Bin6, Bin7),
    ?_decode_compact_array(Topics, Bin7, Bin8, ?_decode_element(decode_topic_data_0)),
    ?decode_tagged_fields(
        fun decode_transaction_state_0_tagged_field/3,
        #{
            error_code => ErrorCode,
            transactional_id => TransactionalId,
            transaction_state => TransactionState,
            transaction_timeout_ms => TransactionTimeoutMs,
            transaction_start_time_ms => TransactionStartTimeMs,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            topics => Topics
        },
        Bin8
    ).

-spec decode_transaction_state_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_transaction_state_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_transactions_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    transaction_states := list(transaction_state_0())
}.
-type topic_data_0() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type transaction_state_0() :: #{
    error_code := integer(),
    transactional_id := binary(),
    transaction_state := binary(),
    transaction_timeout_ms := integer(),
    transaction_start_time_ms := integer(),
    producer_id := integer(),
    producer_epoch := integer(),
    topics := list(topic_data_0())
}.
