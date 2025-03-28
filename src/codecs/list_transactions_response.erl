-module(list_transactions_response).
-export([
    encode_list_transactions_response_0/1,
    decode_list_transactions_response_0/1,
    encode_list_transactions_response_1/1,
    decode_list_transactions_response_1/1
]).
-export_type([
    list_transactions_response_0/0,
    transaction_state_0/0,
    list_transactions_response_1/0,
    transaction_state_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_transactions_response_0(list_transactions_response_0()) -> iodata().

encode_list_transactions_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        error_code := ErrorCode,
        % Set of state filters provided in the request which were unknown to the transaction coordinator
        unknown_state_filters := UnknownStateFilters,
        transaction_states := TransactionStates
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(UnknownStateFilters),
    ?is_array(TransactionStates)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(UnknownStateFilters, ?encode_compact_string_),
        ?encode_compact_array(TransactionStates, fun encode_transaction_state_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_transactions_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        unknown_state_filters => {array, string},
        transaction_states => {array, transaction_state_0}
    }).

-spec decode_list_transactions_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_transactions_response_0(),
    Rest :: binary().

decode_list_transactions_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(UnknownStateFilters, Bin2, Bin3, ?decode_string_),
    ?_decode_compact_array(TransactionStates, Bin3, Bin4, ?_decode_element(decode_transaction_state_0)),
    ?decode_tagged_fields(
        fun decode_list_transactions_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            unknown_state_filters => UnknownStateFilters,
            transaction_states => TransactionStates
        },
        Bin4
    ).

-spec decode_list_transactions_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_transactions_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_transaction_state_0(transaction_state_0()) -> iodata().

encode_transaction_state_0(
    _Args = #{
        transactional_id := TransactionalId,
        producer_id := ProducerId,
        % The current transaction state of the producer
        transaction_state := TransactionState
    }
) when
    ?is_string(TransactionalId),
    ?is_int64(ProducerId),
    ?is_string(TransactionState)
->
    [
        ?encode_compact_string(TransactionalId),
        ?encode_int64(ProducerId),
        ?encode_compact_string(TransactionState),
        ?EMPTY_TAG_BUFFER
    ];
encode_transaction_state_0(Args) ->
    ?encoder_error(Args, #{
        transactional_id => string,
        producer_id => int64,
        transaction_state => string
    }).

-spec decode_transaction_state_0(binary()) -> {Decoded, Rest} when
    Decoded :: transaction_state_0(),
    Rest :: binary().

decode_transaction_state_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TransactionalId, Bin0, Bin1),
    ?_decode_int64(ProducerId, Bin1, Bin2),
    ?_decode_compact_string(TransactionState, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_transaction_state_0_tagged_field/3,
        #{
            transactional_id => TransactionalId,
            producer_id => ProducerId,
            transaction_state => TransactionState
        },
        Bin3
    ).

-spec decode_transaction_state_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_transaction_state_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_transactions_response_1(list_transactions_response_1()) -> iodata().

encode_list_transactions_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        error_code := ErrorCode,
        % Set of state filters provided in the request which were unknown to the transaction coordinator
        unknown_state_filters := UnknownStateFilters,
        transaction_states := TransactionStates
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(UnknownStateFilters),
    ?is_array(TransactionStates)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(UnknownStateFilters, ?encode_compact_string_),
        ?encode_compact_array(TransactionStates, fun encode_transaction_state_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_transactions_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        unknown_state_filters => {array, string},
        transaction_states => {array, transaction_state_1}
    }).

-spec decode_list_transactions_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_transactions_response_1(),
    Rest :: binary().

decode_list_transactions_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(UnknownStateFilters, Bin2, Bin3, ?decode_string_),
    ?_decode_compact_array(TransactionStates, Bin3, Bin4, ?_decode_element(decode_transaction_state_1)),
    ?decode_tagged_fields(
        fun decode_list_transactions_response_1_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            unknown_state_filters => UnknownStateFilters,
            transaction_states => TransactionStates
        },
        Bin4
    ).

-spec decode_list_transactions_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_transactions_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_transaction_state_1(transaction_state_1()) -> iodata().

encode_transaction_state_1(
    _Args = #{
        transactional_id := TransactionalId,
        producer_id := ProducerId,
        % The current transaction state of the producer
        transaction_state := TransactionState
    }
) when
    ?is_string(TransactionalId),
    ?is_int64(ProducerId),
    ?is_string(TransactionState)
->
    [
        ?encode_compact_string(TransactionalId),
        ?encode_int64(ProducerId),
        ?encode_compact_string(TransactionState),
        ?EMPTY_TAG_BUFFER
    ];
encode_transaction_state_1(Args) ->
    ?encoder_error(Args, #{
        transactional_id => string,
        producer_id => int64,
        transaction_state => string
    }).

-spec decode_transaction_state_1(binary()) -> {Decoded, Rest} when
    Decoded :: transaction_state_1(),
    Rest :: binary().

decode_transaction_state_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TransactionalId, Bin0, Bin1),
    ?_decode_int64(ProducerId, Bin1, Bin2),
    ?_decode_compact_string(TransactionState, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_transaction_state_1_tagged_field/3,
        #{
            transactional_id => TransactionalId,
            producer_id => ProducerId,
            transaction_state => TransactionState
        },
        Bin3
    ).

-spec decode_transaction_state_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_transaction_state_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_transactions_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    unknown_state_filters := list(binary()),
    transaction_states := list(transaction_state_0())
}.
-type transaction_state_0() :: #{
    transactional_id := binary(),
    producer_id := integer(),
    transaction_state := binary()
}.
-type list_transactions_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    unknown_state_filters := list(binary()),
    transaction_states := list(transaction_state_1())
}.
-type transaction_state_1() :: #{
    transactional_id := binary(),
    producer_id := integer(),
    transaction_state := binary()
}.
