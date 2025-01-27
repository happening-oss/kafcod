-module(end_txn_request).
-export([
    encode_end_txn_request_0/1,
    decode_end_txn_request_0/1,
    encode_end_txn_request_1/1,
    decode_end_txn_request_1/1,
    encode_end_txn_request_2/1,
    decode_end_txn_request_2/1,
    encode_end_txn_request_3/1,
    decode_end_txn_request_3/1
]).
-export_type([
    end_txn_request_0/0,
    end_txn_request_1/0,
    end_txn_request_2/0,
    end_txn_request_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(END_TXN_REQUEST, 26).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_end_txn_request_0(end_txn_request_0()) -> iodata().

encode_end_txn_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the transaction to end.
        transactional_id := TransactionalId,
        % The producer ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer.
        producer_epoch := ProducerEpoch,
        % True if the transaction was committed, false if it was aborted.
        committed := Committed
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(TransactionalId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_bool(Committed)
->
    [
        ?encode_request_header_1(?END_TXN_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(TransactionalId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_bool(Committed)
    ];
encode_end_txn_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => string,
        producer_id => int64,
        producer_epoch => int16,
        committed => bool
    }).

-spec decode_end_txn_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: end_txn_request_0(),
    Rest :: binary().

decode_end_txn_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(TransactionalId, Bin0, Bin1),
    ?_decode_int64(ProducerId, Bin1, Bin2),
    ?_decode_int16(ProducerEpoch, Bin2, Bin3),
    ?_decode_bool(Committed, Bin3, Bin4),
    {
        Header#{
            transactional_id => TransactionalId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            committed => Committed
        },
        Bin4
    }.

-spec encode_end_txn_request_1(end_txn_request_1()) -> iodata().

encode_end_txn_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the transaction to end.
        transactional_id := TransactionalId,
        % The producer ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer.
        producer_epoch := ProducerEpoch,
        % True if the transaction was committed, false if it was aborted.
        committed := Committed
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(TransactionalId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_bool(Committed)
->
    [
        ?encode_request_header_1(?END_TXN_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(TransactionalId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_bool(Committed)
    ];
encode_end_txn_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => string,
        producer_id => int64,
        producer_epoch => int16,
        committed => bool
    }).

-spec decode_end_txn_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: end_txn_request_1(),
    Rest :: binary().

decode_end_txn_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(TransactionalId, Bin0, Bin1),
    ?_decode_int64(ProducerId, Bin1, Bin2),
    ?_decode_int16(ProducerEpoch, Bin2, Bin3),
    ?_decode_bool(Committed, Bin3, Bin4),
    {
        Header#{
            transactional_id => TransactionalId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            committed => Committed
        },
        Bin4
    }.

-spec encode_end_txn_request_2(end_txn_request_2()) -> iodata().

encode_end_txn_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the transaction to end.
        transactional_id := TransactionalId,
        % The producer ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer.
        producer_epoch := ProducerEpoch,
        % True if the transaction was committed, false if it was aborted.
        committed := Committed
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(TransactionalId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_bool(Committed)
->
    [
        ?encode_request_header_1(?END_TXN_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(TransactionalId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_bool(Committed)
    ];
encode_end_txn_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => string,
        producer_id => int64,
        producer_epoch => int16,
        committed => bool
    }).

-spec decode_end_txn_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: end_txn_request_2(),
    Rest :: binary().

decode_end_txn_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(TransactionalId, Bin0, Bin1),
    ?_decode_int64(ProducerId, Bin1, Bin2),
    ?_decode_int16(ProducerEpoch, Bin2, Bin3),
    ?_decode_bool(Committed, Bin3, Bin4),
    {
        Header#{
            transactional_id => TransactionalId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            committed => Committed
        },
        Bin4
    }.

-spec encode_end_txn_request_3(end_txn_request_3()) -> iodata().

encode_end_txn_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the transaction to end.
        transactional_id := TransactionalId,
        % The producer ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer.
        producer_epoch := ProducerEpoch,
        % True if the transaction was committed, false if it was aborted.
        committed := Committed
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(TransactionalId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_bool(Committed)
->
    [
        ?encode_request_header_2(?END_TXN_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_string(TransactionalId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_bool(Committed),
        ?EMPTY_TAG_BUFFER
    ];
encode_end_txn_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => string,
        producer_id => int64,
        producer_epoch => int16,
        committed => bool
    }).

-spec decode_end_txn_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: end_txn_request_3(),
    Rest :: binary().

decode_end_txn_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(TransactionalId, Bin0, Bin1),
    ?_decode_int64(ProducerId, Bin1, Bin2),
    ?_decode_int16(ProducerEpoch, Bin2, Bin3),
    ?_decode_bool(Committed, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_end_txn_request_3_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            committed => Committed
        },
        Bin4
    ).

-spec decode_end_txn_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_end_txn_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type end_txn_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    committed := boolean()
}.
-type end_txn_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    committed := boolean()
}.
-type end_txn_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    committed := boolean()
}.
-type end_txn_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    committed := boolean()
}.
