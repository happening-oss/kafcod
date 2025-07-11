-module(init_producer_id_request).
-export([
    encode_init_producer_id_request_0/1,
    decode_init_producer_id_request_0/1,
    encode_init_producer_id_request_1/1,
    decode_init_producer_id_request_1/1,
    encode_init_producer_id_request_2/1,
    decode_init_producer_id_request_2/1,
    encode_init_producer_id_request_3/1,
    decode_init_producer_id_request_3/1,
    encode_init_producer_id_request_4/1,
    decode_init_producer_id_request_4/1,
    encode_init_producer_id_request_5/1,
    decode_init_producer_id_request_5/1
]).
-export_type([
    init_producer_id_request_0/0,
    init_producer_id_request_1/0,
    init_producer_id_request_2/0,
    init_producer_id_request_3/0,
    init_producer_id_request_4/0,
    init_producer_id_request_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(INIT_PRODUCER_ID_REQUEST, 22).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_init_producer_id_request_0(init_producer_id_request_0()) -> iodata().

encode_init_producer_id_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The time in ms to wait before aborting idle transactions sent by this producer. This is only relevant if a TransactionalId has been defined.
        transaction_timeout_ms := TransactionTimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int32(TransactionTimeoutMs)
->
    [
        ?encode_request_header_1(?INIT_PRODUCER_ID_REQUEST, 0, CorrelationId, ClientId),
        ?encode_nullable_string(TransactionalId),
        ?encode_int32(TransactionTimeoutMs)
    ];
encode_init_producer_id_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        transaction_timeout_ms => int32
    }).

-spec decode_init_producer_id_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_request_0(),
    Rest :: binary().

decode_init_producer_id_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int32(TransactionTimeoutMs, Bin1, Bin2),
    {
        Header#{
            transactional_id => TransactionalId,
            transaction_timeout_ms => TransactionTimeoutMs
        },
        Bin2
    }.

-spec encode_init_producer_id_request_1(init_producer_id_request_1()) -> iodata().

encode_init_producer_id_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The time in ms to wait before aborting idle transactions sent by this producer. This is only relevant if a TransactionalId has been defined.
        transaction_timeout_ms := TransactionTimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int32(TransactionTimeoutMs)
->
    [
        ?encode_request_header_1(?INIT_PRODUCER_ID_REQUEST, 1, CorrelationId, ClientId),
        ?encode_nullable_string(TransactionalId),
        ?encode_int32(TransactionTimeoutMs)
    ];
encode_init_producer_id_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        transaction_timeout_ms => int32
    }).

-spec decode_init_producer_id_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_request_1(),
    Rest :: binary().

decode_init_producer_id_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int32(TransactionTimeoutMs, Bin1, Bin2),
    {
        Header#{
            transactional_id => TransactionalId,
            transaction_timeout_ms => TransactionTimeoutMs
        },
        Bin2
    }.

-spec encode_init_producer_id_request_2(init_producer_id_request_2()) -> iodata().

encode_init_producer_id_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The time in ms to wait before aborting idle transactions sent by this producer. This is only relevant if a TransactionalId has been defined.
        transaction_timeout_ms := TransactionTimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int32(TransactionTimeoutMs)
->
    [
        ?encode_request_header_2(?INIT_PRODUCER_ID_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_nullable_string(TransactionalId),
        ?encode_int32(TransactionTimeoutMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_init_producer_id_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        transaction_timeout_ms => int32
    }).

-spec decode_init_producer_id_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_request_2(),
    Rest :: binary().

decode_init_producer_id_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int32(TransactionTimeoutMs, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_init_producer_id_request_2_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            transaction_timeout_ms => TransactionTimeoutMs
        },
        Bin2
    ).

-spec decode_init_producer_id_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: init_producer_id_request_2().

decode_init_producer_id_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_init_producer_id_request_3(init_producer_id_request_3()) -> iodata().

encode_init_producer_id_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The time in ms to wait before aborting idle transactions sent by this producer. This is only relevant if a TransactionalId has been defined.
        transaction_timeout_ms := TransactionTimeoutMs,
        % The producer id. This is used to disambiguate requests if a transactional id is reused following its expiration.
        producer_id := ProducerId,
        % The producer's current epoch. This will be checked against the producer epoch on the broker, and the request will return an error if they do not match.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int32(TransactionTimeoutMs),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_request_header_2(?INIT_PRODUCER_ID_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_nullable_string(TransactionalId),
        ?encode_int32(TransactionTimeoutMs),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_init_producer_id_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        transaction_timeout_ms => int32,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_request_3(),
    Rest :: binary().

decode_init_producer_id_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int32(TransactionTimeoutMs, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_init_producer_id_request_3_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            transaction_timeout_ms => TransactionTimeoutMs,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    ).

-spec decode_init_producer_id_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: init_producer_id_request_3().

decode_init_producer_id_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_init_producer_id_request_4(init_producer_id_request_4()) -> iodata().

encode_init_producer_id_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The time in ms to wait before aborting idle transactions sent by this producer. This is only relevant if a TransactionalId has been defined.
        transaction_timeout_ms := TransactionTimeoutMs,
        % The producer id. This is used to disambiguate requests if a transactional id is reused following its expiration.
        producer_id := ProducerId,
        % The producer's current epoch. This will be checked against the producer epoch on the broker, and the request will return an error if they do not match.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int32(TransactionTimeoutMs),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_request_header_2(?INIT_PRODUCER_ID_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_nullable_string(TransactionalId),
        ?encode_int32(TransactionTimeoutMs),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_init_producer_id_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        transaction_timeout_ms => int32,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_request_4(),
    Rest :: binary().

decode_init_producer_id_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int32(TransactionTimeoutMs, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_init_producer_id_request_4_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            transaction_timeout_ms => TransactionTimeoutMs,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    ).

-spec decode_init_producer_id_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: init_producer_id_request_4().

decode_init_producer_id_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_init_producer_id_request_5(init_producer_id_request_5()) -> iodata().

encode_init_producer_id_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The time in ms to wait before aborting idle transactions sent by this producer. This is only relevant if a TransactionalId has been defined.
        transaction_timeout_ms := TransactionTimeoutMs,
        % The producer id. This is used to disambiguate requests if a transactional id is reused following its expiration.
        producer_id := ProducerId,
        % The producer's current epoch. This will be checked against the producer epoch on the broker, and the request will return an error if they do not match.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int32(TransactionTimeoutMs),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_request_header_2(?INIT_PRODUCER_ID_REQUEST, 5, CorrelationId, ClientId),
        ?encode_compact_nullable_string(TransactionalId),
        ?encode_int32(TransactionTimeoutMs),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_init_producer_id_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        transaction_timeout_ms => int32,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_request_5(),
    Rest :: binary().

decode_init_producer_id_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int32(TransactionTimeoutMs, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_init_producer_id_request_5_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            transaction_timeout_ms => TransactionTimeoutMs,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    ).

-spec decode_init_producer_id_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: init_producer_id_request_5().

decode_init_producer_id_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type init_producer_id_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    transaction_timeout_ms := integer()
}.
-type init_producer_id_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    transaction_timeout_ms := integer()
}.
-type init_producer_id_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    transaction_timeout_ms := integer()
}.
-type init_producer_id_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    transaction_timeout_ms := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
-type init_producer_id_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    transaction_timeout_ms := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
-type init_producer_id_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    transaction_timeout_ms := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
