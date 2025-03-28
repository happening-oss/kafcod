-module(add_partitions_to_txn_request).
-export([
    encode_add_partitions_to_txn_request_0/1,
    decode_add_partitions_to_txn_request_0/1,
    encode_add_partitions_to_txn_request_1/1,
    decode_add_partitions_to_txn_request_1/1,
    encode_add_partitions_to_txn_request_2/1,
    decode_add_partitions_to_txn_request_2/1,
    encode_add_partitions_to_txn_request_3/1,
    decode_add_partitions_to_txn_request_3/1,
    encode_add_partitions_to_txn_request_4/1,
    decode_add_partitions_to_txn_request_4/1,
    encode_add_partitions_to_txn_request_5/1,
    decode_add_partitions_to_txn_request_5/1
]).
-export_type([
    add_partitions_to_txn_request_0/0,
    add_partitions_to_txn_topic_0/0,
    add_partitions_to_txn_request_1/0,
    add_partitions_to_txn_topic_1/0,
    add_partitions_to_txn_request_2/0,
    add_partitions_to_txn_topic_2/0,
    add_partitions_to_txn_request_3/0,
    add_partitions_to_txn_topic_3/0,
    add_partitions_to_txn_request_4/0,
    add_partitions_to_txn_transaction_4/0,
    add_partitions_to_txn_topic_4/0,
    add_partitions_to_txn_request_5/0,
    add_partitions_to_txn_transaction_5/0,
    add_partitions_to_txn_topic_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ADD_PARTITIONS_TO_TXN_REQUEST, 24).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_add_partitions_to_txn_request_0(add_partitions_to_txn_request_0()) -> iodata().

encode_add_partitions_to_txn_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id corresponding to the transaction.
        v3_and_below_transactional_id := V3AndBelowTransactionalId,
        % Current producer id in use by the transactional id.
        v3_and_below_producer_id := V3AndBelowProducerId,
        % Current epoch associated with the producer id.
        v3_and_below_producer_epoch := V3AndBelowProducerEpoch,
        % The partitions to add to the transaction.
        v3_and_below_topics := V3AndBelowTopics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(V3AndBelowTransactionalId),
    ?is_int64(V3AndBelowProducerId),
    ?is_int16(V3AndBelowProducerEpoch),
    ?is_array(V3AndBelowTopics)
->
    [
        ?encode_request_header_1(?ADD_PARTITIONS_TO_TXN_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(V3AndBelowTransactionalId),
        ?encode_int64(V3AndBelowProducerId),
        ?encode_int16(V3AndBelowProducerEpoch),
        ?encode_array(V3AndBelowTopics, fun encode_add_partitions_to_txn_topic_0/1)
    ];
encode_add_partitions_to_txn_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        v3_and_below_transactional_id => string,
        v3_and_below_producer_id => int64,
        v3_and_below_producer_epoch => int16,
        v3_and_below_topics => {array, add_partitions_to_txn_topic_0}
    }).

-spec decode_add_partitions_to_txn_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_request_0(),
    Rest :: binary().

decode_add_partitions_to_txn_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(V3AndBelowTransactionalId, Bin0, Bin1),
    ?_decode_int64(V3AndBelowProducerId, Bin1, Bin2),
    ?_decode_int16(V3AndBelowProducerEpoch, Bin2, Bin3),
    ?_decode_array(V3AndBelowTopics, Bin3, Bin4, ?_decode_element(decode_add_partitions_to_txn_topic_0)),
    {
        Header#{
            v3_and_below_transactional_id => V3AndBelowTransactionalId,
            v3_and_below_producer_id => V3AndBelowProducerId,
            v3_and_below_producer_epoch => V3AndBelowProducerEpoch,
            v3_and_below_topics => V3AndBelowTopics
        },
        Bin4
    }.

-spec encode_add_partitions_to_txn_topic_0(add_partitions_to_txn_topic_0()) -> iodata().

encode_add_partitions_to_txn_topic_0(
    _Args = #{
        % The name of the topic.
        name := Name,
        % The partition indexes to add to the transaction
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_add_partitions_to_txn_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_add_partitions_to_txn_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_0(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_request_1(add_partitions_to_txn_request_1()) -> iodata().

encode_add_partitions_to_txn_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id corresponding to the transaction.
        v3_and_below_transactional_id := V3AndBelowTransactionalId,
        % Current producer id in use by the transactional id.
        v3_and_below_producer_id := V3AndBelowProducerId,
        % Current epoch associated with the producer id.
        v3_and_below_producer_epoch := V3AndBelowProducerEpoch,
        % The partitions to add to the transaction.
        v3_and_below_topics := V3AndBelowTopics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(V3AndBelowTransactionalId),
    ?is_int64(V3AndBelowProducerId),
    ?is_int16(V3AndBelowProducerEpoch),
    ?is_array(V3AndBelowTopics)
->
    [
        ?encode_request_header_1(?ADD_PARTITIONS_TO_TXN_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(V3AndBelowTransactionalId),
        ?encode_int64(V3AndBelowProducerId),
        ?encode_int16(V3AndBelowProducerEpoch),
        ?encode_array(V3AndBelowTopics, fun encode_add_partitions_to_txn_topic_1/1)
    ];
encode_add_partitions_to_txn_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        v3_and_below_transactional_id => string,
        v3_and_below_producer_id => int64,
        v3_and_below_producer_epoch => int16,
        v3_and_below_topics => {array, add_partitions_to_txn_topic_1}
    }).

-spec decode_add_partitions_to_txn_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_request_1(),
    Rest :: binary().

decode_add_partitions_to_txn_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(V3AndBelowTransactionalId, Bin0, Bin1),
    ?_decode_int64(V3AndBelowProducerId, Bin1, Bin2),
    ?_decode_int16(V3AndBelowProducerEpoch, Bin2, Bin3),
    ?_decode_array(V3AndBelowTopics, Bin3, Bin4, ?_decode_element(decode_add_partitions_to_txn_topic_1)),
    {
        Header#{
            v3_and_below_transactional_id => V3AndBelowTransactionalId,
            v3_and_below_producer_id => V3AndBelowProducerId,
            v3_and_below_producer_epoch => V3AndBelowProducerEpoch,
            v3_and_below_topics => V3AndBelowTopics
        },
        Bin4
    }.

-spec encode_add_partitions_to_txn_topic_1(add_partitions_to_txn_topic_1()) -> iodata().

encode_add_partitions_to_txn_topic_1(
    _Args = #{
        % The name of the topic.
        name := Name,
        % The partition indexes to add to the transaction
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_add_partitions_to_txn_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_add_partitions_to_txn_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_1(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_request_2(add_partitions_to_txn_request_2()) -> iodata().

encode_add_partitions_to_txn_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id corresponding to the transaction.
        v3_and_below_transactional_id := V3AndBelowTransactionalId,
        % Current producer id in use by the transactional id.
        v3_and_below_producer_id := V3AndBelowProducerId,
        % Current epoch associated with the producer id.
        v3_and_below_producer_epoch := V3AndBelowProducerEpoch,
        % The partitions to add to the transaction.
        v3_and_below_topics := V3AndBelowTopics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(V3AndBelowTransactionalId),
    ?is_int64(V3AndBelowProducerId),
    ?is_int16(V3AndBelowProducerEpoch),
    ?is_array(V3AndBelowTopics)
->
    [
        ?encode_request_header_1(?ADD_PARTITIONS_TO_TXN_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(V3AndBelowTransactionalId),
        ?encode_int64(V3AndBelowProducerId),
        ?encode_int16(V3AndBelowProducerEpoch),
        ?encode_array(V3AndBelowTopics, fun encode_add_partitions_to_txn_topic_2/1)
    ];
encode_add_partitions_to_txn_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        v3_and_below_transactional_id => string,
        v3_and_below_producer_id => int64,
        v3_and_below_producer_epoch => int16,
        v3_and_below_topics => {array, add_partitions_to_txn_topic_2}
    }).

-spec decode_add_partitions_to_txn_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_request_2(),
    Rest :: binary().

decode_add_partitions_to_txn_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(V3AndBelowTransactionalId, Bin0, Bin1),
    ?_decode_int64(V3AndBelowProducerId, Bin1, Bin2),
    ?_decode_int16(V3AndBelowProducerEpoch, Bin2, Bin3),
    ?_decode_array(V3AndBelowTopics, Bin3, Bin4, ?_decode_element(decode_add_partitions_to_txn_topic_2)),
    {
        Header#{
            v3_and_below_transactional_id => V3AndBelowTransactionalId,
            v3_and_below_producer_id => V3AndBelowProducerId,
            v3_and_below_producer_epoch => V3AndBelowProducerEpoch,
            v3_and_below_topics => V3AndBelowTopics
        },
        Bin4
    }.

-spec encode_add_partitions_to_txn_topic_2(add_partitions_to_txn_topic_2()) -> iodata().

encode_add_partitions_to_txn_topic_2(
    _Args = #{
        % The name of the topic.
        name := Name,
        % The partition indexes to add to the transaction
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_add_partitions_to_txn_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_add_partitions_to_txn_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_2(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_request_3(add_partitions_to_txn_request_3()) -> iodata().

encode_add_partitions_to_txn_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional id corresponding to the transaction.
        v3_and_below_transactional_id := V3AndBelowTransactionalId,
        % Current producer id in use by the transactional id.
        v3_and_below_producer_id := V3AndBelowProducerId,
        % Current epoch associated with the producer id.
        v3_and_below_producer_epoch := V3AndBelowProducerEpoch,
        % The partitions to add to the transaction.
        v3_and_below_topics := V3AndBelowTopics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(V3AndBelowTransactionalId),
    ?is_int64(V3AndBelowProducerId),
    ?is_int16(V3AndBelowProducerEpoch),
    ?is_array(V3AndBelowTopics)
->
    [
        ?encode_request_header_2(?ADD_PARTITIONS_TO_TXN_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_string(V3AndBelowTransactionalId),
        ?encode_int64(V3AndBelowProducerId),
        ?encode_int16(V3AndBelowProducerEpoch),
        ?encode_compact_array(V3AndBelowTopics, fun encode_add_partitions_to_txn_topic_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        v3_and_below_transactional_id => string,
        v3_and_below_producer_id => int64,
        v3_and_below_producer_epoch => int16,
        v3_and_below_topics => {array, add_partitions_to_txn_topic_3}
    }).

-spec decode_add_partitions_to_txn_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_request_3(),
    Rest :: binary().

decode_add_partitions_to_txn_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(V3AndBelowTransactionalId, Bin0, Bin1),
    ?_decode_int64(V3AndBelowProducerId, Bin1, Bin2),
    ?_decode_int16(V3AndBelowProducerEpoch, Bin2, Bin3),
    ?_decode_compact_array(V3AndBelowTopics, Bin3, Bin4, ?_decode_element(decode_add_partitions_to_txn_topic_3)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_request_3_tagged_field/3,
        Header#{
            v3_and_below_transactional_id => V3AndBelowTransactionalId,
            v3_and_below_producer_id => V3AndBelowProducerId,
            v3_and_below_producer_epoch => V3AndBelowProducerEpoch,
            v3_and_below_topics => V3AndBelowTopics
        },
        Bin4
    ).

-spec decode_add_partitions_to_txn_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_add_partitions_to_txn_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_topic_3(add_partitions_to_txn_topic_3()) -> iodata().

encode_add_partitions_to_txn_topic_3(
    _Args = #{
        % The name of the topic.
        name := Name,
        % The partition indexes to add to the transaction
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_add_partitions_to_txn_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_3(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_topic_3_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_topic_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_add_partitions_to_txn_topic_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_request_4(add_partitions_to_txn_request_4()) -> iodata().

encode_add_partitions_to_txn_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % List of transactions to add partitions to.
        transactions := Transactions
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Transactions)
->
    [
        ?encode_request_header_2(?ADD_PARTITIONS_TO_TXN_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_array(Transactions, fun encode_add_partitions_to_txn_transaction_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactions => {array, add_partitions_to_txn_transaction_4}
    }).

-spec decode_add_partitions_to_txn_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_request_4(),
    Rest :: binary().

decode_add_partitions_to_txn_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Transactions, Bin0, Bin1, ?_decode_element(decode_add_partitions_to_txn_transaction_4)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_request_4_tagged_field/3,
        Header#{
            transactions => Transactions
        },
        Bin1
    ).

-spec decode_add_partitions_to_txn_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_add_partitions_to_txn_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_transaction_4(add_partitions_to_txn_transaction_4()) -> iodata().

encode_add_partitions_to_txn_transaction_4(
    _Args = #{
        % The transactional id corresponding to the transaction.
        transactional_id := TransactionalId,
        % Current producer id in use by the transactional id.
        producer_id := ProducerId,
        % Current epoch associated with the producer id.
        producer_epoch := ProducerEpoch,
        % Boolean to signify if we want to check if the partition is in the transaction rather than add it.
        verify_only := VerifyOnly,
        % The partitions to add to the transaction.
        topics := Topics
    }
) when
    ?is_string(TransactionalId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_bool(VerifyOnly),
    ?is_array(Topics)
->
    [
        ?encode_compact_string(TransactionalId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_bool(VerifyOnly),
        ?encode_compact_array(Topics, fun encode_add_partitions_to_txn_topic_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_transaction_4(Args) ->
    ?encoder_error(Args, #{
        transactional_id => string,
        producer_id => int64,
        producer_epoch => int16,
        verify_only => bool,
        topics => {array, add_partitions_to_txn_topic_4}
    }).

-spec decode_add_partitions_to_txn_transaction_4(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_transaction_4(),
    Rest :: binary().

decode_add_partitions_to_txn_transaction_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TransactionalId, Bin0, Bin1),
    ?_decode_int64(ProducerId, Bin1, Bin2),
    ?_decode_int16(ProducerEpoch, Bin2, Bin3),
    ?_decode_bool(VerifyOnly, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_add_partitions_to_txn_topic_4)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_transaction_4_tagged_field/3,
        #{
            transactional_id => TransactionalId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            verify_only => VerifyOnly,
            topics => Topics
        },
        Bin5
    ).

-spec decode_add_partitions_to_txn_transaction_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_add_partitions_to_txn_transaction_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_topic_4(add_partitions_to_txn_topic_4()) -> iodata().

encode_add_partitions_to_txn_topic_4(
    _Args = #{
        % The name of the topic.
        name := Name,
        % The partition indexes to add to the transaction
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_add_partitions_to_txn_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_4(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_topic_4_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_topic_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_add_partitions_to_txn_topic_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_request_5(add_partitions_to_txn_request_5()) -> iodata().

encode_add_partitions_to_txn_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % List of transactions to add partitions to.
        transactions := Transactions
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Transactions)
->
    [
        ?encode_request_header_2(?ADD_PARTITIONS_TO_TXN_REQUEST, 5, CorrelationId, ClientId),
        ?encode_compact_array(Transactions, fun encode_add_partitions_to_txn_transaction_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactions => {array, add_partitions_to_txn_transaction_5}
    }).

-spec decode_add_partitions_to_txn_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_request_5(),
    Rest :: binary().

decode_add_partitions_to_txn_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Transactions, Bin0, Bin1, ?_decode_element(decode_add_partitions_to_txn_transaction_5)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_request_5_tagged_field/3,
        Header#{
            transactions => Transactions
        },
        Bin1
    ).

-spec decode_add_partitions_to_txn_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_add_partitions_to_txn_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_transaction_5(add_partitions_to_txn_transaction_5()) -> iodata().

encode_add_partitions_to_txn_transaction_5(
    _Args = #{
        % The transactional id corresponding to the transaction.
        transactional_id := TransactionalId,
        % Current producer id in use by the transactional id.
        producer_id := ProducerId,
        % Current epoch associated with the producer id.
        producer_epoch := ProducerEpoch,
        % Boolean to signify if we want to check if the partition is in the transaction rather than add it.
        verify_only := VerifyOnly,
        % The partitions to add to the transaction.
        topics := Topics
    }
) when
    ?is_string(TransactionalId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_bool(VerifyOnly),
    ?is_array(Topics)
->
    [
        ?encode_compact_string(TransactionalId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_bool(VerifyOnly),
        ?encode_compact_array(Topics, fun encode_add_partitions_to_txn_topic_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_transaction_5(Args) ->
    ?encoder_error(Args, #{
        transactional_id => string,
        producer_id => int64,
        producer_epoch => int16,
        verify_only => bool,
        topics => {array, add_partitions_to_txn_topic_5}
    }).

-spec decode_add_partitions_to_txn_transaction_5(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_transaction_5(),
    Rest :: binary().

decode_add_partitions_to_txn_transaction_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TransactionalId, Bin0, Bin1),
    ?_decode_int64(ProducerId, Bin1, Bin2),
    ?_decode_int16(ProducerEpoch, Bin2, Bin3),
    ?_decode_bool(VerifyOnly, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_add_partitions_to_txn_topic_5)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_transaction_5_tagged_field/3,
        #{
            transactional_id => TransactionalId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            verify_only => VerifyOnly,
            topics => Topics
        },
        Bin5
    ).

-spec decode_add_partitions_to_txn_transaction_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_add_partitions_to_txn_transaction_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_topic_5(add_partitions_to_txn_topic_5()) -> iodata().

encode_add_partitions_to_txn_topic_5(
    _Args = #{
        % The name of the topic.
        name := Name,
        % The partition indexes to add to the transaction
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_topic_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_add_partitions_to_txn_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_5(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_topic_5_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_topic_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_add_partitions_to_txn_topic_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type add_partitions_to_txn_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    v3_and_below_transactional_id := binary(),
    v3_and_below_producer_id := integer(),
    v3_and_below_producer_epoch := integer(),
    v3_and_below_topics := list(add_partitions_to_txn_topic_0())
}.
-type add_partitions_to_txn_topic_0() :: #{
    name := binary(),
    partitions := list(integer())
}.
-type add_partitions_to_txn_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    v3_and_below_transactional_id := binary(),
    v3_and_below_producer_id := integer(),
    v3_and_below_producer_epoch := integer(),
    v3_and_below_topics := list(add_partitions_to_txn_topic_1())
}.
-type add_partitions_to_txn_topic_1() :: #{
    name := binary(),
    partitions := list(integer())
}.
-type add_partitions_to_txn_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    v3_and_below_transactional_id := binary(),
    v3_and_below_producer_id := integer(),
    v3_and_below_producer_epoch := integer(),
    v3_and_below_topics := list(add_partitions_to_txn_topic_2())
}.
-type add_partitions_to_txn_topic_2() :: #{
    name := binary(),
    partitions := list(integer())
}.
-type add_partitions_to_txn_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    v3_and_below_transactional_id := binary(),
    v3_and_below_producer_id := integer(),
    v3_and_below_producer_epoch := integer(),
    v3_and_below_topics := list(add_partitions_to_txn_topic_3())
}.
-type add_partitions_to_txn_topic_3() :: #{
    name := binary(),
    partitions := list(integer())
}.
-type add_partitions_to_txn_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactions := list(add_partitions_to_txn_transaction_4())
}.
-type add_partitions_to_txn_transaction_4() :: #{
    transactional_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    verify_only := boolean(),
    topics := list(add_partitions_to_txn_topic_4())
}.
-type add_partitions_to_txn_topic_4() :: #{
    name := binary(),
    partitions := list(integer())
}.
-type add_partitions_to_txn_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactions := list(add_partitions_to_txn_transaction_5())
}.
-type add_partitions_to_txn_transaction_5() :: #{
    transactional_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    verify_only := boolean(),
    topics := list(add_partitions_to_txn_topic_5())
}.
-type add_partitions_to_txn_topic_5() :: #{
    name := binary(),
    partitions := list(integer())
}.
