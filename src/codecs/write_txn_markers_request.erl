-module(write_txn_markers_request).
-export([
    encode_write_txn_markers_request_0/1,
    decode_write_txn_markers_request_0/1,
    encode_write_txn_markers_request_1/1,
    decode_write_txn_markers_request_1/1
]).
-export_type([
    write_txn_markers_request_0/0,
    writable_txn_marker_topic_0/0,
    writable_txn_marker_0/0,
    write_txn_markers_request_1/0,
    writable_txn_marker_topic_1/0,
    writable_txn_marker_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(WRITE_TXN_MARKERS_REQUEST, 27).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_write_txn_markers_request_0(write_txn_markers_request_0()) -> iodata().

encode_write_txn_markers_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transaction markers to be written.
        markers := Markers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Markers)
->
    [
        ?encode_request_header_1(?WRITE_TXN_MARKERS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Markers, fun encode_writable_txn_marker_0/1)
    ];
encode_write_txn_markers_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        markers => {array, writable_txn_marker_0}
    }).

-spec decode_write_txn_markers_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: write_txn_markers_request_0(),
    Rest :: binary().

decode_write_txn_markers_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Markers, Bin0, Bin1, ?_decode_element(decode_writable_txn_marker_0)),
    {
        Header#{
            markers => Markers
        },
        Bin1
    }.

-spec encode_writable_txn_marker_topic_0(writable_txn_marker_topic_0()) -> iodata().

encode_writable_txn_marker_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The indexes of the partitions to write transaction markers for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionIndexes, ?encode_int32_)
    ];
encode_writable_txn_marker_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_writable_txn_marker_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_topic_0(),
    Rest :: binary().

decode_writable_txn_marker_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    }.

-spec encode_writable_txn_marker_0(writable_txn_marker_0()) -> iodata().

encode_writable_txn_marker_0(
    _Args = #{
        % The current producer ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer ID.
        producer_epoch := ProducerEpoch,
        % The result of the transaction to write to the partitions (false = ABORT, true = COMMIT).
        transaction_result := TransactionResult,
        % Each topic that we want to write transaction marker(s) for.
        topics := Topics,
        % Epoch associated with the transaction state partition hosted by this transaction coordinator
        coordinator_epoch := CoordinatorEpoch
    }
) when
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_bool(TransactionResult),
    ?is_array(Topics),
    ?is_int32(CoordinatorEpoch)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_bool(TransactionResult),
        ?encode_array(Topics, fun encode_writable_txn_marker_topic_0/1),
        ?encode_int32(CoordinatorEpoch)
    ];
encode_writable_txn_marker_0(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        producer_epoch => int16,
        transaction_result => bool,
        topics => {array, writable_txn_marker_topic_0},
        coordinator_epoch => int32
    }).

-spec decode_writable_txn_marker_0(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_0(),
    Rest :: binary().

decode_writable_txn_marker_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int16(ProducerEpoch, Bin1, Bin2),
    ?_decode_bool(TransactionResult, Bin2, Bin3),
    ?_decode_array(Topics, Bin3, Bin4, ?_decode_element(decode_writable_txn_marker_topic_0)),
    ?_decode_int32(CoordinatorEpoch, Bin4, Bin5),
    {
        #{
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            transaction_result => TransactionResult,
            topics => Topics,
            coordinator_epoch => CoordinatorEpoch
        },
        Bin5
    }.

-spec encode_write_txn_markers_request_1(write_txn_markers_request_1()) -> iodata().

encode_write_txn_markers_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transaction markers to be written.
        markers := Markers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Markers)
->
    [
        ?encode_request_header_2(?WRITE_TXN_MARKERS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_compact_array(Markers, fun encode_writable_txn_marker_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_write_txn_markers_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        markers => {array, writable_txn_marker_1}
    }).

-spec decode_write_txn_markers_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: write_txn_markers_request_1(),
    Rest :: binary().

decode_write_txn_markers_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Markers, Bin0, Bin1, ?_decode_element(decode_writable_txn_marker_1)),
    ?decode_tagged_fields(
        fun decode_write_txn_markers_request_1_tagged_field/3,
        Header#{
            markers => Markers
        },
        Bin1
    ).

-spec decode_write_txn_markers_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_write_txn_markers_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_writable_txn_marker_topic_1(writable_txn_marker_topic_1()) -> iodata().

encode_writable_txn_marker_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The indexes of the partitions to write transaction markers for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionIndexes, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_writable_txn_marker_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_writable_txn_marker_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_topic_1(),
    Rest :: binary().

decode_writable_txn_marker_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_writable_txn_marker_topic_1_tagged_field/3,
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    ).

-spec decode_writable_txn_marker_topic_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_writable_txn_marker_topic_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_writable_txn_marker_1(writable_txn_marker_1()) -> iodata().

encode_writable_txn_marker_1(
    _Args = #{
        % The current producer ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer ID.
        producer_epoch := ProducerEpoch,
        % The result of the transaction to write to the partitions (false = ABORT, true = COMMIT).
        transaction_result := TransactionResult,
        % Each topic that we want to write transaction marker(s) for.
        topics := Topics,
        % Epoch associated with the transaction state partition hosted by this transaction coordinator
        coordinator_epoch := CoordinatorEpoch
    }
) when
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_bool(TransactionResult),
    ?is_array(Topics),
    ?is_int32(CoordinatorEpoch)
->
    [
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_bool(TransactionResult),
        ?encode_compact_array(Topics, fun encode_writable_txn_marker_topic_1/1),
        ?encode_int32(CoordinatorEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_writable_txn_marker_1(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        producer_epoch => int16,
        transaction_result => bool,
        topics => {array, writable_txn_marker_topic_1},
        coordinator_epoch => int32
    }).

-spec decode_writable_txn_marker_1(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_1(),
    Rest :: binary().

decode_writable_txn_marker_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_int16(ProducerEpoch, Bin1, Bin2),
    ?_decode_bool(TransactionResult, Bin2, Bin3),
    ?_decode_compact_array(Topics, Bin3, Bin4, ?_decode_element(decode_writable_txn_marker_topic_1)),
    ?_decode_int32(CoordinatorEpoch, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_writable_txn_marker_1_tagged_field/3,
        #{
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            transaction_result => TransactionResult,
            topics => Topics,
            coordinator_epoch => CoordinatorEpoch
        },
        Bin5
    ).

-spec decode_writable_txn_marker_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_writable_txn_marker_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type write_txn_markers_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    markers := list(writable_txn_marker_0())
}.
-type writable_txn_marker_topic_0() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type writable_txn_marker_0() :: #{
    producer_id := integer(),
    producer_epoch := integer(),
    transaction_result := boolean(),
    topics := list(writable_txn_marker_topic_0()),
    coordinator_epoch := integer()
}.
-type write_txn_markers_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    markers := list(writable_txn_marker_1())
}.
-type writable_txn_marker_topic_1() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type writable_txn_marker_1() :: #{
    producer_id := integer(),
    producer_epoch := integer(),
    transaction_result := boolean(),
    topics := list(writable_txn_marker_topic_1()),
    coordinator_epoch := integer()
}.
