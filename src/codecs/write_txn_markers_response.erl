-module(write_txn_markers_response).
-export([
    encode_write_txn_markers_response_0/1,
    decode_write_txn_markers_response_0/1,
    encode_write_txn_markers_response_1/1,
    decode_write_txn_markers_response_1/1
]).
-export_type([
    write_txn_markers_response_0/0,
    writable_txn_marker_partition_result_0/0,
    writable_txn_marker_topic_result_0/0,
    writable_txn_marker_result_0/0,
    write_txn_markers_response_1/0,
    writable_txn_marker_partition_result_1/0,
    writable_txn_marker_topic_result_1/0,
    writable_txn_marker_result_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_write_txn_markers_response_0(write_txn_markers_response_0()) -> iodata().

encode_write_txn_markers_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The results for writing makers.
        markers := Markers
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Markers)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Markers, fun encode_writable_txn_marker_result_0/1)
    ];
encode_write_txn_markers_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        markers => {array, writable_txn_marker_result_0}
    }).

-spec decode_write_txn_markers_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: write_txn_markers_response_0(),
    Rest :: binary().

decode_write_txn_markers_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Markers, Bin0, Bin1, ?_decode_element(decode_writable_txn_marker_result_0)),
    {
        Header#{
            markers => Markers
        },
        Bin1
    }.

-spec encode_writable_txn_marker_partition_result_0(writable_txn_marker_partition_result_0()) -> iodata().

encode_writable_txn_marker_partition_result_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_writable_txn_marker_partition_result_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_writable_txn_marker_partition_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_partition_result_0(),
    Rest :: binary().

decode_writable_txn_marker_partition_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_writable_txn_marker_topic_result_0(writable_txn_marker_topic_result_0()) -> iodata().

encode_writable_txn_marker_topic_result_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The results by partition.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_writable_txn_marker_partition_result_0/1)
    ];
encode_writable_txn_marker_topic_result_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, writable_txn_marker_partition_result_0}
    }).

-spec decode_writable_txn_marker_topic_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_topic_result_0(),
    Rest :: binary().

decode_writable_txn_marker_topic_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_writable_txn_marker_partition_result_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_writable_txn_marker_result_0(writable_txn_marker_result_0()) -> iodata().

encode_writable_txn_marker_result_0(
    _Args = #{
        % The current producer ID in use by the transactional ID.
        producer_id := ProducerId,
        % The results by topic.
        topics := Topics
    }
) when
    ?is_int64(ProducerId),
    ?is_array(Topics)
->
    [
        ?encode_int64(ProducerId),
        ?encode_array(Topics, fun encode_writable_txn_marker_topic_result_0/1)
    ];
encode_writable_txn_marker_result_0(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        topics => {array, writable_txn_marker_topic_result_0}
    }).

-spec decode_writable_txn_marker_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_result_0(),
    Rest :: binary().

decode_writable_txn_marker_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_writable_txn_marker_topic_result_0)),
    {
        #{
            producer_id => ProducerId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_write_txn_markers_response_1(write_txn_markers_response_1()) -> iodata().

encode_write_txn_markers_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The results for writing makers.
        markers := Markers
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Markers)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_compact_array(Markers, fun encode_writable_txn_marker_result_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_write_txn_markers_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        markers => {array, writable_txn_marker_result_1}
    }).

-spec decode_write_txn_markers_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: write_txn_markers_response_1(),
    Rest :: binary().

decode_write_txn_markers_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_compact_array(Markers, Bin0, Bin1, ?_decode_element(decode_writable_txn_marker_result_1)),
    ?decode_tagged_fields(
        fun decode_write_txn_markers_response_1_tagged_field/3,
        Header#{
            markers => Markers
        },
        Bin1
    ).

-spec decode_write_txn_markers_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: write_txn_markers_response_1().

decode_write_txn_markers_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_writable_txn_marker_partition_result_1(writable_txn_marker_partition_result_1()) -> iodata().

encode_writable_txn_marker_partition_result_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_writable_txn_marker_partition_result_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_writable_txn_marker_partition_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_partition_result_1(),
    Rest :: binary().

decode_writable_txn_marker_partition_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_writable_txn_marker_partition_result_1_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_writable_txn_marker_partition_result_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: writable_txn_marker_partition_result_1().

decode_writable_txn_marker_partition_result_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_writable_txn_marker_topic_result_1(writable_txn_marker_topic_result_1()) -> iodata().

encode_writable_txn_marker_topic_result_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The results by partition.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_writable_txn_marker_partition_result_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_writable_txn_marker_topic_result_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, writable_txn_marker_partition_result_1}
    }).

-spec decode_writable_txn_marker_topic_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_topic_result_1(),
    Rest :: binary().

decode_writable_txn_marker_topic_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_writable_txn_marker_partition_result_1)),
    ?decode_tagged_fields(
        fun decode_writable_txn_marker_topic_result_1_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_writable_txn_marker_topic_result_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: writable_txn_marker_topic_result_1().

decode_writable_txn_marker_topic_result_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_writable_txn_marker_result_1(writable_txn_marker_result_1()) -> iodata().

encode_writable_txn_marker_result_1(
    _Args = #{
        % The current producer ID in use by the transactional ID.
        producer_id := ProducerId,
        % The results by topic.
        topics := Topics
    }
) when
    ?is_int64(ProducerId),
    ?is_array(Topics)
->
    [
        ?encode_int64(ProducerId),
        ?encode_compact_array(Topics, fun encode_writable_txn_marker_topic_result_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_writable_txn_marker_result_1(Args) ->
    ?encoder_error(Args, #{
        producer_id => int64,
        topics => {array, writable_txn_marker_topic_result_1}
    }).

-spec decode_writable_txn_marker_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: writable_txn_marker_result_1(),
    Rest :: binary().

decode_writable_txn_marker_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(ProducerId, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_writable_txn_marker_topic_result_1)),
    ?decode_tagged_fields(
        fun decode_writable_txn_marker_result_1_tagged_field/3,
        #{
            producer_id => ProducerId,
            topics => Topics
        },
        Bin2
    ).

-spec decode_writable_txn_marker_result_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: writable_txn_marker_result_1().

decode_writable_txn_marker_result_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type write_txn_markers_response_0() :: #{
    correlation_id => integer(),
    markers := list(writable_txn_marker_result_0())
}.
-type writable_txn_marker_partition_result_0() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type writable_txn_marker_topic_result_0() :: #{
    name := binary(),
    partitions := list(writable_txn_marker_partition_result_0())
}.
-type writable_txn_marker_result_0() :: #{
    producer_id := integer(),
    topics := list(writable_txn_marker_topic_result_0())
}.
-type write_txn_markers_response_1() :: #{
    correlation_id => integer(),
    markers := list(writable_txn_marker_result_1())
}.
-type writable_txn_marker_partition_result_1() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type writable_txn_marker_topic_result_1() :: #{
    name := binary(),
    partitions := list(writable_txn_marker_partition_result_1())
}.
-type writable_txn_marker_result_1() :: #{
    producer_id := integer(),
    topics := list(writable_txn_marker_topic_result_1())
}.
