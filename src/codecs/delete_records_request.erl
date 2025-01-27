-module(delete_records_request).
-export([
    encode_delete_records_request_0/1,
    decode_delete_records_request_0/1,
    encode_delete_records_request_1/1,
    decode_delete_records_request_1/1,
    encode_delete_records_request_2/1,
    decode_delete_records_request_2/1
]).
-export_type([
    delete_records_request_0/0,
    delete_records_partition_0/0,
    delete_records_topic_0/0,
    delete_records_request_1/0,
    delete_records_partition_1/0,
    delete_records_topic_1/0,
    delete_records_request_2/0,
    delete_records_partition_2/0,
    delete_records_topic_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DELETE_RECORDS_REQUEST, 21).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_delete_records_request_0(delete_records_request_0()) -> iodata().

encode_delete_records_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to delete records from.
        topics := Topics,
        % How long to wait for the deletion to complete, in milliseconds.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?DELETE_RECORDS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_delete_records_topic_0/1),
        ?encode_int32(TimeoutMs)
    ];
encode_delete_records_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, delete_records_topic_0},
        timeout_ms => int32
    }).

-spec decode_delete_records_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_request_0(),
    Rest :: binary().

decode_delete_records_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_delete_records_topic_0)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs
        },
        Bin2
    }.

-spec encode_delete_records_partition_0(delete_records_partition_0()) -> iodata().

encode_delete_records_partition_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The deletion offset.
        offset := Offset
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(Offset)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(Offset)
    ];
encode_delete_records_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        offset => int64
    }).

-spec decode_delete_records_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_partition_0(),
    Rest :: binary().

decode_delete_records_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(Offset, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            offset => Offset
        },
        Bin2
    }.

-spec encode_delete_records_topic_0(delete_records_topic_0()) -> iodata().

encode_delete_records_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition that we want to delete records from.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_delete_records_partition_0/1)
    ];
encode_delete_records_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, delete_records_partition_0}
    }).

-spec decode_delete_records_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_topic_0(),
    Rest :: binary().

decode_delete_records_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_delete_records_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_delete_records_request_1(delete_records_request_1()) -> iodata().

encode_delete_records_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to delete records from.
        topics := Topics,
        % How long to wait for the deletion to complete, in milliseconds.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?DELETE_RECORDS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_delete_records_topic_1/1),
        ?encode_int32(TimeoutMs)
    ];
encode_delete_records_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, delete_records_topic_1},
        timeout_ms => int32
    }).

-spec decode_delete_records_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_request_1(),
    Rest :: binary().

decode_delete_records_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_delete_records_topic_1)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs
        },
        Bin2
    }.

-spec encode_delete_records_partition_1(delete_records_partition_1()) -> iodata().

encode_delete_records_partition_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The deletion offset.
        offset := Offset
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(Offset)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(Offset)
    ];
encode_delete_records_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        offset => int64
    }).

-spec decode_delete_records_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_partition_1(),
    Rest :: binary().

decode_delete_records_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(Offset, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            offset => Offset
        },
        Bin2
    }.

-spec encode_delete_records_topic_1(delete_records_topic_1()) -> iodata().

encode_delete_records_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition that we want to delete records from.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_delete_records_partition_1/1)
    ];
encode_delete_records_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, delete_records_partition_1}
    }).

-spec decode_delete_records_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_topic_1(),
    Rest :: binary().

decode_delete_records_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_delete_records_partition_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_delete_records_request_2(delete_records_request_2()) -> iodata().

encode_delete_records_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to delete records from.
        topics := Topics,
        % How long to wait for the deletion to complete, in milliseconds.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_2(?DELETE_RECORDS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_delete_records_topic_2/1),
        ?encode_int32(TimeoutMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_records_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, delete_records_topic_2},
        timeout_ms => int32
    }).

-spec decode_delete_records_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_request_2(),
    Rest :: binary().

decode_delete_records_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_delete_records_topic_2)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_delete_records_request_2_tagged_field/3,
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs
        },
        Bin2
    ).

-spec decode_delete_records_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_records_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_records_partition_2(delete_records_partition_2()) -> iodata().

encode_delete_records_partition_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The deletion offset.
        offset := Offset
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(Offset)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(Offset),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_records_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        offset => int64
    }).

-spec decode_delete_records_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_partition_2(),
    Rest :: binary().

decode_delete_records_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(Offset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_delete_records_partition_2_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            offset => Offset
        },
        Bin2
    ).

-spec decode_delete_records_partition_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_records_partition_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_records_topic_2(delete_records_topic_2()) -> iodata().

encode_delete_records_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition that we want to delete records from.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_delete_records_partition_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_records_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, delete_records_partition_2}
    }).

-spec decode_delete_records_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_records_topic_2(),
    Rest :: binary().

decode_delete_records_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_delete_records_partition_2)),
    ?decode_tagged_fields(
        fun decode_delete_records_topic_2_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_delete_records_topic_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_records_topic_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type delete_records_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(delete_records_topic_0()),
    timeout_ms := integer()
}.
-type delete_records_partition_0() :: #{
    partition_index := integer(),
    offset := integer()
}.
-type delete_records_topic_0() :: #{
    name := binary(),
    partitions := list(delete_records_partition_0())
}.
-type delete_records_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(delete_records_topic_1()),
    timeout_ms := integer()
}.
-type delete_records_partition_1() :: #{
    partition_index := integer(),
    offset := integer()
}.
-type delete_records_topic_1() :: #{
    name := binary(),
    partitions := list(delete_records_partition_1())
}.
-type delete_records_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(delete_records_topic_2()),
    timeout_ms := integer()
}.
-type delete_records_partition_2() :: #{
    partition_index := integer(),
    offset := integer()
}.
-type delete_records_topic_2() :: #{
    name := binary(),
    partitions := list(delete_records_partition_2())
}.
