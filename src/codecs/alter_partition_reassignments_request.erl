-module(alter_partition_reassignments_request).
-export([
    encode_alter_partition_reassignments_request_0/1,
    decode_alter_partition_reassignments_request_0/1
]).
-export_type([
    alter_partition_reassignments_request_0/0,
    reassignable_partition_0/0,
    reassignable_topic_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ALTER_PARTITION_REASSIGNMENTS_REQUEST, 45).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_alter_partition_reassignments_request_0(alter_partition_reassignments_request_0()) -> iodata().

encode_alter_partition_reassignments_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The time in ms to wait for the request to complete.
        timeout_ms := TimeoutMs,
        % The topics to reassign.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(TimeoutMs),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?ALTER_PARTITION_REASSIGNMENTS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(TimeoutMs),
        ?encode_compact_array(Topics, fun encode_reassignable_topic_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_reassignments_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        timeout_ms => int32,
        topics => {array, reassignable_topic_0}
    }).

-spec decode_alter_partition_reassignments_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_reassignments_request_0(),
    Rest :: binary().

decode_alter_partition_reassignments_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(TimeoutMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_reassignable_topic_0)),
    ?decode_tagged_fields(
        fun decode_alter_partition_reassignments_request_0_tagged_field/3,
        Header#{
            timeout_ms => TimeoutMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_alter_partition_reassignments_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_partition_reassignments_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_reassignable_partition_0(reassignable_partition_0()) -> iodata().

encode_reassignable_partition_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The replicas to place the partitions on, or null to cancel a pending reassignment for this partition.
        replicas := Replicas
    }
) when
    ?is_int32(PartitionIndex),
    ?is_nullable_array(Replicas)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_compact_nullable_array(Replicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_reassignable_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        replicas => {nullable_array, int32}
    }).

-spec decode_reassignable_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: reassignable_partition_0(),
    Rest :: binary().

decode_reassignable_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_compact_nullable_array(Replicas, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_reassignable_partition_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            replicas => Replicas
        },
        Bin2
    ).

-spec decode_reassignable_partition_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_reassignable_partition_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_reassignable_topic_0(reassignable_topic_0()) -> iodata().

encode_reassignable_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The partitions to reassign.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_reassignable_partition_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_reassignable_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, reassignable_partition_0}
    }).

-spec decode_reassignable_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: reassignable_topic_0(),
    Rest :: binary().

decode_reassignable_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_reassignable_partition_0)),
    ?decode_tagged_fields(
        fun decode_reassignable_topic_0_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_reassignable_topic_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_reassignable_topic_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type alter_partition_reassignments_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    timeout_ms := integer(),
    topics := list(reassignable_topic_0())
}.
-type reassignable_partition_0() :: #{
    partition_index := integer(),
    replicas := list(integer()) | null
}.
-type reassignable_topic_0() :: #{
    name := binary(),
    partitions := list(reassignable_partition_0())
}.
