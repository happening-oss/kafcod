-module(describe_topic_partitions_response).
-export([
    encode_describe_topic_partitions_response_0/1,
    decode_describe_topic_partitions_response_0/1
]).
-export_type([
    describe_topic_partitions_response_0/0,
    describe_topic_partitions_response_partition_0/0,
    describe_topic_partitions_response_topic_0/0,
    cursor_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_topic_partitions_response_0(describe_topic_partitions_response_0()) -> iodata().

encode_describe_topic_partitions_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each topic in the response.
        topics := Topics,
        % The next topic and partition index to fetch details for.
        next_cursor := NextCursor
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics),
    ?is_nullable_entity(NextCursor)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_describe_topic_partitions_response_topic_0/1),
        encode_cursor_0(NextCursor),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_topic_partitions_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, describe_topic_partitions_response_topic_0},
        next_cursor => nullable_Cursor
    }).

-spec decode_describe_topic_partitions_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_topic_partitions_response_0(),
    Rest :: binary().

decode_describe_topic_partitions_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_describe_topic_partitions_response_topic_0)),
    ?_decode_entity(NextCursor, Bin2, Bin3, decode_cursor_0),
    ?decode_tagged_fields(
        fun decode_describe_topic_partitions_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics,
            next_cursor => NextCursor
        },
        Bin3
    ).

-spec decode_describe_topic_partitions_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_topic_partitions_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_topic_partitions_response_partition_0(describe_topic_partitions_response_partition_0()) -> iodata().

encode_describe_topic_partitions_response_partition_0(
    _Args = #{
        % The partition error, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the leader broker.
        leader_id := LeaderId,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The set of all nodes that host this partition.
        replica_nodes := ReplicaNodes,
        % The set of nodes that are in sync with the leader for this partition.
        isr_nodes := IsrNodes,
        % The new eligible leader replicas otherwise.
        eligible_leader_replicas := EligibleLeaderReplicas,
        % The last known ELR.
        last_known_elr := LastKnownElr,
        % The set of offline replicas of this partition.
        offline_replicas := OfflineReplicas
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch),
    ?is_array(ReplicaNodes),
    ?is_array(IsrNodes),
    ?is_nullable_array(EligibleLeaderReplicas),
    ?is_nullable_array(LastKnownElr),
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(ReplicaNodes, ?encode_int32_),
        ?encode_compact_array(IsrNodes, ?encode_int32_),
        ?encode_compact_nullable_array(EligibleLeaderReplicas, ?encode_int32_),
        ?encode_compact_nullable_array(LastKnownElr, ?encode_int32_),
        ?encode_compact_array(OfflineReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_topic_partitions_response_partition_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        leader_epoch => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        eligible_leader_replicas => {nullable_array, int32},
        last_known_elr => {nullable_array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_describe_topic_partitions_response_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_topic_partitions_response_partition_0(),
    Rest :: binary().

decode_describe_topic_partitions_response_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(ReplicaNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_compact_array(IsrNodes, Bin5, Bin6, ?decode_int32_),
    ?_decode_compact_nullable_array(EligibleLeaderReplicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_compact_nullable_array(LastKnownElr, Bin7, Bin8, ?decode_int32_),
    ?_decode_compact_array(OfflineReplicas, Bin8, Bin9, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_describe_topic_partitions_response_partition_0_tagged_field/3,
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            eligible_leader_replicas => EligibleLeaderReplicas,
            last_known_elr => LastKnownElr,
            offline_replicas => OfflineReplicas
        },
        Bin9
    ).

-spec decode_describe_topic_partitions_response_partition_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_topic_partitions_response_partition_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_topic_partitions_response_topic_0(describe_topic_partitions_response_topic_0()) -> iodata().

encode_describe_topic_partitions_response_topic_0(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % The topic id.
        topic_id := TopicId,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions,
        % 32-bit bitfield to represent authorized operations for this topic.
        topic_authorized_operations := TopicAuthorizedOperations
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(Name),
    ?is_uuid(TopicId),
    ?is_bool(IsInternal),
    ?is_array(Partitions),
    ?is_int32(TopicAuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(Name),
        ?encode_uuid(TopicId),
        ?encode_bool(IsInternal),
        ?encode_compact_array(Partitions, fun encode_describe_topic_partitions_response_partition_0/1),
        ?encode_int32(TopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_topic_partitions_response_topic_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => nullable_string,
        topic_id => uuid,
        is_internal => bool,
        partitions => {array, describe_topic_partitions_response_partition_0},
        topic_authorized_operations => int32
    }).

-spec decode_describe_topic_partitions_response_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_topic_partitions_response_topic_0(),
    Rest :: binary().

decode_describe_topic_partitions_response_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(Name, Bin1, Bin2),
    ?_decode_uuid(TopicId, Bin2, Bin3),
    ?_decode_bool(IsInternal, Bin3, Bin4),
    ?_decode_compact_array(Partitions, Bin4, Bin5, ?_decode_element(decode_describe_topic_partitions_response_partition_0)),
    ?_decode_int32(TopicAuthorizedOperations, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_describe_topic_partitions_response_topic_0_tagged_field/3,
        #{
            error_code => ErrorCode,
            name => Name,
            topic_id => TopicId,
            is_internal => IsInternal,
            partitions => Partitions,
            topic_authorized_operations => TopicAuthorizedOperations
        },
        Bin6
    ).

-spec decode_describe_topic_partitions_response_topic_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_topic_partitions_response_topic_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_cursor_0(cursor_0()) -> iodata().

encode_cursor_0(
    _Args = #{
        % The name for the first topic to process
        topic_name := TopicName,
        % The partition index to start with
        partition_index := PartitionIndex
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?EMPTY_TAG_BUFFER
    ];
encode_cursor_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32
    }).

-spec decode_cursor_0(binary()) -> {Decoded, Rest} when
    Decoded :: cursor_0(),
    Rest :: binary().

decode_cursor_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_cursor_0_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex
        },
        Bin2
    ).

-spec decode_cursor_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_cursor_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_topic_partitions_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(describe_topic_partitions_response_topic_0()),
    next_cursor := cursor_0() | null
}.
-type describe_topic_partitions_response_partition_0() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    eligible_leader_replicas := list(integer()) | null,
    last_known_elr := list(integer()) | null,
    offline_replicas := list(integer())
}.
-type describe_topic_partitions_response_topic_0() :: #{
    error_code := integer(),
    name := binary() | null,
    topic_id := kafcod:uuid(),
    is_internal := boolean(),
    partitions := list(describe_topic_partitions_response_partition_0()),
    topic_authorized_operations := integer()
}.
-type cursor_0() :: #{
    topic_name := binary(),
    partition_index := integer()
}.
