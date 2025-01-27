-module(consumer_group_heartbeat_request).
-export([
    encode_consumer_group_heartbeat_request_0/1,
    decode_consumer_group_heartbeat_request_0/1
]).
-export_type([
    consumer_group_heartbeat_request_0/0,
    assignor_0/0,
    topic_partitions_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(CONSUMER_GROUP_HEARTBEAT_REQUEST, 68).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_consumer_group_heartbeat_request_0(consumer_group_heartbeat_request_0()) -> iodata().

encode_consumer_group_heartbeat_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The member id generated by the coordinator. The member id must be kept during the entire lifetime of the member.
        member_id := MemberId,
        % The current member epoch; 0 to join the group; -1 to leave the group; -2 to indicate that the static member will rejoin.
        member_epoch := MemberEpoch,
        % null if not provided or if it didn't change since the last heartbeat; the instance Id otherwise.
        instance_id := InstanceId,
        % null if not provided or if it didn't change since the last heartbeat; the rack ID of consumer otherwise.
        rack_id := RackId,
        % -1 if it didn't chance since the last heartbeat; the maximum time in milliseconds that the coordinator will wait on the member to revoke its partitions otherwise.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % null if it didn't change since the last heartbeat; the subscribed topic names otherwise.
        subscribed_topic_names := SubscribedTopicNames,
        % null if it didn't change since the last heartbeat; the subscribed topic regex otherwise
        subscribed_topic_regex := SubscribedTopicRegex,
        % null if not used or if it didn't change since the last heartbeat; the server side assignor to use otherwise.
        server_assignor := ServerAssignor,
        % null if not used or if it didn't change since the last heartbeat; the list of client-side assignors otherwise.
        client_assignors := ClientAssignors,
        % null if it didn't change since the last heartbeat; the partitions owned by the member.
        topic_partitions := TopicPartitions
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_string(MemberId),
    ?is_int32(MemberEpoch),
    ?is_nullable_string(InstanceId),
    ?is_nullable_string(RackId),
    ?is_int32(RebalanceTimeoutMs),
    ?is_nullable_array(SubscribedTopicNames),
    ?is_nullable_string(SubscribedTopicRegex),
    ?is_nullable_string(ServerAssignor),
    ?is_nullable_array(ClientAssignors),
    ?is_nullable_array(TopicPartitions)
->
    [
        ?encode_request_header_2(?CONSUMER_GROUP_HEARTBEAT_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_compact_string(MemberId),
        ?encode_int32(MemberEpoch),
        ?encode_compact_nullable_string(InstanceId),
        ?encode_compact_nullable_string(RackId),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_compact_nullable_array(SubscribedTopicNames, ?encode_compact_string_),
        ?encode_compact_nullable_string(SubscribedTopicRegex),
        ?encode_compact_nullable_string(ServerAssignor),
        ?encode_compact_nullable_array(ClientAssignors, fun encode_assignor_0/1),
        ?encode_compact_nullable_array(TopicPartitions, fun encode_topic_partitions_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_consumer_group_heartbeat_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        member_id => string,
        member_epoch => int32,
        instance_id => nullable_string,
        rack_id => nullable_string,
        rebalance_timeout_ms => int32,
        subscribed_topic_names => {nullable_array, string},
        subscribed_topic_regex => nullable_string,
        server_assignor => nullable_string,
        client_assignors => {nullable_array, assignor_0},
        topic_partitions => {nullable_array, topic_partitions_0}
    }).

-spec decode_consumer_group_heartbeat_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_group_heartbeat_request_0(),
    Rest :: binary().

decode_consumer_group_heartbeat_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_string(MemberId, Bin1, Bin2),
    ?_decode_int32(MemberEpoch, Bin2, Bin3),
    ?_decode_compact_nullable_string(InstanceId, Bin3, Bin4),
    ?_decode_compact_nullable_string(RackId, Bin4, Bin5),
    ?_decode_int32(RebalanceTimeoutMs, Bin5, Bin6),
    ?_decode_compact_nullable_array(SubscribedTopicNames, Bin6, Bin7, ?decode_string_),
    ?_decode_compact_nullable_string(SubscribedTopicRegex, Bin7, Bin8),
    ?_decode_compact_nullable_string(ServerAssignor, Bin8, Bin9),
    ?_decode_compact_nullable_array(ClientAssignors, Bin9, Bin10, ?_decode_element(decode_assignor_0)),
    ?_decode_compact_nullable_array(TopicPartitions, Bin10, Bin11, ?_decode_element(decode_topic_partitions_0)),
    ?decode_tagged_fields(
        fun decode_consumer_group_heartbeat_request_0_tagged_field/3,
        Header#{
            group_id => GroupId,
            member_id => MemberId,
            member_epoch => MemberEpoch,
            instance_id => InstanceId,
            rack_id => RackId,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            subscribed_topic_names => SubscribedTopicNames,
            subscribed_topic_regex => SubscribedTopicRegex,
            server_assignor => ServerAssignor,
            client_assignors => ClientAssignors,
            topic_partitions => TopicPartitions
        },
        Bin11
    ).

-spec decode_consumer_group_heartbeat_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_consumer_group_heartbeat_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_assignor_0(assignor_0()) -> iodata().

encode_assignor_0(
    _Args = #{
        % The name of the assignor.
        name := Name,
        % The minimum supported version for the metadata.
        minimum_version := MinimumVersion,
        % The maximum supported version for the metadata.
        maximum_version := MaximumVersion,
        % The reason of the metadata update.
        reason := Reason,
        % The version of the metadata.
        metadata_version := MetadataVersion,
        % The metadata.
        metadata_bytes := MetadataBytes
    }
) when
    ?is_string(Name),
    ?is_int16(MinimumVersion),
    ?is_int16(MaximumVersion),
    ?is_int8(Reason),
    ?is_int16(MetadataVersion),
    ?is_bytes(MetadataBytes)
->
    [
        ?encode_compact_string(Name),
        ?encode_int16(MinimumVersion),
        ?encode_int16(MaximumVersion),
        ?encode_int8(Reason),
        ?encode_int16(MetadataVersion),
        ?encode_compact_bytes(MetadataBytes),
        ?EMPTY_TAG_BUFFER
    ];
encode_assignor_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        minimum_version => int16,
        maximum_version => int16,
        reason => int8,
        metadata_version => int16,
        metadata_bytes => bytes
    }).

-spec decode_assignor_0(binary()) -> {Decoded, Rest} when
    Decoded :: assignor_0(),
    Rest :: binary().

decode_assignor_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(MinimumVersion, Bin1, Bin2),
    ?_decode_int16(MaximumVersion, Bin2, Bin3),
    ?_decode_int8(Reason, Bin3, Bin4),
    ?_decode_int16(MetadataVersion, Bin4, Bin5),
    ?_decode_compact_bytes(MetadataBytes, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_assignor_0_tagged_field/3,
        #{
            name => Name,
            minimum_version => MinimumVersion,
            maximum_version => MaximumVersion,
            reason => Reason,
            metadata_version => MetadataVersion,
            metadata_bytes => MetadataBytes
        },
        Bin6
    ).

-spec decode_assignor_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_assignor_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_partitions_0(topic_partitions_0()) -> iodata().

encode_topic_partitions_0(
    _Args = #{
        % The topic ID.
        topic_id := TopicId,
        % The partitions.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_partitions_0(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, int32}
    }).

-spec decode_topic_partitions_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partitions_0(),
    Rest :: binary().

decode_topic_partitions_0(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_topic_partitions_0_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_partitions_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_topic_partitions_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type consumer_group_heartbeat_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    member_id := binary(),
    member_epoch := integer(),
    instance_id := binary() | null,
    rack_id := binary() | null,
    rebalance_timeout_ms := integer(),
    subscribed_topic_names := list(binary()) | null,
    subscribed_topic_regex := binary() | null,
    server_assignor := binary() | null,
    client_assignors := list(assignor_0()) | null,
    topic_partitions := list(topic_partitions_0()) | null
}.
-type assignor_0() :: #{
    name := binary(),
    minimum_version := integer(),
    maximum_version := integer(),
    reason := integer(),
    metadata_version := integer(),
    metadata_bytes := kafcod:bytes()
}.
-type topic_partitions_0() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(integer())
}.
