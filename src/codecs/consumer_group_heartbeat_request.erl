-module(consumer_group_heartbeat_request).
-export([
    encode_consumer_group_heartbeat_request_0/1,
    decode_consumer_group_heartbeat_request_0/1
]).
-export_type([
    consumer_group_heartbeat_request_0/0,
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
        % -1 if it didn't change since the last heartbeat; the maximum time in milliseconds that the coordinator will wait on the member to revoke its partitions otherwise.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % null if it didn't change since the last heartbeat; the subscribed topic names otherwise.
        subscribed_topic_names := SubscribedTopicNames,
        % null if not used or if it didn't change since the last heartbeat; the server side assignor to use otherwise.
        server_assignor := ServerAssignor,
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
    ?is_nullable_string(ServerAssignor),
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
        ?encode_compact_nullable_string(ServerAssignor),
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
        server_assignor => nullable_string,
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
    ?_decode_compact_nullable_string(ServerAssignor, Bin7, Bin8),
    ?_decode_compact_nullable_array(TopicPartitions, Bin8, Bin9, ?_decode_element(decode_topic_partitions_0)),
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
            server_assignor => ServerAssignor,
            topic_partitions => TopicPartitions
        },
        Bin9
    ).

-spec decode_consumer_group_heartbeat_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_consumer_group_heartbeat_request_0_tagged_field(_Tag, _Bin0, Acc) ->
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
    server_assignor := binary() | null,
    topic_partitions := list(topic_partitions_0()) | null
}.
-type topic_partitions_0() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(integer())
}.
