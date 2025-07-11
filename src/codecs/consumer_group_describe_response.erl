-module(consumer_group_describe_response).
-export([
    encode_consumer_group_describe_response_0/1,
    decode_consumer_group_describe_response_0/1
]).
-export_type([
    consumer_group_describe_response_0/0,
    member_0/0,
    described_group_0/0,
    assignment_0/0,
    topic_partitions_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_consumer_group_describe_response_0(consumer_group_describe_response_0()) -> iodata().

encode_consumer_group_describe_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each described group.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Groups)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Groups, fun encode_described_group_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_consumer_group_describe_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        groups => {array, described_group_0}
    }).

-spec decode_consumer_group_describe_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_group_describe_response_0(),
    Rest :: binary().

decode_consumer_group_describe_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Groups, Bin1, Bin2, ?_decode_element(decode_described_group_0)),
    ?decode_tagged_fields(
        fun decode_consumer_group_describe_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            groups => Groups
        },
        Bin2
    ).

-spec decode_consumer_group_describe_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: consumer_group_describe_response_0().

decode_consumer_group_describe_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_member_0(member_0()) -> iodata().

encode_member_0(
    _Args = #{
        % The member ID.
        member_id := MemberId,
        % The member instance ID.
        instance_id := InstanceId,
        % The member rack ID.
        rack_id := RackId,
        % The current member epoch.
        member_epoch := MemberEpoch,
        % The client ID.
        client_id := ClientId,
        % The client host.
        client_host := ClientHost,
        % The subscribed topic names.
        subscribed_topic_names := SubscribedTopicNames,
        % the subscribed topic regex otherwise or null of not provided.
        subscribed_topic_regex := SubscribedTopicRegex,
        % The current assignment.
        assignment := Assignment,
        % The target assignment.
        target_assignment := TargetAssignment
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(InstanceId),
    ?is_nullable_string(RackId),
    ?is_int32(MemberEpoch),
    ?is_string(ClientId),
    ?is_string(ClientHost),
    ?is_array(SubscribedTopicNames),
    ?is_nullable_string(SubscribedTopicRegex),
    ?is_entity(Assignment),
    ?is_entity(TargetAssignment)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(InstanceId),
        ?encode_compact_nullable_string(RackId),
        ?encode_int32(MemberEpoch),
        ?encode_compact_string(ClientId),
        ?encode_compact_string(ClientHost),
        ?encode_compact_array(SubscribedTopicNames, ?encode_compact_string_),
        ?encode_compact_nullable_string(SubscribedTopicRegex),
        encode_assignment_0(Assignment),
        encode_assignment_0(TargetAssignment),
        ?EMPTY_TAG_BUFFER
    ];
encode_member_0(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        instance_id => nullable_string,
        rack_id => nullable_string,
        member_epoch => int32,
        client_id => string,
        client_host => string,
        subscribed_topic_names => {array, string},
        subscribed_topic_regex => nullable_string,
        assignment => map,
        target_assignment => map
    }).

-spec decode_member_0(binary()) -> {Decoded, Rest} when
    Decoded :: member_0(),
    Rest :: binary().

decode_member_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(InstanceId, Bin1, Bin2),
    ?_decode_compact_nullable_string(RackId, Bin2, Bin3),
    ?_decode_int32(MemberEpoch, Bin3, Bin4),
    ?_decode_compact_string(ClientId, Bin4, Bin5),
    ?_decode_compact_string(ClientHost, Bin5, Bin6),
    ?_decode_compact_array(SubscribedTopicNames, Bin6, Bin7, ?decode_string_),
    ?_decode_compact_nullable_string(SubscribedTopicRegex, Bin7, Bin8),
    ?_decode_entity(Assignment, Bin8, Bin9, decode_assignment_0),
    ?_decode_entity(TargetAssignment, Bin9, Bin10, decode_assignment_0),
    ?decode_tagged_fields(
        fun decode_member_0_tagged_field/3,
        #{
            member_id => MemberId,
            instance_id => InstanceId,
            rack_id => RackId,
            member_epoch => MemberEpoch,
            client_id => ClientId,
            client_host => ClientHost,
            subscribed_topic_names => SubscribedTopicNames,
            subscribed_topic_regex => SubscribedTopicRegex,
            assignment => Assignment,
            target_assignment => TargetAssignment
        },
        Bin10
    ).

-spec decode_member_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: member_0().

decode_member_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_described_group_0(described_group_0()) -> iodata().

encode_described_group_0(
    _Args = #{
        % The describe error, or 0 if there was no error.
        error_code := ErrorCode,
        % The top-level error message, or null if there was no error.
        error_message := ErrorMessage,
        % The group ID string.
        group_id := GroupId,
        % The group state string, or the empty string.
        group_state := GroupState,
        % The group epoch.
        group_epoch := GroupEpoch,
        % The assignment epoch.
        assignment_epoch := AssignmentEpoch,
        % The selected assignor.
        assignor_name := AssignorName,
        % The members.
        members := Members,
        % 32-bit bitfield to represent authorized operations for this group.
        authorized_operations := AuthorizedOperations
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_string(GroupId),
    ?is_string(GroupState),
    ?is_int32(GroupEpoch),
    ?is_int32(AssignmentEpoch),
    ?is_string(AssignorName),
    ?is_array(Members),
    ?is_int32(AuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_string(GroupId),
        ?encode_compact_string(GroupState),
        ?encode_int32(GroupEpoch),
        ?encode_int32(AssignmentEpoch),
        ?encode_compact_string(AssignorName),
        ?encode_compact_array(Members, fun encode_member_0/1),
        ?encode_int32(AuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_described_group_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        group_id => string,
        group_state => string,
        group_epoch => int32,
        assignment_epoch => int32,
        assignor_name => string,
        members => {array, member_0},
        authorized_operations => int32
    }).

-spec decode_described_group_0(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_0(),
    Rest :: binary().

decode_described_group_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_compact_string(GroupId, Bin2, Bin3),
    ?_decode_compact_string(GroupState, Bin3, Bin4),
    ?_decode_int32(GroupEpoch, Bin4, Bin5),
    ?_decode_int32(AssignmentEpoch, Bin5, Bin6),
    ?_decode_compact_string(AssignorName, Bin6, Bin7),
    ?_decode_compact_array(Members, Bin7, Bin8, ?_decode_element(decode_member_0)),
    ?_decode_int32(AuthorizedOperations, Bin8, Bin9),
    ?decode_tagged_fields(
        fun decode_described_group_0_tagged_field/3,
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            group_id => GroupId,
            group_state => GroupState,
            group_epoch => GroupEpoch,
            assignment_epoch => AssignmentEpoch,
            assignor_name => AssignorName,
            members => Members,
            authorized_operations => AuthorizedOperations
        },
        Bin9
    ).

-spec decode_described_group_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: described_group_0().

decode_described_group_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_assignment_0(assignment_0()) -> iodata().

encode_assignment_0(
    _Args = #{
        % The assigned topic-partitions to the member.
        topic_partitions := TopicPartitions
    }
) when
    ?is_array(TopicPartitions)
->
    [
        ?encode_compact_array(TopicPartitions, fun encode_topic_partitions_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_assignment_0(Args) ->
    ?encoder_error(Args, #{
        topic_partitions => {array, topic_partitions_0}
    }).

-spec decode_assignment_0(binary()) -> {Decoded, Rest} when
    Decoded :: assignment_0(),
    Rest :: binary().

decode_assignment_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_array(TopicPartitions, Bin0, Bin1, ?_decode_element(decode_topic_partitions_0)),
    ?decode_tagged_fields(
        fun decode_assignment_0_tagged_field/3,
        #{
            topic_partitions => TopicPartitions
        },
        Bin1
    ).

-spec decode_assignment_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: assignment_0().

decode_assignment_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_partitions_0(topic_partitions_0()) -> iodata().

encode_topic_partitions_0(
    _Args = #{
        % The topic ID.
        topic_id := TopicId,
        % The topic name.
        topic_name := TopicName,
        % The partitions.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_string(TopicName),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_string(TopicName),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_partitions_0(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        topic_name => string,
        partitions => {array, int32}
    }).

-spec decode_topic_partitions_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partitions_0(),
    Rest :: binary().

decode_topic_partitions_0(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_string(TopicName, Bin1, Bin2),
    ?_decode_compact_array(Partitions, Bin2, Bin3, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_topic_partitions_0_tagged_field/3,
        #{
            topic_id => TopicId,
            topic_name => TopicName,
            partitions => Partitions
        },
        Bin3
    ).

-spec decode_topic_partitions_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_partitions_0().

decode_topic_partitions_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type consumer_group_describe_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    groups := list(described_group_0())
}.
-type member_0() :: #{
    member_id := binary(),
    instance_id := binary() | null,
    rack_id := binary() | null,
    member_epoch := integer(),
    client_id := binary(),
    client_host := binary(),
    subscribed_topic_names := list(binary()),
    subscribed_topic_regex := binary() | null,
    assignment := assignment_0(),
    target_assignment := assignment_0()
}.
-type described_group_0() :: #{
    error_code := integer(),
    error_message := binary() | null,
    group_id := binary(),
    group_state := binary(),
    group_epoch := integer(),
    assignment_epoch := integer(),
    assignor_name := binary(),
    members := list(member_0()),
    authorized_operations := integer()
}.
-type assignment_0() :: #{
    topic_partitions := list(topic_partitions_0())
}.
-type topic_partitions_0() :: #{
    topic_id := kafcod:uuid(),
    topic_name := binary(),
    partitions := list(integer())
}.
