-module(stop_replica_request).
-export([
    encode_stop_replica_request_0/1,
    decode_stop_replica_request_0/1,
    encode_stop_replica_request_1/1,
    decode_stop_replica_request_1/1,
    encode_stop_replica_request_2/1,
    decode_stop_replica_request_2/1,
    encode_stop_replica_request_3/1,
    decode_stop_replica_request_3/1,
    encode_stop_replica_request_4/1,
    decode_stop_replica_request_4/1
]).
-export_type([
    stop_replica_request_0/0,
    stop_replica_partition_v0_0/0,
    stop_replica_request_1/0,
    stop_replica_topic_v1_1/0,
    stop_replica_request_2/0,
    stop_replica_topic_v1_2/0,
    stop_replica_request_3/0,
    stop_replica_partition_state_3/0,
    stop_replica_topic_state_3/0,
    stop_replica_request_4/0,
    stop_replica_partition_state_4/0,
    stop_replica_topic_state_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(STOP_REPLICA_REQUEST, 5).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_stop_replica_request_0(stop_replica_request_0()) -> iodata().

encode_stop_replica_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % Whether these partitions should be deleted.
        delete_partitions := DeletePartitions,
        % The partitions to stop.
        ungrouped_partitions := UngroupedPartitions
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_bool(DeletePartitions),
    ?is_array(UngroupedPartitions)
->
    [
        ?encode_request_header_1(?STOP_REPLICA_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_bool(DeletePartitions),
        ?encode_array(UngroupedPartitions, fun encode_stop_replica_partition_v0_0/1)
    ];
encode_stop_replica_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        delete_partitions => bool,
        ungrouped_partitions => {array, stop_replica_partition_v0_0}
    }).

-spec decode_stop_replica_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_request_0(),
    Rest :: binary().

decode_stop_replica_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_bool(DeletePartitions, Bin2, Bin3),
    ?_decode_array(UngroupedPartitions, Bin3, Bin4, ?_decode_element(decode_stop_replica_partition_v0_0)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            delete_partitions => DeletePartitions,
            ungrouped_partitions => UngroupedPartitions
        },
        Bin4
    }.

-spec encode_stop_replica_partition_v0_0(stop_replica_partition_v0_0()) -> iodata().

encode_stop_replica_partition_v0_0(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex)
    ];
encode_stop_replica_partition_v0_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32
    }).

-spec decode_stop_replica_partition_v0_0(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_partition_v0_0(),
    Rest :: binary().

decode_stop_replica_partition_v0_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex
        },
        Bin2
    }.

-spec encode_stop_replica_request_1(stop_replica_request_1()) -> iodata().

encode_stop_replica_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker epoch.
        broker_epoch := BrokerEpoch,
        % Whether these partitions should be deleted.
        delete_partitions := DeletePartitions,
        % The topics to stop.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_bool(DeletePartitions),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?STOP_REPLICA_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_bool(DeletePartitions),
        ?encode_array(Topics, fun encode_stop_replica_topic_v1_1/1)
    ];
encode_stop_replica_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        delete_partitions => bool,
        topics => {array, stop_replica_topic_v1_1}
    }).

-spec decode_stop_replica_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_request_1(),
    Rest :: binary().

decode_stop_replica_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_bool(DeletePartitions, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_stop_replica_topic_v1_1)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            delete_partitions => DeletePartitions,
            topics => Topics
        },
        Bin5
    }.

-spec encode_stop_replica_topic_v1_1(stop_replica_topic_v1_1()) -> iodata().

encode_stop_replica_topic_v1_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes.
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
encode_stop_replica_topic_v1_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_stop_replica_topic_v1_1(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_topic_v1_1(),
    Rest :: binary().

decode_stop_replica_topic_v1_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    }.

-spec encode_stop_replica_request_2(stop_replica_request_2()) -> iodata().

encode_stop_replica_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker epoch.
        broker_epoch := BrokerEpoch,
        % Whether these partitions should be deleted.
        delete_partitions := DeletePartitions,
        % The topics to stop.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_bool(DeletePartitions),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?STOP_REPLICA_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_bool(DeletePartitions),
        ?encode_compact_array(Topics, fun encode_stop_replica_topic_v1_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        delete_partitions => bool,
        topics => {array, stop_replica_topic_v1_2}
    }).

-spec decode_stop_replica_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_request_2(),
    Rest :: binary().

decode_stop_replica_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_bool(DeletePartitions, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_stop_replica_topic_v1_2)),
    ?decode_tagged_fields(
        fun decode_stop_replica_request_2_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            delete_partitions => DeletePartitions,
            topics => Topics
        },
        Bin5
    ).

-spec decode_stop_replica_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_topic_v1_2(stop_replica_topic_v1_2()) -> iodata().

encode_stop_replica_topic_v1_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes.
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
encode_stop_replica_topic_v1_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_stop_replica_topic_v1_2(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_topic_v1_2(),
    Rest :: binary().

decode_stop_replica_topic_v1_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_stop_replica_topic_v1_2_tagged_field/3,
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    ).

-spec decode_stop_replica_topic_v1_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_topic_v1_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_request_3(stop_replica_request_3()) -> iodata().

encode_stop_replica_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker epoch.
        broker_epoch := BrokerEpoch,
        % Each topic.
        topic_states := TopicStates
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates)
->
    [
        ?encode_request_header_2(?STOP_REPLICA_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(TopicStates, fun encode_stop_replica_topic_state_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, stop_replica_topic_state_3}
    }).

-spec decode_stop_replica_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_request_3(),
    Rest :: binary().

decode_stop_replica_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_compact_array(TopicStates, Bin3, Bin4, ?_decode_element(decode_stop_replica_topic_state_3)),
    ?decode_tagged_fields(
        fun decode_stop_replica_request_3_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates
        },
        Bin4
    ).

-spec decode_stop_replica_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_partition_state_3(stop_replica_partition_state_3()) -> iodata().

encode_stop_replica_partition_state_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % Whether this partition should be deleted.
        delete_partition := DeletePartition
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderEpoch),
    ?is_bool(DeletePartition)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderEpoch),
        ?encode_bool(DeletePartition),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_partition_state_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        leader_epoch => int32,
        delete_partition => bool
    }).

-spec decode_stop_replica_partition_state_3(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_partition_state_3(),
    Rest :: binary().

decode_stop_replica_partition_state_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?_decode_bool(DeletePartition, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_stop_replica_partition_state_3_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            leader_epoch => LeaderEpoch,
            delete_partition => DeletePartition
        },
        Bin3
    ).

-spec decode_stop_replica_partition_state_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_partition_state_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_topic_state_3(stop_replica_topic_state_3()) -> iodata().

encode_stop_replica_topic_state_3(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The state of each partition
        partition_states := PartitionStates
    }
) when
    ?is_string(TopicName),
    ?is_array(PartitionStates)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_compact_array(PartitionStates, fun encode_stop_replica_partition_state_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_topic_state_3(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_states => {array, stop_replica_partition_state_3}
    }).

-spec decode_stop_replica_topic_state_3(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_topic_state_3(),
    Rest :: binary().

decode_stop_replica_topic_state_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_compact_array(PartitionStates, Bin1, Bin2, ?_decode_element(decode_stop_replica_partition_state_3)),
    ?decode_tagged_fields(
        fun decode_stop_replica_topic_state_3_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_states => PartitionStates
        },
        Bin2
    ).

-spec decode_stop_replica_topic_state_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_topic_state_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_request_4(stop_replica_request_4()) -> iodata().

encode_stop_replica_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % If KRaft controller id is used during migration. See KIP-866
        is_k_raft_controller := IsKRaftController,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker epoch.
        broker_epoch := BrokerEpoch,
        % Each topic.
        topic_states := TopicStates
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_bool(IsKRaftController),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates)
->
    [
        ?encode_request_header_2(?STOP_REPLICA_REQUEST, 4, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_bool(IsKRaftController),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(TopicStates, fun encode_stop_replica_topic_state_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        is_k_raft_controller => bool,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, stop_replica_topic_state_4}
    }).

-spec decode_stop_replica_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_request_4(),
    Rest :: binary().

decode_stop_replica_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_bool(IsKRaftController, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int64(BrokerEpoch, Bin3, Bin4),
    ?_decode_compact_array(TopicStates, Bin4, Bin5, ?_decode_element(decode_stop_replica_topic_state_4)),
    ?decode_tagged_fields(
        fun decode_stop_replica_request_4_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            is_k_raft_controller => IsKRaftController,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates
        },
        Bin5
    ).

-spec decode_stop_replica_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_partition_state_4(stop_replica_partition_state_4()) -> iodata().

encode_stop_replica_partition_state_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % Whether this partition should be deleted.
        delete_partition := DeletePartition
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderEpoch),
    ?is_bool(DeletePartition)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderEpoch),
        ?encode_bool(DeletePartition),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_partition_state_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        leader_epoch => int32,
        delete_partition => bool
    }).

-spec decode_stop_replica_partition_state_4(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_partition_state_4(),
    Rest :: binary().

decode_stop_replica_partition_state_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?_decode_bool(DeletePartition, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_stop_replica_partition_state_4_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            leader_epoch => LeaderEpoch,
            delete_partition => DeletePartition
        },
        Bin3
    ).

-spec decode_stop_replica_partition_state_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_partition_state_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_topic_state_4(stop_replica_topic_state_4()) -> iodata().

encode_stop_replica_topic_state_4(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The state of each partition
        partition_states := PartitionStates
    }
) when
    ?is_string(TopicName),
    ?is_array(PartitionStates)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_compact_array(PartitionStates, fun encode_stop_replica_partition_state_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_topic_state_4(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_states => {array, stop_replica_partition_state_4}
    }).

-spec decode_stop_replica_topic_state_4(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_topic_state_4(),
    Rest :: binary().

decode_stop_replica_topic_state_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_compact_array(PartitionStates, Bin1, Bin2, ?_decode_element(decode_stop_replica_partition_state_4)),
    ?decode_tagged_fields(
        fun decode_stop_replica_topic_state_4_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_states => PartitionStates
        },
        Bin2
    ).

-spec decode_stop_replica_topic_state_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_topic_state_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type stop_replica_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    delete_partitions := boolean(),
    ungrouped_partitions := list(stop_replica_partition_v0_0())
}.
-type stop_replica_partition_v0_0() :: #{
    topic_name := binary(),
    partition_index := integer()
}.
-type stop_replica_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    delete_partitions := boolean(),
    topics := list(stop_replica_topic_v1_1())
}.
-type stop_replica_topic_v1_1() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type stop_replica_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    delete_partitions := boolean(),
    topics := list(stop_replica_topic_v1_2())
}.
-type stop_replica_topic_v1_2() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type stop_replica_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(stop_replica_topic_state_3())
}.
-type stop_replica_partition_state_3() :: #{
    partition_index := integer(),
    leader_epoch := integer(),
    delete_partition := boolean()
}.
-type stop_replica_topic_state_3() :: #{
    topic_name := binary(),
    partition_states := list(stop_replica_partition_state_3())
}.
-type stop_replica_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    is_k_raft_controller := boolean(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(stop_replica_topic_state_4())
}.
-type stop_replica_partition_state_4() :: #{
    partition_index := integer(),
    leader_epoch := integer(),
    delete_partition := boolean()
}.
-type stop_replica_topic_state_4() :: #{
    topic_name := binary(),
    partition_states := list(stop_replica_partition_state_4())
}.
