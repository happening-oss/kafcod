-module(leader_and_isr_request).
-export([
    encode_leader_and_isr_request_0/1,
    decode_leader_and_isr_request_0/1,
    encode_leader_and_isr_request_1/1,
    decode_leader_and_isr_request_1/1,
    encode_leader_and_isr_request_2/1,
    decode_leader_and_isr_request_2/1,
    encode_leader_and_isr_request_3/1,
    decode_leader_and_isr_request_3/1,
    encode_leader_and_isr_request_4/1,
    decode_leader_and_isr_request_4/1,
    encode_leader_and_isr_request_5/1,
    decode_leader_and_isr_request_5/1,
    encode_leader_and_isr_request_6/1,
    decode_leader_and_isr_request_6/1,
    encode_leader_and_isr_request_7/1,
    decode_leader_and_isr_request_7/1
]).
-export_type([
    leader_and_isr_request_0/0,
    leader_and_isr_live_leader_0/0,
    leader_and_isr_partition_state_0/0,
    leader_and_isr_request_1/0,
    leader_and_isr_live_leader_1/0,
    leader_and_isr_partition_state_1/0,
    leader_and_isr_request_2/0,
    leader_and_isr_topic_state_2/0,
    leader_and_isr_live_leader_2/0,
    leader_and_isr_partition_state_2/0,
    leader_and_isr_request_3/0,
    leader_and_isr_topic_state_3/0,
    leader_and_isr_live_leader_3/0,
    leader_and_isr_partition_state_3/0,
    leader_and_isr_request_4/0,
    leader_and_isr_topic_state_4/0,
    leader_and_isr_live_leader_4/0,
    leader_and_isr_partition_state_4/0,
    leader_and_isr_request_5/0,
    leader_and_isr_topic_state_5/0,
    leader_and_isr_live_leader_5/0,
    leader_and_isr_partition_state_5/0,
    leader_and_isr_request_6/0,
    leader_and_isr_topic_state_6/0,
    leader_and_isr_live_leader_6/0,
    leader_and_isr_partition_state_6/0,
    leader_and_isr_request_7/0,
    leader_and_isr_topic_state_7/0,
    leader_and_isr_live_leader_7/0,
    leader_and_isr_partition_state_7/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(LEADER_AND_ISR_REQUEST, 4).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_leader_and_isr_request_0(leader_and_isr_request_0()) -> iodata().

encode_leader_and_isr_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The current controller ID.
        controller_id := ControllerId,
        % The current controller epoch.
        controller_epoch := ControllerEpoch,
        % The state of each partition, in a v0 or v1 message.
        ungrouped_partition_states := UngroupedPartitionStates,
        % The current live leaders.
        live_leaders := LiveLeaders
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_array(UngroupedPartitionStates),
    ?is_array(LiveLeaders)
->
    [
        ?encode_request_header_1(?LEADER_AND_ISR_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_array(UngroupedPartitionStates, fun encode_leader_and_isr_partition_state_0/1),
        ?encode_array(LiveLeaders, fun encode_leader_and_isr_live_leader_0/1)
    ];
encode_leader_and_isr_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        ungrouped_partition_states => {array, leader_and_isr_partition_state_0},
        live_leaders => {array, leader_and_isr_live_leader_0}
    }).

-spec decode_leader_and_isr_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_request_0(),
    Rest :: binary().

decode_leader_and_isr_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_array(UngroupedPartitionStates, Bin2, Bin3, ?_decode_element(decode_leader_and_isr_partition_state_0)),
    ?_decode_array(LiveLeaders, Bin3, Bin4, ?_decode_element(decode_leader_and_isr_live_leader_0)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            ungrouped_partition_states => UngroupedPartitionStates,
            live_leaders => LiveLeaders
        },
        Bin4
    }.

-spec encode_leader_and_isr_live_leader_0(leader_and_isr_live_leader_0()) -> iodata().

encode_leader_and_isr_live_leader_0(
    _Args = #{
        % The leader's broker ID.
        broker_id := BrokerId,
        % The leader's hostname.
        host_name := HostName,
        % The leader's port.
        port := Port
    }
) when
    ?is_int32(BrokerId),
    ?is_string(HostName),
    ?is_int32(Port)
->
    [
        ?encode_int32(BrokerId),
        ?encode_string(HostName),
        ?encode_int32(Port)
    ];
encode_leader_and_isr_live_leader_0(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host_name => string,
        port => int32
    }).

-spec decode_leader_and_isr_live_leader_0(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_live_leader_0(),
    Rest :: binary().

decode_leader_and_isr_live_leader_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_string(HostName, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    {
        #{
            broker_id => BrokerId,
            host_name => HostName,
            port => Port
        },
        Bin3
    }.

-spec encode_leader_and_isr_partition_state_0(leader_and_isr_partition_state_0()) -> iodata().

encode_leader_and_isr_partition_state_0(
    _Args = #{
        % The topic name.  This is only present in v0 or v1.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker ID of the leader.
        leader := Leader,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition. The epoch is a monotonically increasing value which is incremented after every partition change. (Since the LeaderAndIsr request is only used by the legacy controller, this corresponds to the zkVersion)
        partition_epoch := PartitionEpoch,
        % The replica IDs.
        replicas := Replicas
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch),
    ?is_array(Replicas)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?encode_array(Replicas, ?encode_int32_)
    ];
encode_leader_and_isr_partition_state_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32,
        replicas => {array, int32}
    }).

-spec decode_leader_and_isr_partition_state_0(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_state_0(),
    Rest :: binary().

decode_leader_and_isr_partition_state_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int32(Leader, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?_decode_array(Isr, Bin5, Bin6, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin6, Bin7),
    ?_decode_array(Replicas, Bin7, Bin8, ?decode_int32_),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch,
            replicas => Replicas
        },
        Bin8
    }.

-spec encode_leader_and_isr_request_1(leader_and_isr_request_1()) -> iodata().

encode_leader_and_isr_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The current controller ID.
        controller_id := ControllerId,
        % The current controller epoch.
        controller_epoch := ControllerEpoch,
        % The state of each partition, in a v0 or v1 message.
        ungrouped_partition_states := UngroupedPartitionStates,
        % The current live leaders.
        live_leaders := LiveLeaders
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_array(UngroupedPartitionStates),
    ?is_array(LiveLeaders)
->
    [
        ?encode_request_header_1(?LEADER_AND_ISR_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_array(UngroupedPartitionStates, fun encode_leader_and_isr_partition_state_1/1),
        ?encode_array(LiveLeaders, fun encode_leader_and_isr_live_leader_1/1)
    ];
encode_leader_and_isr_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        ungrouped_partition_states => {array, leader_and_isr_partition_state_1},
        live_leaders => {array, leader_and_isr_live_leader_1}
    }).

-spec decode_leader_and_isr_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_request_1(),
    Rest :: binary().

decode_leader_and_isr_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_array(UngroupedPartitionStates, Bin2, Bin3, ?_decode_element(decode_leader_and_isr_partition_state_1)),
    ?_decode_array(LiveLeaders, Bin3, Bin4, ?_decode_element(decode_leader_and_isr_live_leader_1)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            ungrouped_partition_states => UngroupedPartitionStates,
            live_leaders => LiveLeaders
        },
        Bin4
    }.

-spec encode_leader_and_isr_live_leader_1(leader_and_isr_live_leader_1()) -> iodata().

encode_leader_and_isr_live_leader_1(
    _Args = #{
        % The leader's broker ID.
        broker_id := BrokerId,
        % The leader's hostname.
        host_name := HostName,
        % The leader's port.
        port := Port
    }
) when
    ?is_int32(BrokerId),
    ?is_string(HostName),
    ?is_int32(Port)
->
    [
        ?encode_int32(BrokerId),
        ?encode_string(HostName),
        ?encode_int32(Port)
    ];
encode_leader_and_isr_live_leader_1(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host_name => string,
        port => int32
    }).

-spec decode_leader_and_isr_live_leader_1(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_live_leader_1(),
    Rest :: binary().

decode_leader_and_isr_live_leader_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_string(HostName, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    {
        #{
            broker_id => BrokerId,
            host_name => HostName,
            port => Port
        },
        Bin3
    }.

-spec encode_leader_and_isr_partition_state_1(leader_and_isr_partition_state_1()) -> iodata().

encode_leader_and_isr_partition_state_1(
    _Args = #{
        % The topic name.  This is only present in v0 or v1.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker ID of the leader.
        leader := Leader,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition. The epoch is a monotonically increasing value which is incremented after every partition change. (Since the LeaderAndIsr request is only used by the legacy controller, this corresponds to the zkVersion)
        partition_epoch := PartitionEpoch,
        % The replica IDs.
        replicas := Replicas,
        % Whether the replica should have existed on the broker or not.
        is_new := IsNew
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch),
    ?is_array(Replicas),
    ?is_bool(IsNew)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?encode_array(Replicas, ?encode_int32_),
        ?encode_bool(IsNew)
    ];
encode_leader_and_isr_partition_state_1(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32,
        replicas => {array, int32},
        is_new => bool
    }).

-spec decode_leader_and_isr_partition_state_1(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_state_1(),
    Rest :: binary().

decode_leader_and_isr_partition_state_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int32(Leader, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?_decode_array(Isr, Bin5, Bin6, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin6, Bin7),
    ?_decode_array(Replicas, Bin7, Bin8, ?decode_int32_),
    ?_decode_bool(IsNew, Bin8, Bin9),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch,
            replicas => Replicas,
            is_new => IsNew
        },
        Bin9
    }.

-spec encode_leader_and_isr_request_2(leader_and_isr_request_2()) -> iodata().

encode_leader_and_isr_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The current controller ID.
        controller_id := ControllerId,
        % The current controller epoch.
        controller_epoch := ControllerEpoch,
        % The current broker epoch.
        broker_epoch := BrokerEpoch,
        % Each topic.
        topic_states := TopicStates,
        % The current live leaders.
        live_leaders := LiveLeaders
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates),
    ?is_array(LiveLeaders)
->
    [
        ?encode_request_header_1(?LEADER_AND_ISR_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_array(TopicStates, fun encode_leader_and_isr_topic_state_2/1),
        ?encode_array(LiveLeaders, fun encode_leader_and_isr_live_leader_2/1)
    ];
encode_leader_and_isr_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, leader_and_isr_topic_state_2},
        live_leaders => {array, leader_and_isr_live_leader_2}
    }).

-spec decode_leader_and_isr_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_request_2(),
    Rest :: binary().

decode_leader_and_isr_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_array(TopicStates, Bin3, Bin4, ?_decode_element(decode_leader_and_isr_topic_state_2)),
    ?_decode_array(LiveLeaders, Bin4, Bin5, ?_decode_element(decode_leader_and_isr_live_leader_2)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates,
            live_leaders => LiveLeaders
        },
        Bin5
    }.

-spec encode_leader_and_isr_topic_state_2(leader_and_isr_topic_state_2()) -> iodata().

encode_leader_and_isr_topic_state_2(
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
        ?encode_string(TopicName),
        ?encode_array(PartitionStates, fun encode_leader_and_isr_partition_state_2/1)
    ];
encode_leader_and_isr_topic_state_2(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_states => {array, leader_and_isr_partition_state_2}
    }).

-spec decode_leader_and_isr_topic_state_2(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_state_2(),
    Rest :: binary().

decode_leader_and_isr_topic_state_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_array(PartitionStates, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_state_2)),
    {
        #{
            topic_name => TopicName,
            partition_states => PartitionStates
        },
        Bin2
    }.

-spec encode_leader_and_isr_live_leader_2(leader_and_isr_live_leader_2()) -> iodata().

encode_leader_and_isr_live_leader_2(
    _Args = #{
        % The leader's broker ID.
        broker_id := BrokerId,
        % The leader's hostname.
        host_name := HostName,
        % The leader's port.
        port := Port
    }
) when
    ?is_int32(BrokerId),
    ?is_string(HostName),
    ?is_int32(Port)
->
    [
        ?encode_int32(BrokerId),
        ?encode_string(HostName),
        ?encode_int32(Port)
    ];
encode_leader_and_isr_live_leader_2(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host_name => string,
        port => int32
    }).

-spec decode_leader_and_isr_live_leader_2(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_live_leader_2(),
    Rest :: binary().

decode_leader_and_isr_live_leader_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_string(HostName, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    {
        #{
            broker_id => BrokerId,
            host_name => HostName,
            port => Port
        },
        Bin3
    }.

-spec encode_leader_and_isr_partition_state_2(leader_and_isr_partition_state_2()) -> iodata().

encode_leader_and_isr_partition_state_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker ID of the leader.
        leader := Leader,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition. The epoch is a monotonically increasing value which is incremented after every partition change. (Since the LeaderAndIsr request is only used by the legacy controller, this corresponds to the zkVersion)
        partition_epoch := PartitionEpoch,
        % The replica IDs.
        replicas := Replicas,
        % Whether the replica should have existed on the broker or not.
        is_new := IsNew
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch),
    ?is_array(Replicas),
    ?is_bool(IsNew)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?encode_array(Replicas, ?encode_int32_),
        ?encode_bool(IsNew)
    ];
encode_leader_and_isr_partition_state_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32,
        replicas => {array, int32},
        is_new => bool
    }).

-spec decode_leader_and_isr_partition_state_2(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_state_2(),
    Rest :: binary().

decode_leader_and_isr_partition_state_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin5, Bin6),
    ?_decode_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_bool(IsNew, Bin7, Bin8),
    {
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch,
            replicas => Replicas,
            is_new => IsNew
        },
        Bin8
    }.

-spec encode_leader_and_isr_request_3(leader_and_isr_request_3()) -> iodata().

encode_leader_and_isr_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The current controller ID.
        controller_id := ControllerId,
        % The current controller epoch.
        controller_epoch := ControllerEpoch,
        % The current broker epoch.
        broker_epoch := BrokerEpoch,
        % Each topic.
        topic_states := TopicStates,
        % The current live leaders.
        live_leaders := LiveLeaders
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates),
    ?is_array(LiveLeaders)
->
    [
        ?encode_request_header_1(?LEADER_AND_ISR_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_array(TopicStates, fun encode_leader_and_isr_topic_state_3/1),
        ?encode_array(LiveLeaders, fun encode_leader_and_isr_live_leader_3/1)
    ];
encode_leader_and_isr_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, leader_and_isr_topic_state_3},
        live_leaders => {array, leader_and_isr_live_leader_3}
    }).

-spec decode_leader_and_isr_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_request_3(),
    Rest :: binary().

decode_leader_and_isr_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_array(TopicStates, Bin3, Bin4, ?_decode_element(decode_leader_and_isr_topic_state_3)),
    ?_decode_array(LiveLeaders, Bin4, Bin5, ?_decode_element(decode_leader_and_isr_live_leader_3)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates,
            live_leaders => LiveLeaders
        },
        Bin5
    }.

-spec encode_leader_and_isr_topic_state_3(leader_and_isr_topic_state_3()) -> iodata().

encode_leader_and_isr_topic_state_3(
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
        ?encode_string(TopicName),
        ?encode_array(PartitionStates, fun encode_leader_and_isr_partition_state_3/1)
    ];
encode_leader_and_isr_topic_state_3(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_states => {array, leader_and_isr_partition_state_3}
    }).

-spec decode_leader_and_isr_topic_state_3(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_state_3(),
    Rest :: binary().

decode_leader_and_isr_topic_state_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_array(PartitionStates, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_state_3)),
    {
        #{
            topic_name => TopicName,
            partition_states => PartitionStates
        },
        Bin2
    }.

-spec encode_leader_and_isr_live_leader_3(leader_and_isr_live_leader_3()) -> iodata().

encode_leader_and_isr_live_leader_3(
    _Args = #{
        % The leader's broker ID.
        broker_id := BrokerId,
        % The leader's hostname.
        host_name := HostName,
        % The leader's port.
        port := Port
    }
) when
    ?is_int32(BrokerId),
    ?is_string(HostName),
    ?is_int32(Port)
->
    [
        ?encode_int32(BrokerId),
        ?encode_string(HostName),
        ?encode_int32(Port)
    ];
encode_leader_and_isr_live_leader_3(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host_name => string,
        port => int32
    }).

-spec decode_leader_and_isr_live_leader_3(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_live_leader_3(),
    Rest :: binary().

decode_leader_and_isr_live_leader_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_string(HostName, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    {
        #{
            broker_id => BrokerId,
            host_name => HostName,
            port => Port
        },
        Bin3
    }.

-spec encode_leader_and_isr_partition_state_3(leader_and_isr_partition_state_3()) -> iodata().

encode_leader_and_isr_partition_state_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker ID of the leader.
        leader := Leader,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition. The epoch is a monotonically increasing value which is incremented after every partition change. (Since the LeaderAndIsr request is only used by the legacy controller, this corresponds to the zkVersion)
        partition_epoch := PartitionEpoch,
        % The replica IDs.
        replicas := Replicas,
        % The replica IDs that we are adding this partition to, or null if no replicas are being added.
        adding_replicas := AddingReplicas,
        % The replica IDs that we are removing this partition from, or null if no replicas are being removed.
        removing_replicas := RemovingReplicas,
        % Whether the replica should have existed on the broker or not.
        is_new := IsNew
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch),
    ?is_array(Replicas),
    ?is_array(AddingReplicas),
    ?is_array(RemovingReplicas),
    ?is_bool(IsNew)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?encode_array(Replicas, ?encode_int32_),
        ?encode_array(AddingReplicas, ?encode_int32_),
        ?encode_array(RemovingReplicas, ?encode_int32_),
        ?encode_bool(IsNew)
    ];
encode_leader_and_isr_partition_state_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32,
        replicas => {array, int32},
        adding_replicas => {array, int32},
        removing_replicas => {array, int32},
        is_new => bool
    }).

-spec decode_leader_and_isr_partition_state_3(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_state_3(),
    Rest :: binary().

decode_leader_and_isr_partition_state_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin5, Bin6),
    ?_decode_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_array(AddingReplicas, Bin7, Bin8, ?decode_int32_),
    ?_decode_array(RemovingReplicas, Bin8, Bin9, ?decode_int32_),
    ?_decode_bool(IsNew, Bin9, Bin10),
    {
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch,
            replicas => Replicas,
            adding_replicas => AddingReplicas,
            removing_replicas => RemovingReplicas,
            is_new => IsNew
        },
        Bin10
    }.

-spec encode_leader_and_isr_request_4(leader_and_isr_request_4()) -> iodata().

encode_leader_and_isr_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The current controller ID.
        controller_id := ControllerId,
        % The current controller epoch.
        controller_epoch := ControllerEpoch,
        % The current broker epoch.
        broker_epoch := BrokerEpoch,
        % Each topic.
        topic_states := TopicStates,
        % The current live leaders.
        live_leaders := LiveLeaders
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates),
    ?is_array(LiveLeaders)
->
    [
        ?encode_request_header_2(?LEADER_AND_ISR_REQUEST, 4, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(TopicStates, fun encode_leader_and_isr_topic_state_4/1),
        ?encode_compact_array(LiveLeaders, fun encode_leader_and_isr_live_leader_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, leader_and_isr_topic_state_4},
        live_leaders => {array, leader_and_isr_live_leader_4}
    }).

-spec decode_leader_and_isr_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_request_4(),
    Rest :: binary().

decode_leader_and_isr_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_compact_array(TopicStates, Bin3, Bin4, ?_decode_element(decode_leader_and_isr_topic_state_4)),
    ?_decode_compact_array(LiveLeaders, Bin4, Bin5, ?_decode_element(decode_leader_and_isr_live_leader_4)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_request_4_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates,
            live_leaders => LiveLeaders
        },
        Bin5
    ).

-spec decode_leader_and_isr_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_request_4().

decode_leader_and_isr_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_topic_state_4(leader_and_isr_topic_state_4()) -> iodata().

encode_leader_and_isr_topic_state_4(
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
        ?encode_compact_array(PartitionStates, fun encode_leader_and_isr_partition_state_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_topic_state_4(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_states => {array, leader_and_isr_partition_state_4}
    }).

-spec decode_leader_and_isr_topic_state_4(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_state_4(),
    Rest :: binary().

decode_leader_and_isr_topic_state_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_compact_array(PartitionStates, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_state_4)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_topic_state_4_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_states => PartitionStates
        },
        Bin2
    ).

-spec decode_leader_and_isr_topic_state_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_topic_state_4().

decode_leader_and_isr_topic_state_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_live_leader_4(leader_and_isr_live_leader_4()) -> iodata().

encode_leader_and_isr_live_leader_4(
    _Args = #{
        % The leader's broker ID.
        broker_id := BrokerId,
        % The leader's hostname.
        host_name := HostName,
        % The leader's port.
        port := Port
    }
) when
    ?is_int32(BrokerId),
    ?is_string(HostName),
    ?is_int32(Port)
->
    [
        ?encode_int32(BrokerId),
        ?encode_compact_string(HostName),
        ?encode_int32(Port),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_live_leader_4(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host_name => string,
        port => int32
    }).

-spec decode_leader_and_isr_live_leader_4(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_live_leader_4(),
    Rest :: binary().

decode_leader_and_isr_live_leader_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(HostName, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_live_leader_4_tagged_field/3,
        #{
            broker_id => BrokerId,
            host_name => HostName,
            port => Port
        },
        Bin3
    ).

-spec decode_leader_and_isr_live_leader_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_live_leader_4().

decode_leader_and_isr_live_leader_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_partition_state_4(leader_and_isr_partition_state_4()) -> iodata().

encode_leader_and_isr_partition_state_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker ID of the leader.
        leader := Leader,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition. The epoch is a monotonically increasing value which is incremented after every partition change. (Since the LeaderAndIsr request is only used by the legacy controller, this corresponds to the zkVersion)
        partition_epoch := PartitionEpoch,
        % The replica IDs.
        replicas := Replicas,
        % The replica IDs that we are adding this partition to, or null if no replicas are being added.
        adding_replicas := AddingReplicas,
        % The replica IDs that we are removing this partition from, or null if no replicas are being removed.
        removing_replicas := RemovingReplicas,
        % Whether the replica should have existed on the broker or not.
        is_new := IsNew
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch),
    ?is_array(Replicas),
    ?is_array(AddingReplicas),
    ?is_array(RemovingReplicas),
    ?is_bool(IsNew)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?encode_compact_array(Replicas, ?encode_int32_),
        ?encode_compact_array(AddingReplicas, ?encode_int32_),
        ?encode_compact_array(RemovingReplicas, ?encode_int32_),
        ?encode_bool(IsNew),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_partition_state_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32,
        replicas => {array, int32},
        adding_replicas => {array, int32},
        removing_replicas => {array, int32},
        is_new => bool
    }).

-spec decode_leader_and_isr_partition_state_4(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_state_4(),
    Rest :: binary().

decode_leader_and_isr_partition_state_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin5, Bin6),
    ?_decode_compact_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_compact_array(AddingReplicas, Bin7, Bin8, ?decode_int32_),
    ?_decode_compact_array(RemovingReplicas, Bin8, Bin9, ?decode_int32_),
    ?_decode_bool(IsNew, Bin9, Bin10),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_partition_state_4_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch,
            replicas => Replicas,
            adding_replicas => AddingReplicas,
            removing_replicas => RemovingReplicas,
            is_new => IsNew
        },
        Bin10
    ).

-spec decode_leader_and_isr_partition_state_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_partition_state_4().

decode_leader_and_isr_partition_state_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_request_5(leader_and_isr_request_5()) -> iodata().

encode_leader_and_isr_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The current controller ID.
        controller_id := ControllerId,
        % The current controller epoch.
        controller_epoch := ControllerEpoch,
        % The current broker epoch.
        broker_epoch := BrokerEpoch,
        % The type that indicates whether all topics are included in the request
        type := Type,
        % Each topic.
        topic_states := TopicStates,
        % The current live leaders.
        live_leaders := LiveLeaders
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_int8(Type),
    ?is_array(TopicStates),
    ?is_array(LiveLeaders)
->
    [
        ?encode_request_header_2(?LEADER_AND_ISR_REQUEST, 5, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_int8(Type),
        ?encode_compact_array(TopicStates, fun encode_leader_and_isr_topic_state_5/1),
        ?encode_compact_array(LiveLeaders, fun encode_leader_and_isr_live_leader_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        type => int8,
        topic_states => {array, leader_and_isr_topic_state_5},
        live_leaders => {array, leader_and_isr_live_leader_5}
    }).

-spec decode_leader_and_isr_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_request_5(),
    Rest :: binary().

decode_leader_and_isr_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_int8(Type, Bin3, Bin4),
    ?_decode_compact_array(TopicStates, Bin4, Bin5, ?_decode_element(decode_leader_and_isr_topic_state_5)),
    ?_decode_compact_array(LiveLeaders, Bin5, Bin6, ?_decode_element(decode_leader_and_isr_live_leader_5)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_request_5_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            type => Type,
            topic_states => TopicStates,
            live_leaders => LiveLeaders
        },
        Bin6
    ).

-spec decode_leader_and_isr_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_request_5().

decode_leader_and_isr_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_topic_state_5(leader_and_isr_topic_state_5()) -> iodata().

encode_leader_and_isr_topic_state_5(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The unique topic ID.
        topic_id := TopicId,
        % The state of each partition
        partition_states := PartitionStates
    }
) when
    ?is_string(TopicName),
    ?is_uuid(TopicId),
    ?is_array(PartitionStates)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_uuid(TopicId),
        ?encode_compact_array(PartitionStates, fun encode_leader_and_isr_partition_state_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_topic_state_5(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        topic_id => uuid,
        partition_states => {array, leader_and_isr_partition_state_5}
    }).

-spec decode_leader_and_isr_topic_state_5(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_state_5(),
    Rest :: binary().

decode_leader_and_isr_topic_state_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_uuid(TopicId, Bin1, Bin2),
    ?_decode_compact_array(PartitionStates, Bin2, Bin3, ?_decode_element(decode_leader_and_isr_partition_state_5)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_topic_state_5_tagged_field/3,
        #{
            topic_name => TopicName,
            topic_id => TopicId,
            partition_states => PartitionStates
        },
        Bin3
    ).

-spec decode_leader_and_isr_topic_state_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_topic_state_5().

decode_leader_and_isr_topic_state_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_live_leader_5(leader_and_isr_live_leader_5()) -> iodata().

encode_leader_and_isr_live_leader_5(
    _Args = #{
        % The leader's broker ID.
        broker_id := BrokerId,
        % The leader's hostname.
        host_name := HostName,
        % The leader's port.
        port := Port
    }
) when
    ?is_int32(BrokerId),
    ?is_string(HostName),
    ?is_int32(Port)
->
    [
        ?encode_int32(BrokerId),
        ?encode_compact_string(HostName),
        ?encode_int32(Port),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_live_leader_5(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host_name => string,
        port => int32
    }).

-spec decode_leader_and_isr_live_leader_5(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_live_leader_5(),
    Rest :: binary().

decode_leader_and_isr_live_leader_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(HostName, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_live_leader_5_tagged_field/3,
        #{
            broker_id => BrokerId,
            host_name => HostName,
            port => Port
        },
        Bin3
    ).

-spec decode_leader_and_isr_live_leader_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_live_leader_5().

decode_leader_and_isr_live_leader_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_partition_state_5(leader_and_isr_partition_state_5()) -> iodata().

encode_leader_and_isr_partition_state_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker ID of the leader.
        leader := Leader,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition. The epoch is a monotonically increasing value which is incremented after every partition change. (Since the LeaderAndIsr request is only used by the legacy controller, this corresponds to the zkVersion)
        partition_epoch := PartitionEpoch,
        % The replica IDs.
        replicas := Replicas,
        % The replica IDs that we are adding this partition to, or null if no replicas are being added.
        adding_replicas := AddingReplicas,
        % The replica IDs that we are removing this partition from, or null if no replicas are being removed.
        removing_replicas := RemovingReplicas,
        % Whether the replica should have existed on the broker or not.
        is_new := IsNew
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch),
    ?is_array(Replicas),
    ?is_array(AddingReplicas),
    ?is_array(RemovingReplicas),
    ?is_bool(IsNew)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?encode_compact_array(Replicas, ?encode_int32_),
        ?encode_compact_array(AddingReplicas, ?encode_int32_),
        ?encode_compact_array(RemovingReplicas, ?encode_int32_),
        ?encode_bool(IsNew),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_partition_state_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32,
        replicas => {array, int32},
        adding_replicas => {array, int32},
        removing_replicas => {array, int32},
        is_new => bool
    }).

-spec decode_leader_and_isr_partition_state_5(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_state_5(),
    Rest :: binary().

decode_leader_and_isr_partition_state_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin5, Bin6),
    ?_decode_compact_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_compact_array(AddingReplicas, Bin7, Bin8, ?decode_int32_),
    ?_decode_compact_array(RemovingReplicas, Bin8, Bin9, ?decode_int32_),
    ?_decode_bool(IsNew, Bin9, Bin10),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_partition_state_5_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch,
            replicas => Replicas,
            adding_replicas => AddingReplicas,
            removing_replicas => RemovingReplicas,
            is_new => IsNew
        },
        Bin10
    ).

-spec decode_leader_and_isr_partition_state_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_partition_state_5().

decode_leader_and_isr_partition_state_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_request_6(leader_and_isr_request_6()) -> iodata().

encode_leader_and_isr_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The current controller ID.
        controller_id := ControllerId,
        % The current controller epoch.
        controller_epoch := ControllerEpoch,
        % The current broker epoch.
        broker_epoch := BrokerEpoch,
        % The type that indicates whether all topics are included in the request
        type := Type,
        % Each topic.
        topic_states := TopicStates,
        % The current live leaders.
        live_leaders := LiveLeaders
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_int8(Type),
    ?is_array(TopicStates),
    ?is_array(LiveLeaders)
->
    [
        ?encode_request_header_2(?LEADER_AND_ISR_REQUEST, 6, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_int8(Type),
        ?encode_compact_array(TopicStates, fun encode_leader_and_isr_topic_state_6/1),
        ?encode_compact_array(LiveLeaders, fun encode_leader_and_isr_live_leader_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        type => int8,
        topic_states => {array, leader_and_isr_topic_state_6},
        live_leaders => {array, leader_and_isr_live_leader_6}
    }).

-spec decode_leader_and_isr_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_request_6(),
    Rest :: binary().

decode_leader_and_isr_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_int8(Type, Bin3, Bin4),
    ?_decode_compact_array(TopicStates, Bin4, Bin5, ?_decode_element(decode_leader_and_isr_topic_state_6)),
    ?_decode_compact_array(LiveLeaders, Bin5, Bin6, ?_decode_element(decode_leader_and_isr_live_leader_6)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_request_6_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            type => Type,
            topic_states => TopicStates,
            live_leaders => LiveLeaders
        },
        Bin6
    ).

-spec decode_leader_and_isr_request_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_request_6().

decode_leader_and_isr_request_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_topic_state_6(leader_and_isr_topic_state_6()) -> iodata().

encode_leader_and_isr_topic_state_6(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The unique topic ID.
        topic_id := TopicId,
        % The state of each partition
        partition_states := PartitionStates
    }
) when
    ?is_string(TopicName),
    ?is_uuid(TopicId),
    ?is_array(PartitionStates)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_uuid(TopicId),
        ?encode_compact_array(PartitionStates, fun encode_leader_and_isr_partition_state_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_topic_state_6(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        topic_id => uuid,
        partition_states => {array, leader_and_isr_partition_state_6}
    }).

-spec decode_leader_and_isr_topic_state_6(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_state_6(),
    Rest :: binary().

decode_leader_and_isr_topic_state_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_uuid(TopicId, Bin1, Bin2),
    ?_decode_compact_array(PartitionStates, Bin2, Bin3, ?_decode_element(decode_leader_and_isr_partition_state_6)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_topic_state_6_tagged_field/3,
        #{
            topic_name => TopicName,
            topic_id => TopicId,
            partition_states => PartitionStates
        },
        Bin3
    ).

-spec decode_leader_and_isr_topic_state_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_topic_state_6().

decode_leader_and_isr_topic_state_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_live_leader_6(leader_and_isr_live_leader_6()) -> iodata().

encode_leader_and_isr_live_leader_6(
    _Args = #{
        % The leader's broker ID.
        broker_id := BrokerId,
        % The leader's hostname.
        host_name := HostName,
        % The leader's port.
        port := Port
    }
) when
    ?is_int32(BrokerId),
    ?is_string(HostName),
    ?is_int32(Port)
->
    [
        ?encode_int32(BrokerId),
        ?encode_compact_string(HostName),
        ?encode_int32(Port),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_live_leader_6(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host_name => string,
        port => int32
    }).

-spec decode_leader_and_isr_live_leader_6(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_live_leader_6(),
    Rest :: binary().

decode_leader_and_isr_live_leader_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(HostName, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_live_leader_6_tagged_field/3,
        #{
            broker_id => BrokerId,
            host_name => HostName,
            port => Port
        },
        Bin3
    ).

-spec decode_leader_and_isr_live_leader_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_live_leader_6().

decode_leader_and_isr_live_leader_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_partition_state_6(leader_and_isr_partition_state_6()) -> iodata().

encode_leader_and_isr_partition_state_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker ID of the leader.
        leader := Leader,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition. The epoch is a monotonically increasing value which is incremented after every partition change. (Since the LeaderAndIsr request is only used by the legacy controller, this corresponds to the zkVersion)
        partition_epoch := PartitionEpoch,
        % The replica IDs.
        replicas := Replicas,
        % The replica IDs that we are adding this partition to, or null if no replicas are being added.
        adding_replicas := AddingReplicas,
        % The replica IDs that we are removing this partition from, or null if no replicas are being removed.
        removing_replicas := RemovingReplicas,
        % Whether the replica should have existed on the broker or not.
        is_new := IsNew,
        % 1 if the partition is recovering from an unclean leader election; 0 otherwise.
        leader_recovery_state := LeaderRecoveryState
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch),
    ?is_array(Replicas),
    ?is_array(AddingReplicas),
    ?is_array(RemovingReplicas),
    ?is_bool(IsNew),
    ?is_int8(LeaderRecoveryState)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?encode_compact_array(Replicas, ?encode_int32_),
        ?encode_compact_array(AddingReplicas, ?encode_int32_),
        ?encode_compact_array(RemovingReplicas, ?encode_int32_),
        ?encode_bool(IsNew),
        ?encode_int8(LeaderRecoveryState),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_partition_state_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32,
        replicas => {array, int32},
        adding_replicas => {array, int32},
        removing_replicas => {array, int32},
        is_new => bool,
        leader_recovery_state => int8
    }).

-spec decode_leader_and_isr_partition_state_6(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_state_6(),
    Rest :: binary().

decode_leader_and_isr_partition_state_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin5, Bin6),
    ?_decode_compact_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_compact_array(AddingReplicas, Bin7, Bin8, ?decode_int32_),
    ?_decode_compact_array(RemovingReplicas, Bin8, Bin9, ?decode_int32_),
    ?_decode_bool(IsNew, Bin9, Bin10),
    ?_decode_int8(LeaderRecoveryState, Bin10, Bin11),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_partition_state_6_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch,
            replicas => Replicas,
            adding_replicas => AddingReplicas,
            removing_replicas => RemovingReplicas,
            is_new => IsNew,
            leader_recovery_state => LeaderRecoveryState
        },
        Bin11
    ).

-spec decode_leader_and_isr_partition_state_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_partition_state_6().

decode_leader_and_isr_partition_state_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_request_7(leader_and_isr_request_7()) -> iodata().

encode_leader_and_isr_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The current controller ID.
        controller_id := ControllerId,
        % If KRaft controller id is used during migration. See KIP-866
        is_k_raft_controller := IsKRaftController,
        % The current controller epoch.
        controller_epoch := ControllerEpoch,
        % The current broker epoch.
        broker_epoch := BrokerEpoch,
        % The type that indicates whether all topics are included in the request
        type := Type,
        % Each topic.
        topic_states := TopicStates,
        % The current live leaders.
        live_leaders := LiveLeaders
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_bool(IsKRaftController),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_int8(Type),
    ?is_array(TopicStates),
    ?is_array(LiveLeaders)
->
    [
        ?encode_request_header_2(?LEADER_AND_ISR_REQUEST, 7, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_bool(IsKRaftController),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_int8(Type),
        ?encode_compact_array(TopicStates, fun encode_leader_and_isr_topic_state_7/1),
        ?encode_compact_array(LiveLeaders, fun encode_leader_and_isr_live_leader_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        is_k_raft_controller => bool,
        controller_epoch => int32,
        broker_epoch => int64,
        type => int8,
        topic_states => {array, leader_and_isr_topic_state_7},
        live_leaders => {array, leader_and_isr_live_leader_7}
    }).

-spec decode_leader_and_isr_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_request_7(),
    Rest :: binary().

decode_leader_and_isr_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_bool(IsKRaftController, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int64(BrokerEpoch, Bin3, Bin4),
    ?_decode_int8(Type, Bin4, Bin5),
    ?_decode_compact_array(TopicStates, Bin5, Bin6, ?_decode_element(decode_leader_and_isr_topic_state_7)),
    ?_decode_compact_array(LiveLeaders, Bin6, Bin7, ?_decode_element(decode_leader_and_isr_live_leader_7)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_request_7_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            is_k_raft_controller => IsKRaftController,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            type => Type,
            topic_states => TopicStates,
            live_leaders => LiveLeaders
        },
        Bin7
    ).

-spec decode_leader_and_isr_request_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_request_7().

decode_leader_and_isr_request_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_topic_state_7(leader_and_isr_topic_state_7()) -> iodata().

encode_leader_and_isr_topic_state_7(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The unique topic ID.
        topic_id := TopicId,
        % The state of each partition
        partition_states := PartitionStates
    }
) when
    ?is_string(TopicName),
    ?is_uuid(TopicId),
    ?is_array(PartitionStates)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_uuid(TopicId),
        ?encode_compact_array(PartitionStates, fun encode_leader_and_isr_partition_state_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_topic_state_7(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        topic_id => uuid,
        partition_states => {array, leader_and_isr_partition_state_7}
    }).

-spec decode_leader_and_isr_topic_state_7(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_state_7(),
    Rest :: binary().

decode_leader_and_isr_topic_state_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_uuid(TopicId, Bin1, Bin2),
    ?_decode_compact_array(PartitionStates, Bin2, Bin3, ?_decode_element(decode_leader_and_isr_partition_state_7)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_topic_state_7_tagged_field/3,
        #{
            topic_name => TopicName,
            topic_id => TopicId,
            partition_states => PartitionStates
        },
        Bin3
    ).

-spec decode_leader_and_isr_topic_state_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_topic_state_7().

decode_leader_and_isr_topic_state_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_live_leader_7(leader_and_isr_live_leader_7()) -> iodata().

encode_leader_and_isr_live_leader_7(
    _Args = #{
        % The leader's broker ID.
        broker_id := BrokerId,
        % The leader's hostname.
        host_name := HostName,
        % The leader's port.
        port := Port
    }
) when
    ?is_int32(BrokerId),
    ?is_string(HostName),
    ?is_int32(Port)
->
    [
        ?encode_int32(BrokerId),
        ?encode_compact_string(HostName),
        ?encode_int32(Port),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_live_leader_7(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host_name => string,
        port => int32
    }).

-spec decode_leader_and_isr_live_leader_7(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_live_leader_7(),
    Rest :: binary().

decode_leader_and_isr_live_leader_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(HostName, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_live_leader_7_tagged_field/3,
        #{
            broker_id => BrokerId,
            host_name => HostName,
            port => Port
        },
        Bin3
    ).

-spec decode_leader_and_isr_live_leader_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_live_leader_7().

decode_leader_and_isr_live_leader_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_partition_state_7(leader_and_isr_partition_state_7()) -> iodata().

encode_leader_and_isr_partition_state_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The broker ID of the leader.
        leader := Leader,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition. The epoch is a monotonically increasing value which is incremented after every partition change. (Since the LeaderAndIsr request is only used by the legacy controller, this corresponds to the zkVersion)
        partition_epoch := PartitionEpoch,
        % The replica IDs.
        replicas := Replicas,
        % The replica IDs that we are adding this partition to, or null if no replicas are being added.
        adding_replicas := AddingReplicas,
        % The replica IDs that we are removing this partition from, or null if no replicas are being removed.
        removing_replicas := RemovingReplicas,
        % Whether the replica should have existed on the broker or not.
        is_new := IsNew,
        % 1 if the partition is recovering from an unclean leader election; 0 otherwise.
        leader_recovery_state := LeaderRecoveryState
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch),
    ?is_array(Replicas),
    ?is_array(AddingReplicas),
    ?is_array(RemovingReplicas),
    ?is_bool(IsNew),
    ?is_int8(LeaderRecoveryState)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?encode_compact_array(Replicas, ?encode_int32_),
        ?encode_compact_array(AddingReplicas, ?encode_int32_),
        ?encode_compact_array(RemovingReplicas, ?encode_int32_),
        ?encode_bool(IsNew),
        ?encode_int8(LeaderRecoveryState),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_partition_state_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32,
        replicas => {array, int32},
        adding_replicas => {array, int32},
        removing_replicas => {array, int32},
        is_new => bool,
        leader_recovery_state => int8
    }).

-spec decode_leader_and_isr_partition_state_7(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_state_7(),
    Rest :: binary().

decode_leader_and_isr_partition_state_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin5, Bin6),
    ?_decode_compact_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_compact_array(AddingReplicas, Bin7, Bin8, ?decode_int32_),
    ?_decode_compact_array(RemovingReplicas, Bin8, Bin9, ?decode_int32_),
    ?_decode_bool(IsNew, Bin9, Bin10),
    ?_decode_int8(LeaderRecoveryState, Bin10, Bin11),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_partition_state_7_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch,
            replicas => Replicas,
            adding_replicas => AddingReplicas,
            removing_replicas => RemovingReplicas,
            is_new => IsNew,
            leader_recovery_state => LeaderRecoveryState
        },
        Bin11
    ).

-spec decode_leader_and_isr_partition_state_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_partition_state_7().

decode_leader_and_isr_partition_state_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type leader_and_isr_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    ungrouped_partition_states := list(leader_and_isr_partition_state_0()),
    live_leaders := list(leader_and_isr_live_leader_0())
}.
-type leader_and_isr_live_leader_0() :: #{
    broker_id := integer(),
    host_name := binary(),
    port := integer()
}.
-type leader_and_isr_partition_state_0() :: #{
    topic_name := binary(),
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer(),
    replicas := list(integer())
}.
-type leader_and_isr_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    ungrouped_partition_states := list(leader_and_isr_partition_state_1()),
    live_leaders := list(leader_and_isr_live_leader_1())
}.
-type leader_and_isr_live_leader_1() :: #{
    broker_id := integer(),
    host_name := binary(),
    port := integer()
}.
-type leader_and_isr_partition_state_1() :: #{
    topic_name := binary(),
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer(),
    replicas := list(integer()),
    is_new := boolean()
}.
-type leader_and_isr_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(leader_and_isr_topic_state_2()),
    live_leaders := list(leader_and_isr_live_leader_2())
}.
-type leader_and_isr_topic_state_2() :: #{
    topic_name := binary(),
    partition_states := list(leader_and_isr_partition_state_2())
}.
-type leader_and_isr_live_leader_2() :: #{
    broker_id := integer(),
    host_name := binary(),
    port := integer()
}.
-type leader_and_isr_partition_state_2() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer(),
    replicas := list(integer()),
    is_new := boolean()
}.
-type leader_and_isr_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(leader_and_isr_topic_state_3()),
    live_leaders := list(leader_and_isr_live_leader_3())
}.
-type leader_and_isr_topic_state_3() :: #{
    topic_name := binary(),
    partition_states := list(leader_and_isr_partition_state_3())
}.
-type leader_and_isr_live_leader_3() :: #{
    broker_id := integer(),
    host_name := binary(),
    port := integer()
}.
-type leader_and_isr_partition_state_3() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer(),
    replicas := list(integer()),
    adding_replicas := list(integer()),
    removing_replicas := list(integer()),
    is_new := boolean()
}.
-type leader_and_isr_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(leader_and_isr_topic_state_4()),
    live_leaders := list(leader_and_isr_live_leader_4())
}.
-type leader_and_isr_topic_state_4() :: #{
    topic_name := binary(),
    partition_states := list(leader_and_isr_partition_state_4())
}.
-type leader_and_isr_live_leader_4() :: #{
    broker_id := integer(),
    host_name := binary(),
    port := integer()
}.
-type leader_and_isr_partition_state_4() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer(),
    replicas := list(integer()),
    adding_replicas := list(integer()),
    removing_replicas := list(integer()),
    is_new := boolean()
}.
-type leader_and_isr_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    type := integer(),
    topic_states := list(leader_and_isr_topic_state_5()),
    live_leaders := list(leader_and_isr_live_leader_5())
}.
-type leader_and_isr_topic_state_5() :: #{
    topic_name := binary(),
    topic_id := kafcod:uuid(),
    partition_states := list(leader_and_isr_partition_state_5())
}.
-type leader_and_isr_live_leader_5() :: #{
    broker_id := integer(),
    host_name := binary(),
    port := integer()
}.
-type leader_and_isr_partition_state_5() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer(),
    replicas := list(integer()),
    adding_replicas := list(integer()),
    removing_replicas := list(integer()),
    is_new := boolean()
}.
-type leader_and_isr_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    type := integer(),
    topic_states := list(leader_and_isr_topic_state_6()),
    live_leaders := list(leader_and_isr_live_leader_6())
}.
-type leader_and_isr_topic_state_6() :: #{
    topic_name := binary(),
    topic_id := kafcod:uuid(),
    partition_states := list(leader_and_isr_partition_state_6())
}.
-type leader_and_isr_live_leader_6() :: #{
    broker_id := integer(),
    host_name := binary(),
    port := integer()
}.
-type leader_and_isr_partition_state_6() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer(),
    replicas := list(integer()),
    adding_replicas := list(integer()),
    removing_replicas := list(integer()),
    is_new := boolean(),
    leader_recovery_state := integer()
}.
-type leader_and_isr_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    is_k_raft_controller := boolean(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    type := integer(),
    topic_states := list(leader_and_isr_topic_state_7()),
    live_leaders := list(leader_and_isr_live_leader_7())
}.
-type leader_and_isr_topic_state_7() :: #{
    topic_name := binary(),
    topic_id := kafcod:uuid(),
    partition_states := list(leader_and_isr_partition_state_7())
}.
-type leader_and_isr_live_leader_7() :: #{
    broker_id := integer(),
    host_name := binary(),
    port := integer()
}.
-type leader_and_isr_partition_state_7() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer(),
    replicas := list(integer()),
    adding_replicas := list(integer()),
    removing_replicas := list(integer()),
    is_new := boolean(),
    leader_recovery_state := integer()
}.
