-module(update_metadata_request).
-export([
    encode_update_metadata_request_0/1,
    decode_update_metadata_request_0/1,
    encode_update_metadata_request_1/1,
    decode_update_metadata_request_1/1,
    encode_update_metadata_request_2/1,
    decode_update_metadata_request_2/1,
    encode_update_metadata_request_3/1,
    decode_update_metadata_request_3/1,
    encode_update_metadata_request_4/1,
    decode_update_metadata_request_4/1,
    encode_update_metadata_request_5/1,
    decode_update_metadata_request_5/1,
    encode_update_metadata_request_6/1,
    decode_update_metadata_request_6/1,
    encode_update_metadata_request_7/1,
    decode_update_metadata_request_7/1,
    encode_update_metadata_request_8/1,
    decode_update_metadata_request_8/1
]).
-export_type([
    update_metadata_request_0/0,
    update_metadata_broker_0/0,
    update_metadata_partition_state_0/0,
    update_metadata_request_1/0,
    update_metadata_endpoint_1/0,
    update_metadata_broker_1/0,
    update_metadata_partition_state_1/0,
    update_metadata_request_2/0,
    update_metadata_endpoint_2/0,
    update_metadata_broker_2/0,
    update_metadata_partition_state_2/0,
    update_metadata_request_3/0,
    update_metadata_endpoint_3/0,
    update_metadata_broker_3/0,
    update_metadata_partition_state_3/0,
    update_metadata_request_4/0,
    update_metadata_endpoint_4/0,
    update_metadata_broker_4/0,
    update_metadata_partition_state_4/0,
    update_metadata_request_5/0,
    update_metadata_topic_state_5/0,
    update_metadata_endpoint_5/0,
    update_metadata_broker_5/0,
    update_metadata_partition_state_5/0,
    update_metadata_request_6/0,
    update_metadata_topic_state_6/0,
    update_metadata_endpoint_6/0,
    update_metadata_broker_6/0,
    update_metadata_partition_state_6/0,
    update_metadata_request_7/0,
    update_metadata_topic_state_7/0,
    update_metadata_endpoint_7/0,
    update_metadata_broker_7/0,
    update_metadata_partition_state_7/0,
    update_metadata_request_8/0,
    update_metadata_topic_state_8/0,
    update_metadata_endpoint_8/0,
    update_metadata_broker_8/0,
    update_metadata_partition_state_8/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(UPDATE_METADATA_REQUEST, 6).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_update_metadata_request_0(update_metadata_request_0()) -> iodata().

encode_update_metadata_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % In older versions of this RPC, each partition that we would like to update.
        ungrouped_partition_states := UngroupedPartitionStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_array(UngroupedPartitionStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_1(?UPDATE_METADATA_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_array(UngroupedPartitionStates, fun encode_update_metadata_partition_state_0/1),
        ?encode_array(LiveBrokers, fun encode_update_metadata_broker_0/1)
    ];
encode_update_metadata_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        ungrouped_partition_states => {array, update_metadata_partition_state_0},
        live_brokers => {array, update_metadata_broker_0}
    }).

-spec decode_update_metadata_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_0(),
    Rest :: binary().

decode_update_metadata_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_array(UngroupedPartitionStates, Bin2, Bin3, ?_decode_element(decode_update_metadata_partition_state_0)),
    ?_decode_array(LiveBrokers, Bin3, Bin4, ?_decode_element(decode_update_metadata_broker_0)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            ungrouped_partition_states => UngroupedPartitionStates,
            live_brokers => LiveBrokers
        },
        Bin4
    }.

-spec encode_update_metadata_broker_0(update_metadata_broker_0()) -> iodata().

encode_update_metadata_broker_0(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker hostname.
        v0_host := V0Host,
        % The broker port.
        v0_port := V0Port
    }
) when
    ?is_int32(Id),
    ?is_string(V0Host),
    ?is_int32(V0Port)
->
    [
        ?encode_int32(Id),
        ?encode_string(V0Host),
        ?encode_int32(V0Port)
    ];
encode_update_metadata_broker_0(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        v0_host => string,
        v0_port => int32
    }).

-spec decode_update_metadata_broker_0(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_0(),
    Rest :: binary().

decode_update_metadata_broker_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_string(V0Host, Bin1, Bin2),
    ?_decode_int32(V0Port, Bin2, Bin3),
    {
        #{
            id => Id,
            v0_host => V0Host,
            v0_port => V0Port
        },
        Bin3
    }.

-spec encode_update_metadata_partition_state_0(update_metadata_partition_state_0()) -> iodata().

encode_update_metadata_partition_state_0(
    _Args = #{
        % In older versions of this RPC, the topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_array(Replicas, ?encode_int32_)
    ];
encode_update_metadata_partition_state_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_0(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_0(),
    Rest :: binary().

decode_update_metadata_partition_state_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int32(Leader, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?_decode_array(Isr, Bin5, Bin6, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin6, Bin7),
    ?_decode_array(Replicas, Bin7, Bin8, ?decode_int32_),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas
        },
        Bin8
    }.

-spec encode_update_metadata_request_1(update_metadata_request_1()) -> iodata().

encode_update_metadata_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % In older versions of this RPC, each partition that we would like to update.
        ungrouped_partition_states := UngroupedPartitionStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_array(UngroupedPartitionStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_1(?UPDATE_METADATA_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_array(UngroupedPartitionStates, fun encode_update_metadata_partition_state_1/1),
        ?encode_array(LiveBrokers, fun encode_update_metadata_broker_1/1)
    ];
encode_update_metadata_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        ungrouped_partition_states => {array, update_metadata_partition_state_1},
        live_brokers => {array, update_metadata_broker_1}
    }).

-spec decode_update_metadata_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_1(),
    Rest :: binary().

decode_update_metadata_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_array(UngroupedPartitionStates, Bin2, Bin3, ?_decode_element(decode_update_metadata_partition_state_1)),
    ?_decode_array(LiveBrokers, Bin3, Bin4, ?_decode_element(decode_update_metadata_broker_1)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            ungrouped_partition_states => UngroupedPartitionStates,
            live_brokers => LiveBrokers
        },
        Bin4
    }.

-spec encode_update_metadata_endpoint_1(update_metadata_endpoint_1()) -> iodata().

encode_update_metadata_endpoint_1(
    _Args = #{
        % The port of this endpoint
        port := Port,
        % The hostname of this endpoint
        host := Host,
        % The security protocol type.
        security_protocol := SecurityProtocol
    }
) when
    ?is_int32(Port),
    ?is_string(Host),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_int32(Port),
        ?encode_string(Host),
        ?encode_int16(SecurityProtocol)
    ];
encode_update_metadata_endpoint_1(Args) ->
    ?encoder_error(Args, #{
        port => int32,
        host => string,
        security_protocol => int16
    }).

-spec decode_update_metadata_endpoint_1(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_endpoint_1(),
    Rest :: binary().

decode_update_metadata_endpoint_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Port, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int16(SecurityProtocol, Bin2, Bin3),
    {
        #{
            port => Port,
            host => Host,
            security_protocol => SecurityProtocol
        },
        Bin3
    }.

-spec encode_update_metadata_broker_1(update_metadata_broker_1()) -> iodata().

encode_update_metadata_broker_1(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker endpoints.
        endpoints := Endpoints
    }
) when
    ?is_int32(Id),
    ?is_array(Endpoints)
->
    [
        ?encode_int32(Id),
        ?encode_array(Endpoints, fun encode_update_metadata_endpoint_1/1)
    ];
encode_update_metadata_broker_1(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        endpoints => {array, update_metadata_endpoint_1}
    }).

-spec decode_update_metadata_broker_1(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_1(),
    Rest :: binary().

decode_update_metadata_broker_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_array(Endpoints, Bin1, Bin2, ?_decode_element(decode_update_metadata_endpoint_1)),
    {
        #{
            id => Id,
            endpoints => Endpoints
        },
        Bin2
    }.

-spec encode_update_metadata_partition_state_1(update_metadata_partition_state_1()) -> iodata().

encode_update_metadata_partition_state_1(
    _Args = #{
        % In older versions of this RPC, the topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_array(Replicas, ?encode_int32_)
    ];
encode_update_metadata_partition_state_1(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_1(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_1(),
    Rest :: binary().

decode_update_metadata_partition_state_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int32(Leader, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?_decode_array(Isr, Bin5, Bin6, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin6, Bin7),
    ?_decode_array(Replicas, Bin7, Bin8, ?decode_int32_),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas
        },
        Bin8
    }.

-spec encode_update_metadata_request_2(update_metadata_request_2()) -> iodata().

encode_update_metadata_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % In older versions of this RPC, each partition that we would like to update.
        ungrouped_partition_states := UngroupedPartitionStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_array(UngroupedPartitionStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_1(?UPDATE_METADATA_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_array(UngroupedPartitionStates, fun encode_update_metadata_partition_state_2/1),
        ?encode_array(LiveBrokers, fun encode_update_metadata_broker_2/1)
    ];
encode_update_metadata_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        ungrouped_partition_states => {array, update_metadata_partition_state_2},
        live_brokers => {array, update_metadata_broker_2}
    }).

-spec decode_update_metadata_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_2(),
    Rest :: binary().

decode_update_metadata_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_array(UngroupedPartitionStates, Bin2, Bin3, ?_decode_element(decode_update_metadata_partition_state_2)),
    ?_decode_array(LiveBrokers, Bin3, Bin4, ?_decode_element(decode_update_metadata_broker_2)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            ungrouped_partition_states => UngroupedPartitionStates,
            live_brokers => LiveBrokers
        },
        Bin4
    }.

-spec encode_update_metadata_endpoint_2(update_metadata_endpoint_2()) -> iodata().

encode_update_metadata_endpoint_2(
    _Args = #{
        % The port of this endpoint
        port := Port,
        % The hostname of this endpoint
        host := Host,
        % The security protocol type.
        security_protocol := SecurityProtocol
    }
) when
    ?is_int32(Port),
    ?is_string(Host),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_int32(Port),
        ?encode_string(Host),
        ?encode_int16(SecurityProtocol)
    ];
encode_update_metadata_endpoint_2(Args) ->
    ?encoder_error(Args, #{
        port => int32,
        host => string,
        security_protocol => int16
    }).

-spec decode_update_metadata_endpoint_2(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_endpoint_2(),
    Rest :: binary().

decode_update_metadata_endpoint_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Port, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int16(SecurityProtocol, Bin2, Bin3),
    {
        #{
            port => Port,
            host => Host,
            security_protocol => SecurityProtocol
        },
        Bin3
    }.

-spec encode_update_metadata_broker_2(update_metadata_broker_2()) -> iodata().

encode_update_metadata_broker_2(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker endpoints.
        endpoints := Endpoints,
        % The rack which this broker belongs to.
        rack := Rack
    }
) when
    ?is_int32(Id),
    ?is_array(Endpoints),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(Id),
        ?encode_array(Endpoints, fun encode_update_metadata_endpoint_2/1),
        ?encode_nullable_string(Rack)
    ];
encode_update_metadata_broker_2(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        endpoints => {array, update_metadata_endpoint_2},
        rack => nullable_string
    }).

-spec decode_update_metadata_broker_2(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_2(),
    Rest :: binary().

decode_update_metadata_broker_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_array(Endpoints, Bin1, Bin2, ?_decode_element(decode_update_metadata_endpoint_2)),
    ?_decode_nullable_string(Rack, Bin2, Bin3),
    {
        #{
            id => Id,
            endpoints => Endpoints,
            rack => Rack
        },
        Bin3
    }.

-spec encode_update_metadata_partition_state_2(update_metadata_partition_state_2()) -> iodata().

encode_update_metadata_partition_state_2(
    _Args = #{
        % In older versions of this RPC, the topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_array(Replicas, ?encode_int32_)
    ];
encode_update_metadata_partition_state_2(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_2(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_2(),
    Rest :: binary().

decode_update_metadata_partition_state_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int32(Leader, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?_decode_array(Isr, Bin5, Bin6, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin6, Bin7),
    ?_decode_array(Replicas, Bin7, Bin8, ?decode_int32_),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas
        },
        Bin8
    }.

-spec encode_update_metadata_request_3(update_metadata_request_3()) -> iodata().

encode_update_metadata_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % In older versions of this RPC, each partition that we would like to update.
        ungrouped_partition_states := UngroupedPartitionStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_array(UngroupedPartitionStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_1(?UPDATE_METADATA_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_array(UngroupedPartitionStates, fun encode_update_metadata_partition_state_3/1),
        ?encode_array(LiveBrokers, fun encode_update_metadata_broker_3/1)
    ];
encode_update_metadata_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        ungrouped_partition_states => {array, update_metadata_partition_state_3},
        live_brokers => {array, update_metadata_broker_3}
    }).

-spec decode_update_metadata_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_3(),
    Rest :: binary().

decode_update_metadata_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_array(UngroupedPartitionStates, Bin2, Bin3, ?_decode_element(decode_update_metadata_partition_state_3)),
    ?_decode_array(LiveBrokers, Bin3, Bin4, ?_decode_element(decode_update_metadata_broker_3)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            ungrouped_partition_states => UngroupedPartitionStates,
            live_brokers => LiveBrokers
        },
        Bin4
    }.

-spec encode_update_metadata_endpoint_3(update_metadata_endpoint_3()) -> iodata().

encode_update_metadata_endpoint_3(
    _Args = #{
        % The port of this endpoint
        port := Port,
        % The hostname of this endpoint
        host := Host,
        % The listener name.
        listener := Listener,
        % The security protocol type.
        security_protocol := SecurityProtocol
    }
) when
    ?is_int32(Port),
    ?is_string(Host),
    ?is_string(Listener),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_int32(Port),
        ?encode_string(Host),
        ?encode_string(Listener),
        ?encode_int16(SecurityProtocol)
    ];
encode_update_metadata_endpoint_3(Args) ->
    ?encoder_error(Args, #{
        port => int32,
        host => string,
        listener => string,
        security_protocol => int16
    }).

-spec decode_update_metadata_endpoint_3(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_endpoint_3(),
    Rest :: binary().

decode_update_metadata_endpoint_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Port, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_string(Listener, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    {
        #{
            port => Port,
            host => Host,
            listener => Listener,
            security_protocol => SecurityProtocol
        },
        Bin4
    }.

-spec encode_update_metadata_broker_3(update_metadata_broker_3()) -> iodata().

encode_update_metadata_broker_3(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker endpoints.
        endpoints := Endpoints,
        % The rack which this broker belongs to.
        rack := Rack
    }
) when
    ?is_int32(Id),
    ?is_array(Endpoints),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(Id),
        ?encode_array(Endpoints, fun encode_update_metadata_endpoint_3/1),
        ?encode_nullable_string(Rack)
    ];
encode_update_metadata_broker_3(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        endpoints => {array, update_metadata_endpoint_3},
        rack => nullable_string
    }).

-spec decode_update_metadata_broker_3(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_3(),
    Rest :: binary().

decode_update_metadata_broker_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_array(Endpoints, Bin1, Bin2, ?_decode_element(decode_update_metadata_endpoint_3)),
    ?_decode_nullable_string(Rack, Bin2, Bin3),
    {
        #{
            id => Id,
            endpoints => Endpoints,
            rack => Rack
        },
        Bin3
    }.

-spec encode_update_metadata_partition_state_3(update_metadata_partition_state_3()) -> iodata().

encode_update_metadata_partition_state_3(
    _Args = #{
        % In older versions of this RPC, the topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_array(Replicas, ?encode_int32_)
    ];
encode_update_metadata_partition_state_3(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_3(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_3(),
    Rest :: binary().

decode_update_metadata_partition_state_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int32(Leader, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?_decode_array(Isr, Bin5, Bin6, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin6, Bin7),
    ?_decode_array(Replicas, Bin7, Bin8, ?decode_int32_),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas
        },
        Bin8
    }.

-spec encode_update_metadata_request_4(update_metadata_request_4()) -> iodata().

encode_update_metadata_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The controller id.
        controller_id := ControllerId,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % In older versions of this RPC, each partition that we would like to update.
        ungrouped_partition_states := UngroupedPartitionStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_array(UngroupedPartitionStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_1(?UPDATE_METADATA_REQUEST, 4, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_array(UngroupedPartitionStates, fun encode_update_metadata_partition_state_4/1),
        ?encode_array(LiveBrokers, fun encode_update_metadata_broker_4/1)
    ];
encode_update_metadata_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        ungrouped_partition_states => {array, update_metadata_partition_state_4},
        live_brokers => {array, update_metadata_broker_4}
    }).

-spec decode_update_metadata_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_4(),
    Rest :: binary().

decode_update_metadata_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_array(UngroupedPartitionStates, Bin2, Bin3, ?_decode_element(decode_update_metadata_partition_state_4)),
    ?_decode_array(LiveBrokers, Bin3, Bin4, ?_decode_element(decode_update_metadata_broker_4)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            ungrouped_partition_states => UngroupedPartitionStates,
            live_brokers => LiveBrokers
        },
        Bin4
    }.

-spec encode_update_metadata_endpoint_4(update_metadata_endpoint_4()) -> iodata().

encode_update_metadata_endpoint_4(
    _Args = #{
        % The port of this endpoint
        port := Port,
        % The hostname of this endpoint
        host := Host,
        % The listener name.
        listener := Listener,
        % The security protocol type.
        security_protocol := SecurityProtocol
    }
) when
    ?is_int32(Port),
    ?is_string(Host),
    ?is_string(Listener),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_int32(Port),
        ?encode_string(Host),
        ?encode_string(Listener),
        ?encode_int16(SecurityProtocol)
    ];
encode_update_metadata_endpoint_4(Args) ->
    ?encoder_error(Args, #{
        port => int32,
        host => string,
        listener => string,
        security_protocol => int16
    }).

-spec decode_update_metadata_endpoint_4(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_endpoint_4(),
    Rest :: binary().

decode_update_metadata_endpoint_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Port, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_string(Listener, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    {
        #{
            port => Port,
            host => Host,
            listener => Listener,
            security_protocol => SecurityProtocol
        },
        Bin4
    }.

-spec encode_update_metadata_broker_4(update_metadata_broker_4()) -> iodata().

encode_update_metadata_broker_4(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker endpoints.
        endpoints := Endpoints,
        % The rack which this broker belongs to.
        rack := Rack
    }
) when
    ?is_int32(Id),
    ?is_array(Endpoints),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(Id),
        ?encode_array(Endpoints, fun encode_update_metadata_endpoint_4/1),
        ?encode_nullable_string(Rack)
    ];
encode_update_metadata_broker_4(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        endpoints => {array, update_metadata_endpoint_4},
        rack => nullable_string
    }).

-spec decode_update_metadata_broker_4(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_4(),
    Rest :: binary().

decode_update_metadata_broker_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_array(Endpoints, Bin1, Bin2, ?_decode_element(decode_update_metadata_endpoint_4)),
    ?_decode_nullable_string(Rack, Bin2, Bin3),
    {
        #{
            id => Id,
            endpoints => Endpoints,
            rack => Rack
        },
        Bin3
    }.

-spec encode_update_metadata_partition_state_4(update_metadata_partition_state_4()) -> iodata().

encode_update_metadata_partition_state_4(
    _Args = #{
        % In older versions of this RPC, the topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas,
        % The replicas of this partition which are offline.
        offline_replicas := OfflineReplicas
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas),
    ?is_array(OfflineReplicas)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_array(Replicas, ?encode_int32_),
        ?encode_array(OfflineReplicas, ?encode_int32_)
    ];
encode_update_metadata_partition_state_4(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_4(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_4(),
    Rest :: binary().

decode_update_metadata_partition_state_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int32(Leader, Bin3, Bin4),
    ?_decode_int32(LeaderEpoch, Bin4, Bin5),
    ?_decode_array(Isr, Bin5, Bin6, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin6, Bin7),
    ?_decode_array(Replicas, Bin7, Bin8, ?decode_int32_),
    ?_decode_array(OfflineReplicas, Bin8, Bin9, ?decode_int32_),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas,
            offline_replicas => OfflineReplicas
        },
        Bin9
    }.

-spec encode_update_metadata_request_5(update_metadata_request_5()) -> iodata().

encode_update_metadata_request_5(
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
        % In newer versions of this RPC, each topic that we would like to update.
        topic_states := TopicStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_1(?UPDATE_METADATA_REQUEST, 5, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_array(TopicStates, fun encode_update_metadata_topic_state_5/1),
        ?encode_array(LiveBrokers, fun encode_update_metadata_broker_5/1)
    ];
encode_update_metadata_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, update_metadata_topic_state_5},
        live_brokers => {array, update_metadata_broker_5}
    }).

-spec decode_update_metadata_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_5(),
    Rest :: binary().

decode_update_metadata_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_array(TopicStates, Bin3, Bin4, ?_decode_element(decode_update_metadata_topic_state_5)),
    ?_decode_array(LiveBrokers, Bin4, Bin5, ?_decode_element(decode_update_metadata_broker_5)),
    {
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates,
            live_brokers => LiveBrokers
        },
        Bin5
    }.

-spec encode_update_metadata_topic_state_5(update_metadata_topic_state_5()) -> iodata().

encode_update_metadata_topic_state_5(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition that we would like to update.
        partition_states := PartitionStates
    }
) when
    ?is_string(TopicName),
    ?is_array(PartitionStates)
->
    [
        ?encode_string(TopicName),
        ?encode_array(PartitionStates, fun encode_update_metadata_partition_state_5/1)
    ];
encode_update_metadata_topic_state_5(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_states => {array, update_metadata_partition_state_5}
    }).

-spec decode_update_metadata_topic_state_5(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_topic_state_5(),
    Rest :: binary().

decode_update_metadata_topic_state_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_array(PartitionStates, Bin1, Bin2, ?_decode_element(decode_update_metadata_partition_state_5)),
    {
        #{
            topic_name => TopicName,
            partition_states => PartitionStates
        },
        Bin2
    }.

-spec encode_update_metadata_endpoint_5(update_metadata_endpoint_5()) -> iodata().

encode_update_metadata_endpoint_5(
    _Args = #{
        % The port of this endpoint
        port := Port,
        % The hostname of this endpoint
        host := Host,
        % The listener name.
        listener := Listener,
        % The security protocol type.
        security_protocol := SecurityProtocol
    }
) when
    ?is_int32(Port),
    ?is_string(Host),
    ?is_string(Listener),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_int32(Port),
        ?encode_string(Host),
        ?encode_string(Listener),
        ?encode_int16(SecurityProtocol)
    ];
encode_update_metadata_endpoint_5(Args) ->
    ?encoder_error(Args, #{
        port => int32,
        host => string,
        listener => string,
        security_protocol => int16
    }).

-spec decode_update_metadata_endpoint_5(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_endpoint_5(),
    Rest :: binary().

decode_update_metadata_endpoint_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Port, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_string(Listener, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    {
        #{
            port => Port,
            host => Host,
            listener => Listener,
            security_protocol => SecurityProtocol
        },
        Bin4
    }.

-spec encode_update_metadata_broker_5(update_metadata_broker_5()) -> iodata().

encode_update_metadata_broker_5(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker endpoints.
        endpoints := Endpoints,
        % The rack which this broker belongs to.
        rack := Rack
    }
) when
    ?is_int32(Id),
    ?is_array(Endpoints),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(Id),
        ?encode_array(Endpoints, fun encode_update_metadata_endpoint_5/1),
        ?encode_nullable_string(Rack)
    ];
encode_update_metadata_broker_5(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        endpoints => {array, update_metadata_endpoint_5},
        rack => nullable_string
    }).

-spec decode_update_metadata_broker_5(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_5(),
    Rest :: binary().

decode_update_metadata_broker_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_array(Endpoints, Bin1, Bin2, ?_decode_element(decode_update_metadata_endpoint_5)),
    ?_decode_nullable_string(Rack, Bin2, Bin3),
    {
        #{
            id => Id,
            endpoints => Endpoints,
            rack => Rack
        },
        Bin3
    }.

-spec encode_update_metadata_partition_state_5(update_metadata_partition_state_5()) -> iodata().

encode_update_metadata_partition_state_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas,
        % The replicas of this partition which are offline.
        offline_replicas := OfflineReplicas
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas),
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_array(Replicas, ?encode_int32_),
        ?encode_array(OfflineReplicas, ?encode_int32_)
    ];
encode_update_metadata_partition_state_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_5(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_5(),
    Rest :: binary().

decode_update_metadata_partition_state_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin5, Bin6),
    ?_decode_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_array(OfflineReplicas, Bin7, Bin8, ?decode_int32_),
    {
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas,
            offline_replicas => OfflineReplicas
        },
        Bin8
    }.

-spec encode_update_metadata_request_6(update_metadata_request_6()) -> iodata().

encode_update_metadata_request_6(
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
        % In newer versions of this RPC, each topic that we would like to update.
        topic_states := TopicStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_2(?UPDATE_METADATA_REQUEST, 6, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(TopicStates, fun encode_update_metadata_topic_state_6/1),
        ?encode_compact_array(LiveBrokers, fun encode_update_metadata_broker_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, update_metadata_topic_state_6},
        live_brokers => {array, update_metadata_broker_6}
    }).

-spec decode_update_metadata_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_6(),
    Rest :: binary().

decode_update_metadata_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_compact_array(TopicStates, Bin3, Bin4, ?_decode_element(decode_update_metadata_topic_state_6)),
    ?_decode_compact_array(LiveBrokers, Bin4, Bin5, ?_decode_element(decode_update_metadata_broker_6)),
    ?decode_tagged_fields(
        fun decode_update_metadata_request_6_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates,
            live_brokers => LiveBrokers
        },
        Bin5
    ).

-spec decode_update_metadata_request_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_request_6().

decode_update_metadata_request_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_topic_state_6(update_metadata_topic_state_6()) -> iodata().

encode_update_metadata_topic_state_6(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition that we would like to update.
        partition_states := PartitionStates
    }
) when
    ?is_string(TopicName),
    ?is_array(PartitionStates)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_compact_array(PartitionStates, fun encode_update_metadata_partition_state_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_topic_state_6(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_states => {array, update_metadata_partition_state_6}
    }).

-spec decode_update_metadata_topic_state_6(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_topic_state_6(),
    Rest :: binary().

decode_update_metadata_topic_state_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_compact_array(PartitionStates, Bin1, Bin2, ?_decode_element(decode_update_metadata_partition_state_6)),
    ?decode_tagged_fields(
        fun decode_update_metadata_topic_state_6_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_states => PartitionStates
        },
        Bin2
    ).

-spec decode_update_metadata_topic_state_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_topic_state_6().

decode_update_metadata_topic_state_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_endpoint_6(update_metadata_endpoint_6()) -> iodata().

encode_update_metadata_endpoint_6(
    _Args = #{
        % The port of this endpoint
        port := Port,
        % The hostname of this endpoint
        host := Host,
        % The listener name.
        listener := Listener,
        % The security protocol type.
        security_protocol := SecurityProtocol
    }
) when
    ?is_int32(Port),
    ?is_string(Host),
    ?is_string(Listener),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_int32(Port),
        ?encode_compact_string(Host),
        ?encode_compact_string(Listener),
        ?encode_int16(SecurityProtocol),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_endpoint_6(Args) ->
    ?encoder_error(Args, #{
        port => int32,
        host => string,
        listener => string,
        security_protocol => int16
    }).

-spec decode_update_metadata_endpoint_6(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_endpoint_6(),
    Rest :: binary().

decode_update_metadata_endpoint_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Port, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_compact_string(Listener, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_update_metadata_endpoint_6_tagged_field/3,
        #{
            port => Port,
            host => Host,
            listener => Listener,
            security_protocol => SecurityProtocol
        },
        Bin4
    ).

-spec decode_update_metadata_endpoint_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_endpoint_6().

decode_update_metadata_endpoint_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_broker_6(update_metadata_broker_6()) -> iodata().

encode_update_metadata_broker_6(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker endpoints.
        endpoints := Endpoints,
        % The rack which this broker belongs to.
        rack := Rack
    }
) when
    ?is_int32(Id),
    ?is_array(Endpoints),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(Id),
        ?encode_compact_array(Endpoints, fun encode_update_metadata_endpoint_6/1),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_broker_6(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        endpoints => {array, update_metadata_endpoint_6},
        rack => nullable_string
    }).

-spec decode_update_metadata_broker_6(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_6(),
    Rest :: binary().

decode_update_metadata_broker_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_compact_array(Endpoints, Bin1, Bin2, ?_decode_element(decode_update_metadata_endpoint_6)),
    ?_decode_compact_nullable_string(Rack, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_update_metadata_broker_6_tagged_field/3,
        #{
            id => Id,
            endpoints => Endpoints,
            rack => Rack
        },
        Bin3
    ).

-spec decode_update_metadata_broker_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_broker_6().

decode_update_metadata_broker_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_partition_state_6(update_metadata_partition_state_6()) -> iodata().

encode_update_metadata_partition_state_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas,
        % The replicas of this partition which are offline.
        offline_replicas := OfflineReplicas
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas),
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_compact_array(Replicas, ?encode_int32_),
        ?encode_compact_array(OfflineReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_partition_state_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_6(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_6(),
    Rest :: binary().

decode_update_metadata_partition_state_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin5, Bin6),
    ?_decode_compact_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_compact_array(OfflineReplicas, Bin7, Bin8, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_update_metadata_partition_state_6_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas,
            offline_replicas => OfflineReplicas
        },
        Bin8
    ).

-spec decode_update_metadata_partition_state_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_partition_state_6().

decode_update_metadata_partition_state_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_request_7(update_metadata_request_7()) -> iodata().

encode_update_metadata_request_7(
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
        % In newer versions of this RPC, each topic that we would like to update.
        topic_states := TopicStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_2(?UPDATE_METADATA_REQUEST, 7, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(TopicStates, fun encode_update_metadata_topic_state_7/1),
        ?encode_compact_array(LiveBrokers, fun encode_update_metadata_broker_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, update_metadata_topic_state_7},
        live_brokers => {array, update_metadata_broker_7}
    }).

-spec decode_update_metadata_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_7(),
    Rest :: binary().

decode_update_metadata_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int64(BrokerEpoch, Bin2, Bin3),
    ?_decode_compact_array(TopicStates, Bin3, Bin4, ?_decode_element(decode_update_metadata_topic_state_7)),
    ?_decode_compact_array(LiveBrokers, Bin4, Bin5, ?_decode_element(decode_update_metadata_broker_7)),
    ?decode_tagged_fields(
        fun decode_update_metadata_request_7_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates,
            live_brokers => LiveBrokers
        },
        Bin5
    ).

-spec decode_update_metadata_request_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_request_7().

decode_update_metadata_request_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_topic_state_7(update_metadata_topic_state_7()) -> iodata().

encode_update_metadata_topic_state_7(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The topic id.
        topic_id := TopicId,
        % The partition that we would like to update.
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
        ?encode_compact_array(PartitionStates, fun encode_update_metadata_partition_state_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_topic_state_7(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        topic_id => uuid,
        partition_states => {array, update_metadata_partition_state_7}
    }).

-spec decode_update_metadata_topic_state_7(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_topic_state_7(),
    Rest :: binary().

decode_update_metadata_topic_state_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_uuid(TopicId, Bin1, Bin2),
    ?_decode_compact_array(PartitionStates, Bin2, Bin3, ?_decode_element(decode_update_metadata_partition_state_7)),
    ?decode_tagged_fields(
        fun decode_update_metadata_topic_state_7_tagged_field/3,
        #{
            topic_name => TopicName,
            topic_id => TopicId,
            partition_states => PartitionStates
        },
        Bin3
    ).

-spec decode_update_metadata_topic_state_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_topic_state_7().

decode_update_metadata_topic_state_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_endpoint_7(update_metadata_endpoint_7()) -> iodata().

encode_update_metadata_endpoint_7(
    _Args = #{
        % The port of this endpoint
        port := Port,
        % The hostname of this endpoint
        host := Host,
        % The listener name.
        listener := Listener,
        % The security protocol type.
        security_protocol := SecurityProtocol
    }
) when
    ?is_int32(Port),
    ?is_string(Host),
    ?is_string(Listener),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_int32(Port),
        ?encode_compact_string(Host),
        ?encode_compact_string(Listener),
        ?encode_int16(SecurityProtocol),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_endpoint_7(Args) ->
    ?encoder_error(Args, #{
        port => int32,
        host => string,
        listener => string,
        security_protocol => int16
    }).

-spec decode_update_metadata_endpoint_7(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_endpoint_7(),
    Rest :: binary().

decode_update_metadata_endpoint_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Port, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_compact_string(Listener, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_update_metadata_endpoint_7_tagged_field/3,
        #{
            port => Port,
            host => Host,
            listener => Listener,
            security_protocol => SecurityProtocol
        },
        Bin4
    ).

-spec decode_update_metadata_endpoint_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_endpoint_7().

decode_update_metadata_endpoint_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_broker_7(update_metadata_broker_7()) -> iodata().

encode_update_metadata_broker_7(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker endpoints.
        endpoints := Endpoints,
        % The rack which this broker belongs to.
        rack := Rack
    }
) when
    ?is_int32(Id),
    ?is_array(Endpoints),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(Id),
        ?encode_compact_array(Endpoints, fun encode_update_metadata_endpoint_7/1),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_broker_7(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        endpoints => {array, update_metadata_endpoint_7},
        rack => nullable_string
    }).

-spec decode_update_metadata_broker_7(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_7(),
    Rest :: binary().

decode_update_metadata_broker_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_compact_array(Endpoints, Bin1, Bin2, ?_decode_element(decode_update_metadata_endpoint_7)),
    ?_decode_compact_nullable_string(Rack, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_update_metadata_broker_7_tagged_field/3,
        #{
            id => Id,
            endpoints => Endpoints,
            rack => Rack
        },
        Bin3
    ).

-spec decode_update_metadata_broker_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_broker_7().

decode_update_metadata_broker_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_partition_state_7(update_metadata_partition_state_7()) -> iodata().

encode_update_metadata_partition_state_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas,
        % The replicas of this partition which are offline.
        offline_replicas := OfflineReplicas
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas),
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_compact_array(Replicas, ?encode_int32_),
        ?encode_compact_array(OfflineReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_partition_state_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_7(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_7(),
    Rest :: binary().

decode_update_metadata_partition_state_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin5, Bin6),
    ?_decode_compact_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_compact_array(OfflineReplicas, Bin7, Bin8, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_update_metadata_partition_state_7_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas,
            offline_replicas => OfflineReplicas
        },
        Bin8
    ).

-spec decode_update_metadata_partition_state_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_partition_state_7().

decode_update_metadata_partition_state_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_request_8(update_metadata_request_8()) -> iodata().

encode_update_metadata_request_8(
    Args = #{
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
        % In newer versions of this RPC, each topic that we would like to update.
        topic_states := TopicStates,
        live_brokers := LiveBrokers
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_bool(IsKRaftController),
    ?is_int32(ControllerEpoch),
    ?is_int64(BrokerEpoch),
    ?is_array(TopicStates),
    ?is_array(LiveBrokers)
->
    [
        ?encode_request_header_2(?UPDATE_METADATA_REQUEST, 8, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_bool(IsKRaftController),
        ?encode_int32(ControllerEpoch),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(TopicStates, fun encode_update_metadata_topic_state_8/1),
        ?encode_compact_array(LiveBrokers, fun encode_update_metadata_broker_8/1),
        ?encode_tagged_fields(
            fun encode_update_metadata_request_8_tagged_field/2,
            Args
        )
    ];
encode_update_metadata_request_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        is_k_raft_controller => bool,
        controller_epoch => int32,
        broker_epoch => int64,
        topic_states => {array, update_metadata_topic_state_8},
        live_brokers => {array, update_metadata_broker_8}
    }).

-spec encode_update_metadata_request_8_tagged_field(
    Key :: atom(), Value :: integer()
) -> {non_neg_integer(), iodata()} | ignore.

encode_update_metadata_request_8_tagged_field(_Key = type, Type) ->
    {0, ?encode_int8(Type)};
encode_update_metadata_request_8_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_update_metadata_request_8(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_request_8(),
    Rest :: binary().

decode_update_metadata_request_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_bool(IsKRaftController, Bin1, Bin2),
    ?_decode_int32(ControllerEpoch, Bin2, Bin3),
    ?_decode_int64(BrokerEpoch, Bin3, Bin4),
    ?_decode_compact_array(TopicStates, Bin4, Bin5, ?_decode_element(decode_update_metadata_topic_state_8)),
    ?_decode_compact_array(LiveBrokers, Bin5, Bin6, ?_decode_element(decode_update_metadata_broker_8)),
    ?decode_tagged_fields(
        fun decode_update_metadata_request_8_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            is_k_raft_controller => IsKRaftController,
            controller_epoch => ControllerEpoch,
            broker_epoch => BrokerEpoch,
            topic_states => TopicStates,
            live_brokers => LiveBrokers
        },
        Bin6
    ).

-spec decode_update_metadata_request_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_request_8().

%% Type
%% Indicates if this request is a Full metadata snapshot (2), Incremental (1), or Unknown (0). Using during ZK migration, see KIP-866
decode_update_metadata_request_8_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_int8(Type, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{type => Type};
decode_update_metadata_request_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_topic_state_8(update_metadata_topic_state_8()) -> iodata().

encode_update_metadata_topic_state_8(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The topic id.
        topic_id := TopicId,
        % The partition that we would like to update.
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
        ?encode_compact_array(PartitionStates, fun encode_update_metadata_partition_state_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_topic_state_8(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        topic_id => uuid,
        partition_states => {array, update_metadata_partition_state_8}
    }).

-spec decode_update_metadata_topic_state_8(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_topic_state_8(),
    Rest :: binary().

decode_update_metadata_topic_state_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_uuid(TopicId, Bin1, Bin2),
    ?_decode_compact_array(PartitionStates, Bin2, Bin3, ?_decode_element(decode_update_metadata_partition_state_8)),
    ?decode_tagged_fields(
        fun decode_update_metadata_topic_state_8_tagged_field/3,
        #{
            topic_name => TopicName,
            topic_id => TopicId,
            partition_states => PartitionStates
        },
        Bin3
    ).

-spec decode_update_metadata_topic_state_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_topic_state_8().

decode_update_metadata_topic_state_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_endpoint_8(update_metadata_endpoint_8()) -> iodata().

encode_update_metadata_endpoint_8(
    _Args = #{
        % The port of this endpoint
        port := Port,
        % The hostname of this endpoint
        host := Host,
        % The listener name.
        listener := Listener,
        % The security protocol type.
        security_protocol := SecurityProtocol
    }
) when
    ?is_int32(Port),
    ?is_string(Host),
    ?is_string(Listener),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_int32(Port),
        ?encode_compact_string(Host),
        ?encode_compact_string(Listener),
        ?encode_int16(SecurityProtocol),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_endpoint_8(Args) ->
    ?encoder_error(Args, #{
        port => int32,
        host => string,
        listener => string,
        security_protocol => int16
    }).

-spec decode_update_metadata_endpoint_8(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_endpoint_8(),
    Rest :: binary().

decode_update_metadata_endpoint_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Port, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_compact_string(Listener, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_update_metadata_endpoint_8_tagged_field/3,
        #{
            port => Port,
            host => Host,
            listener => Listener,
            security_protocol => SecurityProtocol
        },
        Bin4
    ).

-spec decode_update_metadata_endpoint_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_endpoint_8().

decode_update_metadata_endpoint_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_broker_8(update_metadata_broker_8()) -> iodata().

encode_update_metadata_broker_8(
    _Args = #{
        % The broker id.
        id := Id,
        % The broker endpoints.
        endpoints := Endpoints,
        % The rack which this broker belongs to.
        rack := Rack
    }
) when
    ?is_int32(Id),
    ?is_array(Endpoints),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(Id),
        ?encode_compact_array(Endpoints, fun encode_update_metadata_endpoint_8/1),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_broker_8(Args) ->
    ?encoder_error(Args, #{
        id => int32,
        endpoints => {array, update_metadata_endpoint_8},
        rack => nullable_string
    }).

-spec decode_update_metadata_broker_8(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_broker_8(),
    Rest :: binary().

decode_update_metadata_broker_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Id, Bin0, Bin1),
    ?_decode_compact_array(Endpoints, Bin1, Bin2, ?_decode_element(decode_update_metadata_endpoint_8)),
    ?_decode_compact_nullable_string(Rack, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_update_metadata_broker_8_tagged_field/3,
        #{
            id => Id,
            endpoints => Endpoints,
            rack => Rack
        },
        Bin3
    ).

-spec decode_update_metadata_broker_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_broker_8().

decode_update_metadata_broker_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_partition_state_8(update_metadata_partition_state_8()) -> iodata().

encode_update_metadata_partition_state_8(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The controller epoch.
        controller_epoch := ControllerEpoch,
        % The ID of the broker which is the current partition leader.
        leader := Leader,
        % The leader epoch of this partition.
        leader_epoch := LeaderEpoch,
        % The brokers which are in the ISR for this partition.
        isr := Isr,
        % The Zookeeper version.
        zk_version := ZkVersion,
        % All the replicas of this partition.
        replicas := Replicas,
        % The replicas of this partition which are offline.
        offline_replicas := OfflineReplicas
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(ControllerEpoch),
    ?is_int32(Leader),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(ZkVersion),
    ?is_array(Replicas),
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(ControllerEpoch),
        ?encode_int32(Leader),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int32(ZkVersion),
        ?encode_compact_array(Replicas, ?encode_int32_),
        ?encode_compact_array(OfflineReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_partition_state_8(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        controller_epoch => int32,
        leader => int32,
        leader_epoch => int32,
        isr => {array, int32},
        zk_version => int32,
        replicas => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_update_metadata_partition_state_8(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_partition_state_8(),
    Rest :: binary().

decode_update_metadata_partition_state_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(ControllerEpoch, Bin1, Bin2),
    ?_decode_int32(Leader, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(ZkVersion, Bin5, Bin6),
    ?_decode_compact_array(Replicas, Bin6, Bin7, ?decode_int32_),
    ?_decode_compact_array(OfflineReplicas, Bin7, Bin8, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_update_metadata_partition_state_8_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            controller_epoch => ControllerEpoch,
            leader => Leader,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            zk_version => ZkVersion,
            replicas => Replicas,
            offline_replicas => OfflineReplicas
        },
        Bin8
    ).

-spec decode_update_metadata_partition_state_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_metadata_partition_state_8().

decode_update_metadata_partition_state_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type update_metadata_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    ungrouped_partition_states := list(update_metadata_partition_state_0()),
    live_brokers := list(update_metadata_broker_0())
}.
-type update_metadata_broker_0() :: #{
    id := integer(),
    v0_host := binary(),
    v0_port := integer()
}.
-type update_metadata_partition_state_0() :: #{
    topic_name := binary(),
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer())
}.
-type update_metadata_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    ungrouped_partition_states := list(update_metadata_partition_state_1()),
    live_brokers := list(update_metadata_broker_1())
}.
-type update_metadata_endpoint_1() :: #{
    port := integer(),
    host := binary(),
    security_protocol := integer()
}.
-type update_metadata_broker_1() :: #{
    id := integer(),
    endpoints := list(update_metadata_endpoint_1())
}.
-type update_metadata_partition_state_1() :: #{
    topic_name := binary(),
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer())
}.
-type update_metadata_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    ungrouped_partition_states := list(update_metadata_partition_state_2()),
    live_brokers := list(update_metadata_broker_2())
}.
-type update_metadata_endpoint_2() :: #{
    port := integer(),
    host := binary(),
    security_protocol := integer()
}.
-type update_metadata_broker_2() :: #{
    id := integer(),
    endpoints := list(update_metadata_endpoint_2()),
    rack := binary() | null
}.
-type update_metadata_partition_state_2() :: #{
    topic_name := binary(),
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer())
}.
-type update_metadata_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    ungrouped_partition_states := list(update_metadata_partition_state_3()),
    live_brokers := list(update_metadata_broker_3())
}.
-type update_metadata_endpoint_3() :: #{
    port := integer(),
    host := binary(),
    listener := binary(),
    security_protocol := integer()
}.
-type update_metadata_broker_3() :: #{
    id := integer(),
    endpoints := list(update_metadata_endpoint_3()),
    rack := binary() | null
}.
-type update_metadata_partition_state_3() :: #{
    topic_name := binary(),
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer())
}.
-type update_metadata_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    ungrouped_partition_states := list(update_metadata_partition_state_4()),
    live_brokers := list(update_metadata_broker_4())
}.
-type update_metadata_endpoint_4() :: #{
    port := integer(),
    host := binary(),
    listener := binary(),
    security_protocol := integer()
}.
-type update_metadata_broker_4() :: #{
    id := integer(),
    endpoints := list(update_metadata_endpoint_4()),
    rack := binary() | null
}.
-type update_metadata_partition_state_4() :: #{
    topic_name := binary(),
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer()),
    offline_replicas := list(integer())
}.
-type update_metadata_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(update_metadata_topic_state_5()),
    live_brokers := list(update_metadata_broker_5())
}.
-type update_metadata_topic_state_5() :: #{
    topic_name := binary(),
    partition_states := list(update_metadata_partition_state_5())
}.
-type update_metadata_endpoint_5() :: #{
    port := integer(),
    host := binary(),
    listener := binary(),
    security_protocol := integer()
}.
-type update_metadata_broker_5() :: #{
    id := integer(),
    endpoints := list(update_metadata_endpoint_5()),
    rack := binary() | null
}.
-type update_metadata_partition_state_5() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer()),
    offline_replicas := list(integer())
}.
-type update_metadata_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(update_metadata_topic_state_6()),
    live_brokers := list(update_metadata_broker_6())
}.
-type update_metadata_topic_state_6() :: #{
    topic_name := binary(),
    partition_states := list(update_metadata_partition_state_6())
}.
-type update_metadata_endpoint_6() :: #{
    port := integer(),
    host := binary(),
    listener := binary(),
    security_protocol := integer()
}.
-type update_metadata_broker_6() :: #{
    id := integer(),
    endpoints := list(update_metadata_endpoint_6()),
    rack := binary() | null
}.
-type update_metadata_partition_state_6() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer()),
    offline_replicas := list(integer())
}.
-type update_metadata_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(update_metadata_topic_state_7()),
    live_brokers := list(update_metadata_broker_7())
}.
-type update_metadata_topic_state_7() :: #{
    topic_name := binary(),
    topic_id := kafcod:uuid(),
    partition_states := list(update_metadata_partition_state_7())
}.
-type update_metadata_endpoint_7() :: #{
    port := integer(),
    host := binary(),
    listener := binary(),
    security_protocol := integer()
}.
-type update_metadata_broker_7() :: #{
    id := integer(),
    endpoints := list(update_metadata_endpoint_7()),
    rack := binary() | null
}.
-type update_metadata_partition_state_7() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer()),
    offline_replicas := list(integer())
}.
-type update_metadata_request_8() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    is_k_raft_controller := boolean(),
    type => integer(),
    controller_epoch := integer(),
    broker_epoch := integer(),
    topic_states := list(update_metadata_topic_state_8()),
    live_brokers := list(update_metadata_broker_8())
}.
-type update_metadata_topic_state_8() :: #{
    topic_name := binary(),
    topic_id := kafcod:uuid(),
    partition_states := list(update_metadata_partition_state_8())
}.
-type update_metadata_endpoint_8() :: #{
    port := integer(),
    host := binary(),
    listener := binary(),
    security_protocol := integer()
}.
-type update_metadata_broker_8() :: #{
    id := integer(),
    endpoints := list(update_metadata_endpoint_8()),
    rack := binary() | null
}.
-type update_metadata_partition_state_8() :: #{
    partition_index := integer(),
    controller_epoch := integer(),
    leader := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    zk_version := integer(),
    replicas := list(integer()),
    offline_replicas := list(integer())
}.
