-module(metadata_response).
-export([
    encode_metadata_response_0/1,
    decode_metadata_response_0/1,
    encode_metadata_response_1/1,
    decode_metadata_response_1/1,
    encode_metadata_response_2/1,
    decode_metadata_response_2/1,
    encode_metadata_response_3/1,
    decode_metadata_response_3/1,
    encode_metadata_response_4/1,
    decode_metadata_response_4/1,
    encode_metadata_response_5/1,
    decode_metadata_response_5/1,
    encode_metadata_response_6/1,
    decode_metadata_response_6/1,
    encode_metadata_response_7/1,
    decode_metadata_response_7/1,
    encode_metadata_response_8/1,
    decode_metadata_response_8/1,
    encode_metadata_response_9/1,
    decode_metadata_response_9/1,
    encode_metadata_response_10/1,
    decode_metadata_response_10/1,
    encode_metadata_response_11/1,
    decode_metadata_response_11/1,
    encode_metadata_response_12/1,
    decode_metadata_response_12/1
]).
-export_type([
    metadata_response_0/0,
    metadata_response_broker_0/0,
    metadata_response_partition_0/0,
    metadata_response_topic_0/0,
    metadata_response_1/0,
    metadata_response_broker_1/0,
    metadata_response_partition_1/0,
    metadata_response_topic_1/0,
    metadata_response_2/0,
    metadata_response_broker_2/0,
    metadata_response_partition_2/0,
    metadata_response_topic_2/0,
    metadata_response_3/0,
    metadata_response_broker_3/0,
    metadata_response_partition_3/0,
    metadata_response_topic_3/0,
    metadata_response_4/0,
    metadata_response_broker_4/0,
    metadata_response_partition_4/0,
    metadata_response_topic_4/0,
    metadata_response_5/0,
    metadata_response_broker_5/0,
    metadata_response_partition_5/0,
    metadata_response_topic_5/0,
    metadata_response_6/0,
    metadata_response_broker_6/0,
    metadata_response_partition_6/0,
    metadata_response_topic_6/0,
    metadata_response_7/0,
    metadata_response_broker_7/0,
    metadata_response_partition_7/0,
    metadata_response_topic_7/0,
    metadata_response_8/0,
    metadata_response_broker_8/0,
    metadata_response_partition_8/0,
    metadata_response_topic_8/0,
    metadata_response_9/0,
    metadata_response_broker_9/0,
    metadata_response_partition_9/0,
    metadata_response_topic_9/0,
    metadata_response_10/0,
    metadata_response_broker_10/0,
    metadata_response_partition_10/0,
    metadata_response_topic_10/0,
    metadata_response_11/0,
    metadata_response_broker_11/0,
    metadata_response_partition_11/0,
    metadata_response_topic_11/0,
    metadata_response_12/0,
    metadata_response_broker_12/0,
    metadata_response_partition_12/0,
    metadata_response_topic_12/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_metadata_response_0(metadata_response_0()) -> iodata().

encode_metadata_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each broker in the response.
        brokers := Brokers,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Brokers),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Brokers, fun encode_metadata_response_broker_0/1),
        ?encode_array(Topics, fun encode_metadata_response_topic_0/1)
    ];
encode_metadata_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        brokers => {array, metadata_response_broker_0},
        topics => {array, metadata_response_topic_0}
    }).

-spec decode_metadata_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_0(),
    Rest :: binary().

decode_metadata_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Brokers, Bin0, Bin1, ?_decode_element(decode_metadata_response_broker_0)),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_metadata_response_topic_0)),
    {
        Header#{
            brokers => Brokers,
            topics => Topics
        },
        Bin2
    }.

-spec encode_metadata_response_broker_0(metadata_response_broker_0()) -> iodata().

encode_metadata_response_broker_0(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port)
    ];
encode_metadata_response_broker_0(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32
    }).

-spec decode_metadata_response_broker_0(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_0(),
    Rest :: binary().

decode_metadata_response_broker_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port
        },
        Bin3
    }.

-spec encode_metadata_response_partition_0(metadata_response_partition_0()) -> iodata().

encode_metadata_response_partition_0(
    _Args = #{
        % The partition error, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the leader broker.
        leader_id := LeaderId,
        % The set of all nodes that host this partition.
        replica_nodes := ReplicaNodes,
        % The set of nodes that are in sync with the leader for this partition.
        isr_nodes := IsrNodes
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_array(ReplicaNodes),
    ?is_array(IsrNodes)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_)
    ];
encode_metadata_response_partition_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32}
    }).

-spec decode_metadata_response_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_0(),
    Rest :: binary().

decode_metadata_response_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_array(ReplicaNodes, Bin3, Bin4, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin4, Bin5, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes
        },
        Bin5
    }.

-spec encode_metadata_response_topic_0(metadata_response_topic_0()) -> iodata().

encode_metadata_response_topic_0(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % Each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_metadata_response_partition_0/1)
    ];
encode_metadata_response_topic_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        partitions => {array, metadata_response_partition_0}
    }).

-spec decode_metadata_response_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_0(),
    Rest :: binary().

decode_metadata_response_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_array(Partitions, Bin2, Bin3, ?_decode_element(decode_metadata_response_partition_0)),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            partitions => Partitions
        },
        Bin3
    }.

-spec encode_metadata_response_1(metadata_response_1()) -> iodata().

encode_metadata_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each broker in the response.
        brokers := Brokers,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Brokers),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Brokers, fun encode_metadata_response_broker_1/1),
        ?encode_int32(ControllerId),
        ?encode_array(Topics, fun encode_metadata_response_topic_1/1)
    ];
encode_metadata_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        brokers => {array, metadata_response_broker_1},
        controller_id => int32,
        topics => {array, metadata_response_topic_1}
    }).

-spec decode_metadata_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_1(),
    Rest :: binary().

decode_metadata_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Brokers, Bin0, Bin1, ?_decode_element(decode_metadata_response_broker_1)),
    ?_decode_int32(ControllerId, Bin1, Bin2),
    ?_decode_array(Topics, Bin2, Bin3, ?_decode_element(decode_metadata_response_topic_1)),
    {
        Header#{
            brokers => Brokers,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin3
    }.

-spec encode_metadata_response_broker_1(metadata_response_broker_1()) -> iodata().

encode_metadata_response_broker_1(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port),
        ?encode_nullable_string(Rack)
    ];
encode_metadata_response_broker_1(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_1(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_1(),
    Rest :: binary().

decode_metadata_response_broker_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_nullable_string(Rack, Bin3, Bin4),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    }.

-spec encode_metadata_response_partition_1(metadata_response_partition_1()) -> iodata().

encode_metadata_response_partition_1(
    _Args = #{
        % The partition error, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the leader broker.
        leader_id := LeaderId,
        % The set of all nodes that host this partition.
        replica_nodes := ReplicaNodes,
        % The set of nodes that are in sync with the leader for this partition.
        isr_nodes := IsrNodes
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_array(ReplicaNodes),
    ?is_array(IsrNodes)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_)
    ];
encode_metadata_response_partition_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32}
    }).

-spec decode_metadata_response_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_1(),
    Rest :: binary().

decode_metadata_response_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_array(ReplicaNodes, Bin3, Bin4, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin4, Bin5, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes
        },
        Bin5
    }.

-spec encode_metadata_response_topic_1(metadata_response_topic_1()) -> iodata().

encode_metadata_response_topic_1(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_bool(IsInternal),
        ?encode_array(Partitions, fun encode_metadata_response_partition_1/1)
    ];
encode_metadata_response_topic_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_1}
    }).

-spec decode_metadata_response_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_1(),
    Rest :: binary().

decode_metadata_response_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_1)),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions
        },
        Bin4
    }.

-spec encode_metadata_response_2(metadata_response_2()) -> iodata().

encode_metadata_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Brokers, fun encode_metadata_response_broker_2/1),
        ?encode_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_array(Topics, fun encode_metadata_response_topic_2/1)
    ];
encode_metadata_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        brokers => {array, metadata_response_broker_2},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_2}
    }).

-spec decode_metadata_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_2(),
    Rest :: binary().

decode_metadata_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Brokers, Bin0, Bin1, ?_decode_element(decode_metadata_response_broker_2)),
    ?_decode_nullable_string(ClusterId, Bin1, Bin2),
    ?_decode_int32(ControllerId, Bin2, Bin3),
    ?_decode_array(Topics, Bin3, Bin4, ?_decode_element(decode_metadata_response_topic_2)),
    {
        Header#{
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin4
    }.

-spec encode_metadata_response_broker_2(metadata_response_broker_2()) -> iodata().

encode_metadata_response_broker_2(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port),
        ?encode_nullable_string(Rack)
    ];
encode_metadata_response_broker_2(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_2(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_2(),
    Rest :: binary().

decode_metadata_response_broker_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_nullable_string(Rack, Bin3, Bin4),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    }.

-spec encode_metadata_response_partition_2(metadata_response_partition_2()) -> iodata().

encode_metadata_response_partition_2(
    _Args = #{
        % The partition error, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the leader broker.
        leader_id := LeaderId,
        % The set of all nodes that host this partition.
        replica_nodes := ReplicaNodes,
        % The set of nodes that are in sync with the leader for this partition.
        isr_nodes := IsrNodes
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_array(ReplicaNodes),
    ?is_array(IsrNodes)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_)
    ];
encode_metadata_response_partition_2(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32}
    }).

-spec decode_metadata_response_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_2(),
    Rest :: binary().

decode_metadata_response_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_array(ReplicaNodes, Bin3, Bin4, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin4, Bin5, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes
        },
        Bin5
    }.

-spec encode_metadata_response_topic_2(metadata_response_topic_2()) -> iodata().

encode_metadata_response_topic_2(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_bool(IsInternal),
        ?encode_array(Partitions, fun encode_metadata_response_partition_2/1)
    ];
encode_metadata_response_topic_2(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_2}
    }).

-spec decode_metadata_response_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_2(),
    Rest :: binary().

decode_metadata_response_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_2)),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions
        },
        Bin4
    }.

-spec encode_metadata_response_3(metadata_response_3()) -> iodata().

encode_metadata_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Brokers, fun encode_metadata_response_broker_3/1),
        ?encode_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_array(Topics, fun encode_metadata_response_topic_3/1)
    ];
encode_metadata_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_3},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_3}
    }).

-spec decode_metadata_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_3(),
    Rest :: binary().

decode_metadata_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_3)),
    ?_decode_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin5
    }.

-spec encode_metadata_response_broker_3(metadata_response_broker_3()) -> iodata().

encode_metadata_response_broker_3(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port),
        ?encode_nullable_string(Rack)
    ];
encode_metadata_response_broker_3(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_3(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_3(),
    Rest :: binary().

decode_metadata_response_broker_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_nullable_string(Rack, Bin3, Bin4),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    }.

-spec encode_metadata_response_partition_3(metadata_response_partition_3()) -> iodata().

encode_metadata_response_partition_3(
    _Args = #{
        % The partition error, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the leader broker.
        leader_id := LeaderId,
        % The set of all nodes that host this partition.
        replica_nodes := ReplicaNodes,
        % The set of nodes that are in sync with the leader for this partition.
        isr_nodes := IsrNodes
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_array(ReplicaNodes),
    ?is_array(IsrNodes)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_)
    ];
encode_metadata_response_partition_3(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32}
    }).

-spec decode_metadata_response_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_3(),
    Rest :: binary().

decode_metadata_response_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_array(ReplicaNodes, Bin3, Bin4, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin4, Bin5, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes
        },
        Bin5
    }.

-spec encode_metadata_response_topic_3(metadata_response_topic_3()) -> iodata().

encode_metadata_response_topic_3(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_bool(IsInternal),
        ?encode_array(Partitions, fun encode_metadata_response_partition_3/1)
    ];
encode_metadata_response_topic_3(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_3}
    }).

-spec decode_metadata_response_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_3(),
    Rest :: binary().

decode_metadata_response_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_3)),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions
        },
        Bin4
    }.

-spec encode_metadata_response_4(metadata_response_4()) -> iodata().

encode_metadata_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Brokers, fun encode_metadata_response_broker_4/1),
        ?encode_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_array(Topics, fun encode_metadata_response_topic_4/1)
    ];
encode_metadata_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_4},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_4}
    }).

-spec decode_metadata_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_4(),
    Rest :: binary().

decode_metadata_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_4)),
    ?_decode_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_4)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin5
    }.

-spec encode_metadata_response_broker_4(metadata_response_broker_4()) -> iodata().

encode_metadata_response_broker_4(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port),
        ?encode_nullable_string(Rack)
    ];
encode_metadata_response_broker_4(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_4(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_4(),
    Rest :: binary().

decode_metadata_response_broker_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_nullable_string(Rack, Bin3, Bin4),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    }.

-spec encode_metadata_response_partition_4(metadata_response_partition_4()) -> iodata().

encode_metadata_response_partition_4(
    _Args = #{
        % The partition error, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the leader broker.
        leader_id := LeaderId,
        % The set of all nodes that host this partition.
        replica_nodes := ReplicaNodes,
        % The set of nodes that are in sync with the leader for this partition.
        isr_nodes := IsrNodes
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_array(ReplicaNodes),
    ?is_array(IsrNodes)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_)
    ];
encode_metadata_response_partition_4(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32}
    }).

-spec decode_metadata_response_partition_4(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_4(),
    Rest :: binary().

decode_metadata_response_partition_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_array(ReplicaNodes, Bin3, Bin4, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin4, Bin5, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes
        },
        Bin5
    }.

-spec encode_metadata_response_topic_4(metadata_response_topic_4()) -> iodata().

encode_metadata_response_topic_4(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_bool(IsInternal),
        ?encode_array(Partitions, fun encode_metadata_response_partition_4/1)
    ];
encode_metadata_response_topic_4(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_4}
    }).

-spec decode_metadata_response_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_4(),
    Rest :: binary().

decode_metadata_response_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_4)),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions
        },
        Bin4
    }.

-spec encode_metadata_response_5(metadata_response_5()) -> iodata().

encode_metadata_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Brokers, fun encode_metadata_response_broker_5/1),
        ?encode_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_array(Topics, fun encode_metadata_response_topic_5/1)
    ];
encode_metadata_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_5},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_5}
    }).

-spec decode_metadata_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_5(),
    Rest :: binary().

decode_metadata_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_5)),
    ?_decode_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_5)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin5
    }.

-spec encode_metadata_response_broker_5(metadata_response_broker_5()) -> iodata().

encode_metadata_response_broker_5(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port),
        ?encode_nullable_string(Rack)
    ];
encode_metadata_response_broker_5(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_5(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_5(),
    Rest :: binary().

decode_metadata_response_broker_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_nullable_string(Rack, Bin3, Bin4),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    }.

-spec encode_metadata_response_partition_5(metadata_response_partition_5()) -> iodata().

encode_metadata_response_partition_5(
    _Args = #{
        % The partition error, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the leader broker.
        leader_id := LeaderId,
        % The set of all nodes that host this partition.
        replica_nodes := ReplicaNodes,
        % The set of nodes that are in sync with the leader for this partition.
        isr_nodes := IsrNodes,
        % The set of offline replicas of this partition.
        offline_replicas := OfflineReplicas
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_array(ReplicaNodes),
    ?is_array(IsrNodes),
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_),
        ?encode_array(OfflineReplicas, ?encode_int32_)
    ];
encode_metadata_response_partition_5(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_metadata_response_partition_5(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_5(),
    Rest :: binary().

decode_metadata_response_partition_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_array(ReplicaNodes, Bin3, Bin4, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_array(OfflineReplicas, Bin5, Bin6, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            offline_replicas => OfflineReplicas
        },
        Bin6
    }.

-spec encode_metadata_response_topic_5(metadata_response_topic_5()) -> iodata().

encode_metadata_response_topic_5(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_bool(IsInternal),
        ?encode_array(Partitions, fun encode_metadata_response_partition_5/1)
    ];
encode_metadata_response_topic_5(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_5}
    }).

-spec decode_metadata_response_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_5(),
    Rest :: binary().

decode_metadata_response_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_5)),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions
        },
        Bin4
    }.

-spec encode_metadata_response_6(metadata_response_6()) -> iodata().

encode_metadata_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Brokers, fun encode_metadata_response_broker_6/1),
        ?encode_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_array(Topics, fun encode_metadata_response_topic_6/1)
    ];
encode_metadata_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_6},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_6}
    }).

-spec decode_metadata_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_6(),
    Rest :: binary().

decode_metadata_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_6)),
    ?_decode_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_6)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin5
    }.

-spec encode_metadata_response_broker_6(metadata_response_broker_6()) -> iodata().

encode_metadata_response_broker_6(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port),
        ?encode_nullable_string(Rack)
    ];
encode_metadata_response_broker_6(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_6(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_6(),
    Rest :: binary().

decode_metadata_response_broker_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_nullable_string(Rack, Bin3, Bin4),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    }.

-spec encode_metadata_response_partition_6(metadata_response_partition_6()) -> iodata().

encode_metadata_response_partition_6(
    _Args = #{
        % The partition error, or 0 if there was no error.
        error_code := ErrorCode,
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the leader broker.
        leader_id := LeaderId,
        % The set of all nodes that host this partition.
        replica_nodes := ReplicaNodes,
        % The set of nodes that are in sync with the leader for this partition.
        isr_nodes := IsrNodes,
        % The set of offline replicas of this partition.
        offline_replicas := OfflineReplicas
    }
) when
    ?is_int16(ErrorCode),
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_array(ReplicaNodes),
    ?is_array(IsrNodes),
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_),
        ?encode_array(OfflineReplicas, ?encode_int32_)
    ];
encode_metadata_response_partition_6(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_metadata_response_partition_6(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_6(),
    Rest :: binary().

decode_metadata_response_partition_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_array(ReplicaNodes, Bin3, Bin4, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_array(OfflineReplicas, Bin5, Bin6, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            offline_replicas => OfflineReplicas
        },
        Bin6
    }.

-spec encode_metadata_response_topic_6(metadata_response_topic_6()) -> iodata().

encode_metadata_response_topic_6(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_bool(IsInternal),
        ?encode_array(Partitions, fun encode_metadata_response_partition_6/1)
    ];
encode_metadata_response_topic_6(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_6}
    }).

-spec decode_metadata_response_topic_6(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_6(),
    Rest :: binary().

decode_metadata_response_topic_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_6)),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions
        },
        Bin4
    }.

-spec encode_metadata_response_7(metadata_response_7()) -> iodata().

encode_metadata_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Brokers, fun encode_metadata_response_broker_7/1),
        ?encode_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_array(Topics, fun encode_metadata_response_topic_7/1)
    ];
encode_metadata_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_7},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_7}
    }).

-spec decode_metadata_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_7(),
    Rest :: binary().

decode_metadata_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_7)),
    ?_decode_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_7)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin5
    }.

-spec encode_metadata_response_broker_7(metadata_response_broker_7()) -> iodata().

encode_metadata_response_broker_7(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port),
        ?encode_nullable_string(Rack)
    ];
encode_metadata_response_broker_7(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_7(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_7(),
    Rest :: binary().

decode_metadata_response_broker_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_nullable_string(Rack, Bin3, Bin4),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    }.

-spec encode_metadata_response_partition_7(metadata_response_partition_7()) -> iodata().

encode_metadata_response_partition_7(
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
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_),
        ?encode_array(OfflineReplicas, ?encode_int32_)
    ];
encode_metadata_response_partition_7(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        leader_epoch => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_metadata_response_partition_7(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_7(),
    Rest :: binary().

decode_metadata_response_partition_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_array(ReplicaNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin5, Bin6, ?decode_int32_),
    ?_decode_array(OfflineReplicas, Bin6, Bin7, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            offline_replicas => OfflineReplicas
        },
        Bin7
    }.

-spec encode_metadata_response_topic_7(metadata_response_topic_7()) -> iodata().

encode_metadata_response_topic_7(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_bool(IsInternal),
        ?encode_array(Partitions, fun encode_metadata_response_partition_7/1)
    ];
encode_metadata_response_topic_7(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_7}
    }).

-spec decode_metadata_response_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_7(),
    Rest :: binary().

decode_metadata_response_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_7)),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions
        },
        Bin4
    }.

-spec encode_metadata_response_8(metadata_response_8()) -> iodata().

encode_metadata_response_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics,
        % 32-bit bitfield to represent authorized operations for this cluster.
        cluster_authorized_operations := ClusterAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics),
    ?is_int32(ClusterAuthorizedOperations)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Brokers, fun encode_metadata_response_broker_8/1),
        ?encode_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_array(Topics, fun encode_metadata_response_topic_8/1),
        ?encode_int32(ClusterAuthorizedOperations)
    ];
encode_metadata_response_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_8},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_8},
        cluster_authorized_operations => int32
    }).

-spec decode_metadata_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_8(),
    Rest :: binary().

decode_metadata_response_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_8)),
    ?_decode_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_8)),
    ?_decode_int32(ClusterAuthorizedOperations, Bin5, Bin6),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics,
            cluster_authorized_operations => ClusterAuthorizedOperations
        },
        Bin6
    }.

-spec encode_metadata_response_broker_8(metadata_response_broker_8()) -> iodata().

encode_metadata_response_broker_8(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port),
        ?encode_nullable_string(Rack)
    ];
encode_metadata_response_broker_8(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_8(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_8(),
    Rest :: binary().

decode_metadata_response_broker_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_nullable_string(Rack, Bin3, Bin4),
    {
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    }.

-spec encode_metadata_response_partition_8(metadata_response_partition_8()) -> iodata().

encode_metadata_response_partition_8(
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
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_array(ReplicaNodes, ?encode_int32_),
        ?encode_array(IsrNodes, ?encode_int32_),
        ?encode_array(OfflineReplicas, ?encode_int32_)
    ];
encode_metadata_response_partition_8(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        leader_epoch => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_metadata_response_partition_8(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_8(),
    Rest :: binary().

decode_metadata_response_partition_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_array(ReplicaNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_array(IsrNodes, Bin5, Bin6, ?decode_int32_),
    ?_decode_array(OfflineReplicas, Bin6, Bin7, ?decode_int32_),
    {
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            offline_replicas => OfflineReplicas
        },
        Bin7
    }.

-spec encode_metadata_response_topic_8(metadata_response_topic_8()) -> iodata().

encode_metadata_response_topic_8(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions,
        % 32-bit bitfield to represent authorized operations for this topic.
        topic_authorized_operations := TopicAuthorizedOperations
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions),
    ?is_int32(TopicAuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(Name),
        ?encode_bool(IsInternal),
        ?encode_array(Partitions, fun encode_metadata_response_partition_8/1),
        ?encode_int32(TopicAuthorizedOperations)
    ];
encode_metadata_response_topic_8(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_8},
        topic_authorized_operations => int32
    }).

-spec decode_metadata_response_topic_8(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_8(),
    Rest :: binary().

decode_metadata_response_topic_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_8)),
    ?_decode_int32(TopicAuthorizedOperations, Bin4, Bin5),
    {
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions,
            topic_authorized_operations => TopicAuthorizedOperations
        },
        Bin5
    }.

-spec encode_metadata_response_9(metadata_response_9()) -> iodata().

encode_metadata_response_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics,
        % 32-bit bitfield to represent authorized operations for this cluster.
        cluster_authorized_operations := ClusterAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics),
    ?is_int32(ClusterAuthorizedOperations)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Brokers, fun encode_metadata_response_broker_9/1),
        ?encode_compact_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_compact_array(Topics, fun encode_metadata_response_topic_9/1),
        ?encode_int32(ClusterAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_9},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_9},
        cluster_authorized_operations => int32
    }).

-spec decode_metadata_response_9(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_9(),
    Rest :: binary().

decode_metadata_response_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_9)),
    ?_decode_compact_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_9)),
    ?_decode_int32(ClusterAuthorizedOperations, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_metadata_response_9_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics,
            cluster_authorized_operations => ClusterAuthorizedOperations
        },
        Bin6
    ).

-spec decode_metadata_response_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_broker_9(metadata_response_broker_9()) -> iodata().

encode_metadata_response_broker_9(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_broker_9(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_9(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_9(),
    Rest :: binary().

decode_metadata_response_broker_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_compact_nullable_string(Rack, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_metadata_response_broker_9_tagged_field/3,
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    ).

-spec decode_metadata_response_broker_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_broker_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_partition_9(metadata_response_partition_9()) -> iodata().

encode_metadata_response_partition_9(
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
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(ReplicaNodes, ?encode_int32_),
        ?encode_compact_array(IsrNodes, ?encode_int32_),
        ?encode_compact_array(OfflineReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_partition_9(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        leader_epoch => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_metadata_response_partition_9(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_9(),
    Rest :: binary().

decode_metadata_response_partition_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(ReplicaNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_compact_array(IsrNodes, Bin5, Bin6, ?decode_int32_),
    ?_decode_compact_array(OfflineReplicas, Bin6, Bin7, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_metadata_response_partition_9_tagged_field/3,
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            offline_replicas => OfflineReplicas
        },
        Bin7
    ).

-spec decode_metadata_response_partition_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_partition_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_topic_9(metadata_response_topic_9()) -> iodata().

encode_metadata_response_topic_9(
    _Args = #{
        % The topic error, or 0 if there was no error.
        error_code := ErrorCode,
        % The topic name.
        name := Name,
        % True if the topic is internal.
        is_internal := IsInternal,
        % Each partition in the topic.
        partitions := Partitions,
        % 32-bit bitfield to represent authorized operations for this topic.
        topic_authorized_operations := TopicAuthorizedOperations
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(Name),
    ?is_bool(IsInternal),
    ?is_array(Partitions),
    ?is_int32(TopicAuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_string(Name),
        ?encode_bool(IsInternal),
        ?encode_compact_array(Partitions, fun encode_metadata_response_partition_9/1),
        ?encode_int32(TopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_topic_9(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        is_internal => bool,
        partitions => {array, metadata_response_partition_9},
        topic_authorized_operations => int32
    }).

-spec decode_metadata_response_topic_9(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_9(),
    Rest :: binary().

decode_metadata_response_topic_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(Name, Bin1, Bin2),
    ?_decode_bool(IsInternal, Bin2, Bin3),
    ?_decode_compact_array(Partitions, Bin3, Bin4, ?_decode_element(decode_metadata_response_partition_9)),
    ?_decode_int32(TopicAuthorizedOperations, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_metadata_response_topic_9_tagged_field/3,
        #{
            error_code => ErrorCode,
            name => Name,
            is_internal => IsInternal,
            partitions => Partitions,
            topic_authorized_operations => TopicAuthorizedOperations
        },
        Bin5
    ).

-spec decode_metadata_response_topic_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_topic_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_10(metadata_response_10()) -> iodata().

encode_metadata_response_10(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics,
        % 32-bit bitfield to represent authorized operations for this cluster.
        cluster_authorized_operations := ClusterAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics),
    ?is_int32(ClusterAuthorizedOperations)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Brokers, fun encode_metadata_response_broker_10/1),
        ?encode_compact_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_compact_array(Topics, fun encode_metadata_response_topic_10/1),
        ?encode_int32(ClusterAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_10(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_10},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_10},
        cluster_authorized_operations => int32
    }).

-spec decode_metadata_response_10(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_10(),
    Rest :: binary().

decode_metadata_response_10(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_10)),
    ?_decode_compact_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_10)),
    ?_decode_int32(ClusterAuthorizedOperations, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_metadata_response_10_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics,
            cluster_authorized_operations => ClusterAuthorizedOperations
        },
        Bin6
    ).

-spec decode_metadata_response_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_broker_10(metadata_response_broker_10()) -> iodata().

encode_metadata_response_broker_10(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_broker_10(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_10(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_10(),
    Rest :: binary().

decode_metadata_response_broker_10(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_compact_nullable_string(Rack, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_metadata_response_broker_10_tagged_field/3,
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    ).

-spec decode_metadata_response_broker_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_broker_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_partition_10(metadata_response_partition_10()) -> iodata().

encode_metadata_response_partition_10(
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
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(ReplicaNodes, ?encode_int32_),
        ?encode_compact_array(IsrNodes, ?encode_int32_),
        ?encode_compact_array(OfflineReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_partition_10(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        leader_epoch => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_metadata_response_partition_10(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_10(),
    Rest :: binary().

decode_metadata_response_partition_10(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(ReplicaNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_compact_array(IsrNodes, Bin5, Bin6, ?decode_int32_),
    ?_decode_compact_array(OfflineReplicas, Bin6, Bin7, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_metadata_response_partition_10_tagged_field/3,
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            offline_replicas => OfflineReplicas
        },
        Bin7
    ).

-spec decode_metadata_response_partition_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_partition_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_topic_10(metadata_response_topic_10()) -> iodata().

encode_metadata_response_topic_10(
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
    ?is_string(Name),
    ?is_uuid(TopicId),
    ?is_bool(IsInternal),
    ?is_array(Partitions),
    ?is_int32(TopicAuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_string(Name),
        ?encode_uuid(TopicId),
        ?encode_bool(IsInternal),
        ?encode_compact_array(Partitions, fun encode_metadata_response_partition_10/1),
        ?encode_int32(TopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_topic_10(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        topic_id => uuid,
        is_internal => bool,
        partitions => {array, metadata_response_partition_10},
        topic_authorized_operations => int32
    }).

-spec decode_metadata_response_topic_10(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_10(),
    Rest :: binary().

decode_metadata_response_topic_10(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(Name, Bin1, Bin2),
    ?_decode_uuid(TopicId, Bin2, Bin3),
    ?_decode_bool(IsInternal, Bin3, Bin4),
    ?_decode_compact_array(Partitions, Bin4, Bin5, ?_decode_element(decode_metadata_response_partition_10)),
    ?_decode_int32(TopicAuthorizedOperations, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_metadata_response_topic_10_tagged_field/3,
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

-spec decode_metadata_response_topic_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_topic_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_11(metadata_response_11()) -> iodata().

encode_metadata_response_11(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Brokers, fun encode_metadata_response_broker_11/1),
        ?encode_compact_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_compact_array(Topics, fun encode_metadata_response_topic_11/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_11(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_11},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_11}
    }).

-spec decode_metadata_response_11(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_11(),
    Rest :: binary().

decode_metadata_response_11(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_11)),
    ?_decode_compact_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_11)),
    ?decode_tagged_fields(
        fun decode_metadata_response_11_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin5
    ).

-spec decode_metadata_response_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_broker_11(metadata_response_broker_11()) -> iodata().

encode_metadata_response_broker_11(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_broker_11(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_11(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_11(),
    Rest :: binary().

decode_metadata_response_broker_11(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_compact_nullable_string(Rack, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_metadata_response_broker_11_tagged_field/3,
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    ).

-spec decode_metadata_response_broker_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_broker_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_partition_11(metadata_response_partition_11()) -> iodata().

encode_metadata_response_partition_11(
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
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(ReplicaNodes, ?encode_int32_),
        ?encode_compact_array(IsrNodes, ?encode_int32_),
        ?encode_compact_array(OfflineReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_partition_11(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        leader_epoch => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_metadata_response_partition_11(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_11(),
    Rest :: binary().

decode_metadata_response_partition_11(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(ReplicaNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_compact_array(IsrNodes, Bin5, Bin6, ?decode_int32_),
    ?_decode_compact_array(OfflineReplicas, Bin6, Bin7, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_metadata_response_partition_11_tagged_field/3,
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            offline_replicas => OfflineReplicas
        },
        Bin7
    ).

-spec decode_metadata_response_partition_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_partition_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_topic_11(metadata_response_topic_11()) -> iodata().

encode_metadata_response_topic_11(
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
    ?is_string(Name),
    ?is_uuid(TopicId),
    ?is_bool(IsInternal),
    ?is_array(Partitions),
    ?is_int32(TopicAuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_string(Name),
        ?encode_uuid(TopicId),
        ?encode_bool(IsInternal),
        ?encode_compact_array(Partitions, fun encode_metadata_response_partition_11/1),
        ?encode_int32(TopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_topic_11(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => string,
        topic_id => uuid,
        is_internal => bool,
        partitions => {array, metadata_response_partition_11},
        topic_authorized_operations => int32
    }).

-spec decode_metadata_response_topic_11(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_11(),
    Rest :: binary().

decode_metadata_response_topic_11(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(Name, Bin1, Bin2),
    ?_decode_uuid(TopicId, Bin2, Bin3),
    ?_decode_bool(IsInternal, Bin3, Bin4),
    ?_decode_compact_array(Partitions, Bin4, Bin5, ?_decode_element(decode_metadata_response_partition_11)),
    ?_decode_int32(TopicAuthorizedOperations, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_metadata_response_topic_11_tagged_field/3,
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

-spec decode_metadata_response_topic_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_topic_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_12(metadata_response_12()) -> iodata().

encode_metadata_response_12(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each broker in the response.
        brokers := Brokers,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each topic in the response.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Brokers),
    ?is_nullable_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Brokers, fun encode_metadata_response_broker_12/1),
        ?encode_compact_nullable_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_compact_array(Topics, fun encode_metadata_response_topic_12/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_12(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        brokers => {array, metadata_response_broker_12},
        cluster_id => nullable_string,
        controller_id => int32,
        topics => {array, metadata_response_topic_12}
    }).

-spec decode_metadata_response_12(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_12(),
    Rest :: binary().

decode_metadata_response_12(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Brokers, Bin1, Bin2, ?_decode_element(decode_metadata_response_broker_12)),
    ?_decode_compact_nullable_string(ClusterId, Bin2, Bin3),
    ?_decode_int32(ControllerId, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_metadata_response_topic_12)),
    ?decode_tagged_fields(
        fun decode_metadata_response_12_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            brokers => Brokers,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            topics => Topics
        },
        Bin5
    ).

-spec decode_metadata_response_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_broker_12(metadata_response_broker_12()) -> iodata().

encode_metadata_response_broker_12(
    _Args = #{
        % The broker ID.
        node_id := NodeId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(NodeId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_broker_12(Args) ->
    ?encoder_error(Args, #{
        node_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_metadata_response_broker_12(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_broker_12(),
    Rest :: binary().

decode_metadata_response_broker_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(NodeId, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_compact_nullable_string(Rack, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_metadata_response_broker_12_tagged_field/3,
        #{
            node_id => NodeId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    ).

-spec decode_metadata_response_broker_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_broker_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_partition_12(metadata_response_partition_12()) -> iodata().

encode_metadata_response_partition_12(
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
    ?is_array(OfflineReplicas)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(ReplicaNodes, ?encode_int32_),
        ?encode_compact_array(IsrNodes, ?encode_int32_),
        ?encode_compact_array(OfflineReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_partition_12(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        partition_index => int32,
        leader_id => int32,
        leader_epoch => int32,
        replica_nodes => {array, int32},
        isr_nodes => {array, int32},
        offline_replicas => {array, int32}
    }).

-spec decode_metadata_response_partition_12(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_partition_12(),
    Rest :: binary().

decode_metadata_response_partition_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(ReplicaNodes, Bin4, Bin5, ?decode_int32_),
    ?_decode_compact_array(IsrNodes, Bin5, Bin6, ?decode_int32_),
    ?_decode_compact_array(OfflineReplicas, Bin6, Bin7, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_metadata_response_partition_12_tagged_field/3,
        #{
            error_code => ErrorCode,
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            replica_nodes => ReplicaNodes,
            isr_nodes => IsrNodes,
            offline_replicas => OfflineReplicas
        },
        Bin7
    ).

-spec decode_metadata_response_partition_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_partition_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_response_topic_12(metadata_response_topic_12()) -> iodata().

encode_metadata_response_topic_12(
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
        ?encode_compact_array(Partitions, fun encode_metadata_response_partition_12/1),
        ?encode_int32(TopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_response_topic_12(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        name => nullable_string,
        topic_id => uuid,
        is_internal => bool,
        partitions => {array, metadata_response_partition_12},
        topic_authorized_operations => int32
    }).

-spec decode_metadata_response_topic_12(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_response_topic_12(),
    Rest :: binary().

decode_metadata_response_topic_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(Name, Bin1, Bin2),
    ?_decode_uuid(TopicId, Bin2, Bin3),
    ?_decode_bool(IsInternal, Bin3, Bin4),
    ?_decode_compact_array(Partitions, Bin4, Bin5, ?_decode_element(decode_metadata_response_partition_12)),
    ?_decode_int32(TopicAuthorizedOperations, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_metadata_response_topic_12_tagged_field/3,
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

-spec decode_metadata_response_topic_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_metadata_response_topic_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type metadata_response_0() :: #{
    correlation_id => integer(),
    brokers := list(metadata_response_broker_0()),
    topics := list(metadata_response_topic_0())
}.
-type metadata_response_broker_0() :: #{
    node_id := integer(),
    host := binary(),
    port := integer()
}.
-type metadata_response_partition_0() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer())
}.
-type metadata_response_topic_0() :: #{
    error_code := integer(),
    name := binary(),
    partitions := list(metadata_response_partition_0())
}.
-type metadata_response_1() :: #{
    correlation_id => integer(),
    brokers := list(metadata_response_broker_1()),
    controller_id := integer(),
    topics := list(metadata_response_topic_1())
}.
-type metadata_response_broker_1() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_1() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer())
}.
-type metadata_response_topic_1() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_1())
}.
-type metadata_response_2() :: #{
    correlation_id => integer(),
    brokers := list(metadata_response_broker_2()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_2())
}.
-type metadata_response_broker_2() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_2() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer())
}.
-type metadata_response_topic_2() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_2())
}.
-type metadata_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_3()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_3())
}.
-type metadata_response_broker_3() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_3() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer())
}.
-type metadata_response_topic_3() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_3())
}.
-type metadata_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_4()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_4())
}.
-type metadata_response_broker_4() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_4() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer())
}.
-type metadata_response_topic_4() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_4())
}.
-type metadata_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_5()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_5())
}.
-type metadata_response_broker_5() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_5() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    offline_replicas := list(integer())
}.
-type metadata_response_topic_5() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_5())
}.
-type metadata_response_6() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_6()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_6())
}.
-type metadata_response_broker_6() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_6() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    offline_replicas := list(integer())
}.
-type metadata_response_topic_6() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_6())
}.
-type metadata_response_7() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_7()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_7())
}.
-type metadata_response_broker_7() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_7() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    offline_replicas := list(integer())
}.
-type metadata_response_topic_7() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_7())
}.
-type metadata_response_8() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_8()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_8()),
    cluster_authorized_operations := integer()
}.
-type metadata_response_broker_8() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_8() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    offline_replicas := list(integer())
}.
-type metadata_response_topic_8() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_8()),
    topic_authorized_operations := integer()
}.
-type metadata_response_9() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_9()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_9()),
    cluster_authorized_operations := integer()
}.
-type metadata_response_broker_9() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_9() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    offline_replicas := list(integer())
}.
-type metadata_response_topic_9() :: #{
    error_code := integer(),
    name := binary(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_9()),
    topic_authorized_operations := integer()
}.
-type metadata_response_10() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_10()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_10()),
    cluster_authorized_operations := integer()
}.
-type metadata_response_broker_10() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_10() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    offline_replicas := list(integer())
}.
-type metadata_response_topic_10() :: #{
    error_code := integer(),
    name := binary(),
    topic_id := kafcod:uuid(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_10()),
    topic_authorized_operations := integer()
}.
-type metadata_response_11() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_11()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_11())
}.
-type metadata_response_broker_11() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_11() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    offline_replicas := list(integer())
}.
-type metadata_response_topic_11() :: #{
    error_code := integer(),
    name := binary(),
    topic_id := kafcod:uuid(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_11()),
    topic_authorized_operations := integer()
}.
-type metadata_response_12() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    brokers := list(metadata_response_broker_12()),
    cluster_id := binary() | null,
    controller_id := integer(),
    topics := list(metadata_response_topic_12())
}.
-type metadata_response_broker_12() :: #{
    node_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type metadata_response_partition_12() :: #{
    error_code := integer(),
    partition_index := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    replica_nodes := list(integer()),
    isr_nodes := list(integer()),
    offline_replicas := list(integer())
}.
-type metadata_response_topic_12() :: #{
    error_code := integer(),
    name := binary() | null,
    topic_id := kafcod:uuid(),
    is_internal := boolean(),
    partitions := list(metadata_response_partition_12()),
    topic_authorized_operations := integer()
}.
