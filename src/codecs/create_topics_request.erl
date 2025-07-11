-module(create_topics_request).
-export([
    encode_create_topics_request_0/1,
    decode_create_topics_request_0/1,
    encode_create_topics_request_1/1,
    decode_create_topics_request_1/1,
    encode_create_topics_request_2/1,
    decode_create_topics_request_2/1,
    encode_create_topics_request_3/1,
    decode_create_topics_request_3/1,
    encode_create_topics_request_4/1,
    decode_create_topics_request_4/1,
    encode_create_topics_request_5/1,
    decode_create_topics_request_5/1,
    encode_create_topics_request_6/1,
    decode_create_topics_request_6/1,
    encode_create_topics_request_7/1,
    decode_create_topics_request_7/1
]).
-export_type([
    create_topics_request_0/0,
    creatable_replica_assignment_0/0,
    createable_topic_config_0/0,
    creatable_topic_0/0,
    create_topics_request_1/0,
    creatable_replica_assignment_1/0,
    createable_topic_config_1/0,
    creatable_topic_1/0,
    create_topics_request_2/0,
    creatable_replica_assignment_2/0,
    createable_topic_config_2/0,
    creatable_topic_2/0,
    create_topics_request_3/0,
    creatable_replica_assignment_3/0,
    createable_topic_config_3/0,
    creatable_topic_3/0,
    create_topics_request_4/0,
    creatable_replica_assignment_4/0,
    createable_topic_config_4/0,
    creatable_topic_4/0,
    create_topics_request_5/0,
    creatable_replica_assignment_5/0,
    createable_topic_config_5/0,
    creatable_topic_5/0,
    create_topics_request_6/0,
    creatable_replica_assignment_6/0,
    createable_topic_config_6/0,
    creatable_topic_6/0,
    create_topics_request_7/0,
    creatable_replica_assignment_7/0,
    createable_topic_config_7/0,
    creatable_topic_7/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(CREATE_TOPICS_REQUEST, 19).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_create_topics_request_0(create_topics_request_0()) -> iodata().

encode_create_topics_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to create.
        topics := Topics,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?CREATE_TOPICS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_creatable_topic_0/1),
        ?encode_int32(TimeoutMs)
    ];
encode_create_topics_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, creatable_topic_0},
        timeout_ms => int32
    }).

-spec decode_create_topics_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_request_0(),
    Rest :: binary().

decode_create_topics_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_0)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs
        },
        Bin2
    }.

-spec encode_creatable_replica_assignment_0(creatable_replica_assignment_0()) -> iodata().

encode_creatable_replica_assignment_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The brokers to place the partition on.
        broker_ids := BrokerIds
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(BrokerIds)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_array(BrokerIds, ?encode_int32_)
    ];
encode_creatable_replica_assignment_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        broker_ids => {array, int32}
    }).

-spec decode_creatable_replica_assignment_0(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_replica_assignment_0(),
    Rest :: binary().

decode_creatable_replica_assignment_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_array(BrokerIds, Bin1, Bin2, ?decode_int32_),
    {
        #{
            partition_index => PartitionIndex,
            broker_ids => BrokerIds
        },
        Bin2
    }.

-spec encode_createable_topic_config_0(createable_topic_config_0()) -> iodata().

encode_createable_topic_config_0(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value)
    ];
encode_createable_topic_config_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string
    }).

-spec decode_createable_topic_config_0(binary()) -> {Decoded, Rest} when
    Decoded :: createable_topic_config_0(),
    Rest :: binary().

decode_createable_topic_config_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    {
        #{
            name => Name,
            value => Value
        },
        Bin2
    }.

-spec encode_creatable_topic_0(creatable_topic_0()) -> iodata().

encode_creatable_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The number of partitions to create in the topic, or -1 if we are either specifying a manual partition assignment or using the default partitions.
        num_partitions := NumPartitions,
        % The number of replicas to create for each partition in the topic, or -1 if we are either specifying a manual partition assignment or using the default replication factor.
        replication_factor := ReplicationFactor,
        % The manual partition assignment, or the empty array if we are using automatic assignment.
        assignments := Assignments,
        % The custom topic configurations to set.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_array(Assignments),
    ?is_array(Configs)
->
    [
        ?encode_string(Name),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_array(Assignments, fun encode_creatable_replica_assignment_0/1),
        ?encode_array(Configs, fun encode_createable_topic_config_0/1)
    ];
encode_creatable_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        num_partitions => int32,
        replication_factor => int16,
        assignments => {array, creatable_replica_assignment_0},
        configs => {array, createable_topic_config_0}
    }).

-spec decode_creatable_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_0(),
    Rest :: binary().

decode_creatable_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int32(NumPartitions, Bin1, Bin2),
    ?_decode_int16(ReplicationFactor, Bin2, Bin3),
    ?_decode_array(Assignments, Bin3, Bin4, ?_decode_element(decode_creatable_replica_assignment_0)),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_createable_topic_config_0)),
    {
        #{
            name => Name,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            assignments => Assignments,
            configs => Configs
        },
        Bin5
    }.

-spec encode_create_topics_request_1(create_topics_request_1()) -> iodata().

encode_create_topics_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to create.
        topics := Topics,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % If true, check that the topics can be created as specified, but don't create anything.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_1(?CREATE_TOPICS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_creatable_topic_1/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly)
    ];
encode_create_topics_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, creatable_topic_1},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_topics_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_request_1(),
    Rest :: binary().

decode_create_topics_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_1)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    }.

-spec encode_creatable_replica_assignment_1(creatable_replica_assignment_1()) -> iodata().

encode_creatable_replica_assignment_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The brokers to place the partition on.
        broker_ids := BrokerIds
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(BrokerIds)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_array(BrokerIds, ?encode_int32_)
    ];
encode_creatable_replica_assignment_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        broker_ids => {array, int32}
    }).

-spec decode_creatable_replica_assignment_1(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_replica_assignment_1(),
    Rest :: binary().

decode_creatable_replica_assignment_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_array(BrokerIds, Bin1, Bin2, ?decode_int32_),
    {
        #{
            partition_index => PartitionIndex,
            broker_ids => BrokerIds
        },
        Bin2
    }.

-spec encode_createable_topic_config_1(createable_topic_config_1()) -> iodata().

encode_createable_topic_config_1(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value)
    ];
encode_createable_topic_config_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string
    }).

-spec decode_createable_topic_config_1(binary()) -> {Decoded, Rest} when
    Decoded :: createable_topic_config_1(),
    Rest :: binary().

decode_createable_topic_config_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    {
        #{
            name => Name,
            value => Value
        },
        Bin2
    }.

-spec encode_creatable_topic_1(creatable_topic_1()) -> iodata().

encode_creatable_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The number of partitions to create in the topic, or -1 if we are either specifying a manual partition assignment or using the default partitions.
        num_partitions := NumPartitions,
        % The number of replicas to create for each partition in the topic, or -1 if we are either specifying a manual partition assignment or using the default replication factor.
        replication_factor := ReplicationFactor,
        % The manual partition assignment, or the empty array if we are using automatic assignment.
        assignments := Assignments,
        % The custom topic configurations to set.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_array(Assignments),
    ?is_array(Configs)
->
    [
        ?encode_string(Name),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_array(Assignments, fun encode_creatable_replica_assignment_1/1),
        ?encode_array(Configs, fun encode_createable_topic_config_1/1)
    ];
encode_creatable_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        num_partitions => int32,
        replication_factor => int16,
        assignments => {array, creatable_replica_assignment_1},
        configs => {array, createable_topic_config_1}
    }).

-spec decode_creatable_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_1(),
    Rest :: binary().

decode_creatable_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int32(NumPartitions, Bin1, Bin2),
    ?_decode_int16(ReplicationFactor, Bin2, Bin3),
    ?_decode_array(Assignments, Bin3, Bin4, ?_decode_element(decode_creatable_replica_assignment_1)),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_createable_topic_config_1)),
    {
        #{
            name => Name,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            assignments => Assignments,
            configs => Configs
        },
        Bin5
    }.

-spec encode_create_topics_request_2(create_topics_request_2()) -> iodata().

encode_create_topics_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to create.
        topics := Topics,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % If true, check that the topics can be created as specified, but don't create anything.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_1(?CREATE_TOPICS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_creatable_topic_2/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly)
    ];
encode_create_topics_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, creatable_topic_2},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_topics_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_request_2(),
    Rest :: binary().

decode_create_topics_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_2)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    }.

-spec encode_creatable_replica_assignment_2(creatable_replica_assignment_2()) -> iodata().

encode_creatable_replica_assignment_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The brokers to place the partition on.
        broker_ids := BrokerIds
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(BrokerIds)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_array(BrokerIds, ?encode_int32_)
    ];
encode_creatable_replica_assignment_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        broker_ids => {array, int32}
    }).

-spec decode_creatable_replica_assignment_2(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_replica_assignment_2(),
    Rest :: binary().

decode_creatable_replica_assignment_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_array(BrokerIds, Bin1, Bin2, ?decode_int32_),
    {
        #{
            partition_index => PartitionIndex,
            broker_ids => BrokerIds
        },
        Bin2
    }.

-spec encode_createable_topic_config_2(createable_topic_config_2()) -> iodata().

encode_createable_topic_config_2(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value)
    ];
encode_createable_topic_config_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string
    }).

-spec decode_createable_topic_config_2(binary()) -> {Decoded, Rest} when
    Decoded :: createable_topic_config_2(),
    Rest :: binary().

decode_createable_topic_config_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    {
        #{
            name => Name,
            value => Value
        },
        Bin2
    }.

-spec encode_creatable_topic_2(creatable_topic_2()) -> iodata().

encode_creatable_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The number of partitions to create in the topic, or -1 if we are either specifying a manual partition assignment or using the default partitions.
        num_partitions := NumPartitions,
        % The number of replicas to create for each partition in the topic, or -1 if we are either specifying a manual partition assignment or using the default replication factor.
        replication_factor := ReplicationFactor,
        % The manual partition assignment, or the empty array if we are using automatic assignment.
        assignments := Assignments,
        % The custom topic configurations to set.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_array(Assignments),
    ?is_array(Configs)
->
    [
        ?encode_string(Name),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_array(Assignments, fun encode_creatable_replica_assignment_2/1),
        ?encode_array(Configs, fun encode_createable_topic_config_2/1)
    ];
encode_creatable_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        num_partitions => int32,
        replication_factor => int16,
        assignments => {array, creatable_replica_assignment_2},
        configs => {array, createable_topic_config_2}
    }).

-spec decode_creatable_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_2(),
    Rest :: binary().

decode_creatable_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int32(NumPartitions, Bin1, Bin2),
    ?_decode_int16(ReplicationFactor, Bin2, Bin3),
    ?_decode_array(Assignments, Bin3, Bin4, ?_decode_element(decode_creatable_replica_assignment_2)),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_createable_topic_config_2)),
    {
        #{
            name => Name,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            assignments => Assignments,
            configs => Configs
        },
        Bin5
    }.

-spec encode_create_topics_request_3(create_topics_request_3()) -> iodata().

encode_create_topics_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to create.
        topics := Topics,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % If true, check that the topics can be created as specified, but don't create anything.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_1(?CREATE_TOPICS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_creatable_topic_3/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly)
    ];
encode_create_topics_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, creatable_topic_3},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_topics_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_request_3(),
    Rest :: binary().

decode_create_topics_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_3)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    }.

-spec encode_creatable_replica_assignment_3(creatable_replica_assignment_3()) -> iodata().

encode_creatable_replica_assignment_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The brokers to place the partition on.
        broker_ids := BrokerIds
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(BrokerIds)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_array(BrokerIds, ?encode_int32_)
    ];
encode_creatable_replica_assignment_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        broker_ids => {array, int32}
    }).

-spec decode_creatable_replica_assignment_3(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_replica_assignment_3(),
    Rest :: binary().

decode_creatable_replica_assignment_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_array(BrokerIds, Bin1, Bin2, ?decode_int32_),
    {
        #{
            partition_index => PartitionIndex,
            broker_ids => BrokerIds
        },
        Bin2
    }.

-spec encode_createable_topic_config_3(createable_topic_config_3()) -> iodata().

encode_createable_topic_config_3(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value)
    ];
encode_createable_topic_config_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string
    }).

-spec decode_createable_topic_config_3(binary()) -> {Decoded, Rest} when
    Decoded :: createable_topic_config_3(),
    Rest :: binary().

decode_createable_topic_config_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    {
        #{
            name => Name,
            value => Value
        },
        Bin2
    }.

-spec encode_creatable_topic_3(creatable_topic_3()) -> iodata().

encode_creatable_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The number of partitions to create in the topic, or -1 if we are either specifying a manual partition assignment or using the default partitions.
        num_partitions := NumPartitions,
        % The number of replicas to create for each partition in the topic, or -1 if we are either specifying a manual partition assignment or using the default replication factor.
        replication_factor := ReplicationFactor,
        % The manual partition assignment, or the empty array if we are using automatic assignment.
        assignments := Assignments,
        % The custom topic configurations to set.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_array(Assignments),
    ?is_array(Configs)
->
    [
        ?encode_string(Name),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_array(Assignments, fun encode_creatable_replica_assignment_3/1),
        ?encode_array(Configs, fun encode_createable_topic_config_3/1)
    ];
encode_creatable_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        num_partitions => int32,
        replication_factor => int16,
        assignments => {array, creatable_replica_assignment_3},
        configs => {array, createable_topic_config_3}
    }).

-spec decode_creatable_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_3(),
    Rest :: binary().

decode_creatable_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int32(NumPartitions, Bin1, Bin2),
    ?_decode_int16(ReplicationFactor, Bin2, Bin3),
    ?_decode_array(Assignments, Bin3, Bin4, ?_decode_element(decode_creatable_replica_assignment_3)),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_createable_topic_config_3)),
    {
        #{
            name => Name,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            assignments => Assignments,
            configs => Configs
        },
        Bin5
    }.

-spec encode_create_topics_request_4(create_topics_request_4()) -> iodata().

encode_create_topics_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to create.
        topics := Topics,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % If true, check that the topics can be created as specified, but don't create anything.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_1(?CREATE_TOPICS_REQUEST, 4, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_creatable_topic_4/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly)
    ];
encode_create_topics_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, creatable_topic_4},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_topics_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_request_4(),
    Rest :: binary().

decode_create_topics_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_4)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    }.

-spec encode_creatable_replica_assignment_4(creatable_replica_assignment_4()) -> iodata().

encode_creatable_replica_assignment_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The brokers to place the partition on.
        broker_ids := BrokerIds
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(BrokerIds)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_array(BrokerIds, ?encode_int32_)
    ];
encode_creatable_replica_assignment_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        broker_ids => {array, int32}
    }).

-spec decode_creatable_replica_assignment_4(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_replica_assignment_4(),
    Rest :: binary().

decode_creatable_replica_assignment_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_array(BrokerIds, Bin1, Bin2, ?decode_int32_),
    {
        #{
            partition_index => PartitionIndex,
            broker_ids => BrokerIds
        },
        Bin2
    }.

-spec encode_createable_topic_config_4(createable_topic_config_4()) -> iodata().

encode_createable_topic_config_4(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value)
    ];
encode_createable_topic_config_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string
    }).

-spec decode_createable_topic_config_4(binary()) -> {Decoded, Rest} when
    Decoded :: createable_topic_config_4(),
    Rest :: binary().

decode_createable_topic_config_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    {
        #{
            name => Name,
            value => Value
        },
        Bin2
    }.

-spec encode_creatable_topic_4(creatable_topic_4()) -> iodata().

encode_creatable_topic_4(
    _Args = #{
        % The topic name.
        name := Name,
        % The number of partitions to create in the topic, or -1 if we are either specifying a manual partition assignment or using the default partitions.
        num_partitions := NumPartitions,
        % The number of replicas to create for each partition in the topic, or -1 if we are either specifying a manual partition assignment or using the default replication factor.
        replication_factor := ReplicationFactor,
        % The manual partition assignment, or the empty array if we are using automatic assignment.
        assignments := Assignments,
        % The custom topic configurations to set.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_array(Assignments),
    ?is_array(Configs)
->
    [
        ?encode_string(Name),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_array(Assignments, fun encode_creatable_replica_assignment_4/1),
        ?encode_array(Configs, fun encode_createable_topic_config_4/1)
    ];
encode_creatable_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        num_partitions => int32,
        replication_factor => int16,
        assignments => {array, creatable_replica_assignment_4},
        configs => {array, createable_topic_config_4}
    }).

-spec decode_creatable_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_4(),
    Rest :: binary().

decode_creatable_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int32(NumPartitions, Bin1, Bin2),
    ?_decode_int16(ReplicationFactor, Bin2, Bin3),
    ?_decode_array(Assignments, Bin3, Bin4, ?_decode_element(decode_creatable_replica_assignment_4)),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_createable_topic_config_4)),
    {
        #{
            name => Name,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            assignments => Assignments,
            configs => Configs
        },
        Bin5
    }.

-spec encode_create_topics_request_5(create_topics_request_5()) -> iodata().

encode_create_topics_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to create.
        topics := Topics,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % If true, check that the topics can be created as specified, but don't create anything.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_2(?CREATE_TOPICS_REQUEST, 5, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_creatable_topic_5/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_topics_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, creatable_topic_5},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_topics_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_request_5(),
    Rest :: binary().

decode_create_topics_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_5)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_create_topics_request_5_tagged_field/3,
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    ).

-spec decode_create_topics_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: create_topics_request_5().

decode_create_topics_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_replica_assignment_5(creatable_replica_assignment_5()) -> iodata().

encode_creatable_replica_assignment_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The brokers to place the partition on.
        broker_ids := BrokerIds
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(BrokerIds)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_compact_array(BrokerIds, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_replica_assignment_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        broker_ids => {array, int32}
    }).

-spec decode_creatable_replica_assignment_5(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_replica_assignment_5(),
    Rest :: binary().

decode_creatable_replica_assignment_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_compact_array(BrokerIds, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_creatable_replica_assignment_5_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            broker_ids => BrokerIds
        },
        Bin2
    ).

-spec decode_creatable_replica_assignment_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_replica_assignment_5().

decode_creatable_replica_assignment_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_createable_topic_config_5(createable_topic_config_5()) -> iodata().

encode_createable_topic_config_5(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_nullable_string(Value),
        ?EMPTY_TAG_BUFFER
    ];
encode_createable_topic_config_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string
    }).

-spec decode_createable_topic_config_5(binary()) -> {Decoded, Rest} when
    Decoded :: createable_topic_config_5(),
    Rest :: binary().

decode_createable_topic_config_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_nullable_string(Value, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_createable_topic_config_5_tagged_field/3,
        #{
            name => Name,
            value => Value
        },
        Bin2
    ).

-spec decode_createable_topic_config_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: createable_topic_config_5().

decode_createable_topic_config_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_5(creatable_topic_5()) -> iodata().

encode_creatable_topic_5(
    _Args = #{
        % The topic name.
        name := Name,
        % The number of partitions to create in the topic, or -1 if we are either specifying a manual partition assignment or using the default partitions.
        num_partitions := NumPartitions,
        % The number of replicas to create for each partition in the topic, or -1 if we are either specifying a manual partition assignment or using the default replication factor.
        replication_factor := ReplicationFactor,
        % The manual partition assignment, or the empty array if we are using automatic assignment.
        assignments := Assignments,
        % The custom topic configurations to set.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_array(Assignments),
    ?is_array(Configs)
->
    [
        ?encode_compact_string(Name),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_compact_array(Assignments, fun encode_creatable_replica_assignment_5/1),
        ?encode_compact_array(Configs, fun encode_createable_topic_config_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_topic_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        num_partitions => int32,
        replication_factor => int16,
        assignments => {array, creatable_replica_assignment_5},
        configs => {array, createable_topic_config_5}
    }).

-spec decode_creatable_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_5(),
    Rest :: binary().

decode_creatable_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int32(NumPartitions, Bin1, Bin2),
    ?_decode_int16(ReplicationFactor, Bin2, Bin3),
    ?_decode_compact_array(Assignments, Bin3, Bin4, ?_decode_element(decode_creatable_replica_assignment_5)),
    ?_decode_compact_array(Configs, Bin4, Bin5, ?_decode_element(decode_createable_topic_config_5)),
    ?decode_tagged_fields(
        fun decode_creatable_topic_5_tagged_field/3,
        #{
            name => Name,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            assignments => Assignments,
            configs => Configs
        },
        Bin5
    ).

-spec decode_creatable_topic_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_5().

decode_creatable_topic_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_topics_request_6(create_topics_request_6()) -> iodata().

encode_create_topics_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to create.
        topics := Topics,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % If true, check that the topics can be created as specified, but don't create anything.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_2(?CREATE_TOPICS_REQUEST, 6, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_creatable_topic_6/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_topics_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, creatable_topic_6},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_topics_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_request_6(),
    Rest :: binary().

decode_create_topics_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_6)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_create_topics_request_6_tagged_field/3,
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    ).

-spec decode_create_topics_request_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: create_topics_request_6().

decode_create_topics_request_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_replica_assignment_6(creatable_replica_assignment_6()) -> iodata().

encode_creatable_replica_assignment_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The brokers to place the partition on.
        broker_ids := BrokerIds
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(BrokerIds)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_compact_array(BrokerIds, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_replica_assignment_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        broker_ids => {array, int32}
    }).

-spec decode_creatable_replica_assignment_6(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_replica_assignment_6(),
    Rest :: binary().

decode_creatable_replica_assignment_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_compact_array(BrokerIds, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_creatable_replica_assignment_6_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            broker_ids => BrokerIds
        },
        Bin2
    ).

-spec decode_creatable_replica_assignment_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_replica_assignment_6().

decode_creatable_replica_assignment_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_createable_topic_config_6(createable_topic_config_6()) -> iodata().

encode_createable_topic_config_6(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_nullable_string(Value),
        ?EMPTY_TAG_BUFFER
    ];
encode_createable_topic_config_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string
    }).

-spec decode_createable_topic_config_6(binary()) -> {Decoded, Rest} when
    Decoded :: createable_topic_config_6(),
    Rest :: binary().

decode_createable_topic_config_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_nullable_string(Value, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_createable_topic_config_6_tagged_field/3,
        #{
            name => Name,
            value => Value
        },
        Bin2
    ).

-spec decode_createable_topic_config_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: createable_topic_config_6().

decode_createable_topic_config_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_6(creatable_topic_6()) -> iodata().

encode_creatable_topic_6(
    _Args = #{
        % The topic name.
        name := Name,
        % The number of partitions to create in the topic, or -1 if we are either specifying a manual partition assignment or using the default partitions.
        num_partitions := NumPartitions,
        % The number of replicas to create for each partition in the topic, or -1 if we are either specifying a manual partition assignment or using the default replication factor.
        replication_factor := ReplicationFactor,
        % The manual partition assignment, or the empty array if we are using automatic assignment.
        assignments := Assignments,
        % The custom topic configurations to set.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_array(Assignments),
    ?is_array(Configs)
->
    [
        ?encode_compact_string(Name),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_compact_array(Assignments, fun encode_creatable_replica_assignment_6/1),
        ?encode_compact_array(Configs, fun encode_createable_topic_config_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_topic_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        num_partitions => int32,
        replication_factor => int16,
        assignments => {array, creatable_replica_assignment_6},
        configs => {array, createable_topic_config_6}
    }).

-spec decode_creatable_topic_6(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_6(),
    Rest :: binary().

decode_creatable_topic_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int32(NumPartitions, Bin1, Bin2),
    ?_decode_int16(ReplicationFactor, Bin2, Bin3),
    ?_decode_compact_array(Assignments, Bin3, Bin4, ?_decode_element(decode_creatable_replica_assignment_6)),
    ?_decode_compact_array(Configs, Bin4, Bin5, ?_decode_element(decode_createable_topic_config_6)),
    ?decode_tagged_fields(
        fun decode_creatable_topic_6_tagged_field/3,
        #{
            name => Name,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            assignments => Assignments,
            configs => Configs
        },
        Bin5
    ).

-spec decode_creatable_topic_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_6().

decode_creatable_topic_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_topics_request_7(create_topics_request_7()) -> iodata().

encode_create_topics_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to create.
        topics := Topics,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % If true, check that the topics can be created as specified, but don't create anything.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_2(?CREATE_TOPICS_REQUEST, 7, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_creatable_topic_7/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_topics_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, creatable_topic_7},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_topics_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_request_7(),
    Rest :: binary().

decode_create_topics_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_7)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_create_topics_request_7_tagged_field/3,
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    ).

-spec decode_create_topics_request_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: create_topics_request_7().

decode_create_topics_request_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_replica_assignment_7(creatable_replica_assignment_7()) -> iodata().

encode_creatable_replica_assignment_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The brokers to place the partition on.
        broker_ids := BrokerIds
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(BrokerIds)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_compact_array(BrokerIds, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_replica_assignment_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        broker_ids => {array, int32}
    }).

-spec decode_creatable_replica_assignment_7(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_replica_assignment_7(),
    Rest :: binary().

decode_creatable_replica_assignment_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_compact_array(BrokerIds, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_creatable_replica_assignment_7_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            broker_ids => BrokerIds
        },
        Bin2
    ).

-spec decode_creatable_replica_assignment_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_replica_assignment_7().

decode_creatable_replica_assignment_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_createable_topic_config_7(createable_topic_config_7()) -> iodata().

encode_createable_topic_config_7(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_nullable_string(Value),
        ?EMPTY_TAG_BUFFER
    ];
encode_createable_topic_config_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string
    }).

-spec decode_createable_topic_config_7(binary()) -> {Decoded, Rest} when
    Decoded :: createable_topic_config_7(),
    Rest :: binary().

decode_createable_topic_config_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_nullable_string(Value, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_createable_topic_config_7_tagged_field/3,
        #{
            name => Name,
            value => Value
        },
        Bin2
    ).

-spec decode_createable_topic_config_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: createable_topic_config_7().

decode_createable_topic_config_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_7(creatable_topic_7()) -> iodata().

encode_creatable_topic_7(
    _Args = #{
        % The topic name.
        name := Name,
        % The number of partitions to create in the topic, or -1 if we are either specifying a manual partition assignment or using the default partitions.
        num_partitions := NumPartitions,
        % The number of replicas to create for each partition in the topic, or -1 if we are either specifying a manual partition assignment or using the default replication factor.
        replication_factor := ReplicationFactor,
        % The manual partition assignment, or the empty array if we are using automatic assignment.
        assignments := Assignments,
        % The custom topic configurations to set.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_array(Assignments),
    ?is_array(Configs)
->
    [
        ?encode_compact_string(Name),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_compact_array(Assignments, fun encode_creatable_replica_assignment_7/1),
        ?encode_compact_array(Configs, fun encode_createable_topic_config_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_topic_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        num_partitions => int32,
        replication_factor => int16,
        assignments => {array, creatable_replica_assignment_7},
        configs => {array, createable_topic_config_7}
    }).

-spec decode_creatable_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_7(),
    Rest :: binary().

decode_creatable_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int32(NumPartitions, Bin1, Bin2),
    ?_decode_int16(ReplicationFactor, Bin2, Bin3),
    ?_decode_compact_array(Assignments, Bin3, Bin4, ?_decode_element(decode_creatable_replica_assignment_7)),
    ?_decode_compact_array(Configs, Bin4, Bin5, ?_decode_element(decode_createable_topic_config_7)),
    ?decode_tagged_fields(
        fun decode_creatable_topic_7_tagged_field/3,
        #{
            name => Name,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            assignments => Assignments,
            configs => Configs
        },
        Bin5
    ).

-spec decode_creatable_topic_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_7().

decode_creatable_topic_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type create_topics_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(creatable_topic_0()),
    timeout_ms := integer()
}.
-type creatable_replica_assignment_0() :: #{
    partition_index := integer(),
    broker_ids := list(integer())
}.
-type createable_topic_config_0() :: #{
    name := binary(),
    value := binary() | null
}.
-type creatable_topic_0() :: #{
    name := binary(),
    num_partitions := integer(),
    replication_factor := integer(),
    assignments := list(creatable_replica_assignment_0()),
    configs := list(createable_topic_config_0())
}.
-type create_topics_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(creatable_topic_1()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type creatable_replica_assignment_1() :: #{
    partition_index := integer(),
    broker_ids := list(integer())
}.
-type createable_topic_config_1() :: #{
    name := binary(),
    value := binary() | null
}.
-type creatable_topic_1() :: #{
    name := binary(),
    num_partitions := integer(),
    replication_factor := integer(),
    assignments := list(creatable_replica_assignment_1()),
    configs := list(createable_topic_config_1())
}.
-type create_topics_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(creatable_topic_2()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type creatable_replica_assignment_2() :: #{
    partition_index := integer(),
    broker_ids := list(integer())
}.
-type createable_topic_config_2() :: #{
    name := binary(),
    value := binary() | null
}.
-type creatable_topic_2() :: #{
    name := binary(),
    num_partitions := integer(),
    replication_factor := integer(),
    assignments := list(creatable_replica_assignment_2()),
    configs := list(createable_topic_config_2())
}.
-type create_topics_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(creatable_topic_3()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type creatable_replica_assignment_3() :: #{
    partition_index := integer(),
    broker_ids := list(integer())
}.
-type createable_topic_config_3() :: #{
    name := binary(),
    value := binary() | null
}.
-type creatable_topic_3() :: #{
    name := binary(),
    num_partitions := integer(),
    replication_factor := integer(),
    assignments := list(creatable_replica_assignment_3()),
    configs := list(createable_topic_config_3())
}.
-type create_topics_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(creatable_topic_4()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type creatable_replica_assignment_4() :: #{
    partition_index := integer(),
    broker_ids := list(integer())
}.
-type createable_topic_config_4() :: #{
    name := binary(),
    value := binary() | null
}.
-type creatable_topic_4() :: #{
    name := binary(),
    num_partitions := integer(),
    replication_factor := integer(),
    assignments := list(creatable_replica_assignment_4()),
    configs := list(createable_topic_config_4())
}.
-type create_topics_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(creatable_topic_5()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type creatable_replica_assignment_5() :: #{
    partition_index := integer(),
    broker_ids := list(integer())
}.
-type createable_topic_config_5() :: #{
    name := binary(),
    value := binary() | null
}.
-type creatable_topic_5() :: #{
    name := binary(),
    num_partitions := integer(),
    replication_factor := integer(),
    assignments := list(creatable_replica_assignment_5()),
    configs := list(createable_topic_config_5())
}.
-type create_topics_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(creatable_topic_6()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type creatable_replica_assignment_6() :: #{
    partition_index := integer(),
    broker_ids := list(integer())
}.
-type createable_topic_config_6() :: #{
    name := binary(),
    value := binary() | null
}.
-type creatable_topic_6() :: #{
    name := binary(),
    num_partitions := integer(),
    replication_factor := integer(),
    assignments := list(creatable_replica_assignment_6()),
    configs := list(createable_topic_config_6())
}.
-type create_topics_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(creatable_topic_7()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type creatable_replica_assignment_7() :: #{
    partition_index := integer(),
    broker_ids := list(integer())
}.
-type createable_topic_config_7() :: #{
    name := binary(),
    value := binary() | null
}.
-type creatable_topic_7() :: #{
    name := binary(),
    num_partitions := integer(),
    replication_factor := integer(),
    assignments := list(creatable_replica_assignment_7()),
    configs := list(createable_topic_config_7())
}.
