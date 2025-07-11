-module(list_offsets_request).
-export([
    encode_list_offsets_request_0/1,
    decode_list_offsets_request_0/1,
    encode_list_offsets_request_1/1,
    decode_list_offsets_request_1/1,
    encode_list_offsets_request_2/1,
    decode_list_offsets_request_2/1,
    encode_list_offsets_request_3/1,
    decode_list_offsets_request_3/1,
    encode_list_offsets_request_4/1,
    decode_list_offsets_request_4/1,
    encode_list_offsets_request_5/1,
    decode_list_offsets_request_5/1,
    encode_list_offsets_request_6/1,
    decode_list_offsets_request_6/1,
    encode_list_offsets_request_7/1,
    decode_list_offsets_request_7/1,
    encode_list_offsets_request_8/1,
    decode_list_offsets_request_8/1
]).
-export_type([
    list_offsets_request_0/0,
    list_offsets_partition_0/0,
    list_offsets_topic_0/0,
    list_offsets_request_1/0,
    list_offsets_partition_1/0,
    list_offsets_topic_1/0,
    list_offsets_request_2/0,
    list_offsets_partition_2/0,
    list_offsets_topic_2/0,
    list_offsets_request_3/0,
    list_offsets_partition_3/0,
    list_offsets_topic_3/0,
    list_offsets_request_4/0,
    list_offsets_partition_4/0,
    list_offsets_topic_4/0,
    list_offsets_request_5/0,
    list_offsets_partition_5/0,
    list_offsets_topic_5/0,
    list_offsets_request_6/0,
    list_offsets_partition_6/0,
    list_offsets_topic_6/0,
    list_offsets_request_7/0,
    list_offsets_partition_7/0,
    list_offsets_topic_7/0,
    list_offsets_request_8/0,
    list_offsets_partition_8/0,
    list_offsets_topic_8/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(LIST_OFFSETS_REQUEST, 2).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_offsets_request_0(list_offsets_request_0()) -> iodata().

encode_list_offsets_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?LIST_OFFSETS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_array(Topics, fun encode_list_offsets_topic_0/1)
    ];
encode_list_offsets_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        topics => {array, list_offsets_topic_0}
    }).

-spec decode_list_offsets_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_0(),
    Rest :: binary().

decode_list_offsets_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_0)),
    {
        Header#{
            replica_id => ReplicaId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_list_offsets_partition_0(list_offsets_partition_0()) -> iodata().

encode_list_offsets_partition_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current timestamp.
        timestamp := Timestamp,
        % The maximum number of offsets to report.
        max_num_offsets := MaxNumOffsets
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(Timestamp),
    ?is_int32(MaxNumOffsets)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(Timestamp),
        ?encode_int32(MaxNumOffsets)
    ];
encode_list_offsets_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        timestamp => int64,
        max_num_offsets => int32
    }).

-spec decode_list_offsets_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_0(),
    Rest :: binary().

decode_list_offsets_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(Timestamp, Bin1, Bin2),
    ?_decode_int32(MaxNumOffsets, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            timestamp => Timestamp,
            max_num_offsets => MaxNumOffsets
        },
        Bin3
    }.

-spec encode_list_offsets_topic_0(list_offsets_topic_0()) -> iodata().

encode_list_offsets_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_0/1)
    ];
encode_list_offsets_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_0}
    }).

-spec decode_list_offsets_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_0(),
    Rest :: binary().

decode_list_offsets_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_request_1(list_offsets_request_1()) -> iodata().

encode_list_offsets_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?LIST_OFFSETS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_array(Topics, fun encode_list_offsets_topic_1/1)
    ];
encode_list_offsets_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        topics => {array, list_offsets_topic_1}
    }).

-spec decode_list_offsets_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_1(),
    Rest :: binary().

decode_list_offsets_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_offsets_topic_1)),
    {
        Header#{
            replica_id => ReplicaId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_list_offsets_partition_1(list_offsets_partition_1()) -> iodata().

encode_list_offsets_partition_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current timestamp.
        timestamp := Timestamp
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(Timestamp)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(Timestamp)
    ];
encode_list_offsets_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        timestamp => int64
    }).

-spec decode_list_offsets_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_1(),
    Rest :: binary().

decode_list_offsets_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(Timestamp, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            timestamp => Timestamp
        },
        Bin2
    }.

-spec encode_list_offsets_topic_1(list_offsets_topic_1()) -> iodata().

encode_list_offsets_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_1/1)
    ];
encode_list_offsets_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_1}
    }).

-spec decode_list_offsets_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_1(),
    Rest :: binary().

decode_list_offsets_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_request_2(list_offsets_request_2()) -> iodata().

encode_list_offsets_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?LIST_OFFSETS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int8(IsolationLevel),
        ?encode_array(Topics, fun encode_list_offsets_topic_2/1)
    ];
encode_list_offsets_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        isolation_level => int8,
        topics => {array, list_offsets_topic_2}
    }).

-spec decode_list_offsets_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_2(),
    Rest :: binary().

decode_list_offsets_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int8(IsolationLevel, Bin1, Bin2),
    ?_decode_array(Topics, Bin2, Bin3, ?_decode_element(decode_list_offsets_topic_2)),
    {
        Header#{
            replica_id => ReplicaId,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin3
    }.

-spec encode_list_offsets_partition_2(list_offsets_partition_2()) -> iodata().

encode_list_offsets_partition_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current timestamp.
        timestamp := Timestamp
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(Timestamp)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(Timestamp)
    ];
encode_list_offsets_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        timestamp => int64
    }).

-spec decode_list_offsets_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_2(),
    Rest :: binary().

decode_list_offsets_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(Timestamp, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            timestamp => Timestamp
        },
        Bin2
    }.

-spec encode_list_offsets_topic_2(list_offsets_topic_2()) -> iodata().

encode_list_offsets_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_2/1)
    ];
encode_list_offsets_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_2}
    }).

-spec decode_list_offsets_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_2(),
    Rest :: binary().

decode_list_offsets_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_2)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_request_3(list_offsets_request_3()) -> iodata().

encode_list_offsets_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?LIST_OFFSETS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int8(IsolationLevel),
        ?encode_array(Topics, fun encode_list_offsets_topic_3/1)
    ];
encode_list_offsets_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        isolation_level => int8,
        topics => {array, list_offsets_topic_3}
    }).

-spec decode_list_offsets_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_3(),
    Rest :: binary().

decode_list_offsets_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int8(IsolationLevel, Bin1, Bin2),
    ?_decode_array(Topics, Bin2, Bin3, ?_decode_element(decode_list_offsets_topic_3)),
    {
        Header#{
            replica_id => ReplicaId,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin3
    }.

-spec encode_list_offsets_partition_3(list_offsets_partition_3()) -> iodata().

encode_list_offsets_partition_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current timestamp.
        timestamp := Timestamp
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(Timestamp)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(Timestamp)
    ];
encode_list_offsets_partition_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        timestamp => int64
    }).

-spec decode_list_offsets_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_3(),
    Rest :: binary().

decode_list_offsets_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(Timestamp, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            timestamp => Timestamp
        },
        Bin2
    }.

-spec encode_list_offsets_topic_3(list_offsets_topic_3()) -> iodata().

encode_list_offsets_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_3/1)
    ];
encode_list_offsets_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_3}
    }).

-spec decode_list_offsets_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_3(),
    Rest :: binary().

decode_list_offsets_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_3)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_request_4(list_offsets_request_4()) -> iodata().

encode_list_offsets_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?LIST_OFFSETS_REQUEST, 4, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int8(IsolationLevel),
        ?encode_array(Topics, fun encode_list_offsets_topic_4/1)
    ];
encode_list_offsets_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        isolation_level => int8,
        topics => {array, list_offsets_topic_4}
    }).

-spec decode_list_offsets_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_4(),
    Rest :: binary().

decode_list_offsets_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int8(IsolationLevel, Bin1, Bin2),
    ?_decode_array(Topics, Bin2, Bin3, ?_decode_element(decode_list_offsets_topic_4)),
    {
        Header#{
            replica_id => ReplicaId,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin3
    }.

-spec encode_list_offsets_partition_4(list_offsets_partition_4()) -> iodata().

encode_list_offsets_partition_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current leader epoch.
        current_leader_epoch := CurrentLeaderEpoch,
        % The current timestamp.
        timestamp := Timestamp
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(Timestamp)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(Timestamp)
    ];
encode_list_offsets_partition_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        current_leader_epoch => int32,
        timestamp => int64
    }).

-spec decode_list_offsets_partition_4(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_4(),
    Rest :: binary().

decode_list_offsets_partition_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            current_leader_epoch => CurrentLeaderEpoch,
            timestamp => Timestamp
        },
        Bin3
    }.

-spec encode_list_offsets_topic_4(list_offsets_topic_4()) -> iodata().

encode_list_offsets_topic_4(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_4/1)
    ];
encode_list_offsets_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_4}
    }).

-spec decode_list_offsets_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_4(),
    Rest :: binary().

decode_list_offsets_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_4)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_request_5(list_offsets_request_5()) -> iodata().

encode_list_offsets_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?LIST_OFFSETS_REQUEST, 5, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int8(IsolationLevel),
        ?encode_array(Topics, fun encode_list_offsets_topic_5/1)
    ];
encode_list_offsets_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        isolation_level => int8,
        topics => {array, list_offsets_topic_5}
    }).

-spec decode_list_offsets_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_5(),
    Rest :: binary().

decode_list_offsets_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int8(IsolationLevel, Bin1, Bin2),
    ?_decode_array(Topics, Bin2, Bin3, ?_decode_element(decode_list_offsets_topic_5)),
    {
        Header#{
            replica_id => ReplicaId,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin3
    }.

-spec encode_list_offsets_partition_5(list_offsets_partition_5()) -> iodata().

encode_list_offsets_partition_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current leader epoch.
        current_leader_epoch := CurrentLeaderEpoch,
        % The current timestamp.
        timestamp := Timestamp
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(Timestamp)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(Timestamp)
    ];
encode_list_offsets_partition_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        current_leader_epoch => int32,
        timestamp => int64
    }).

-spec decode_list_offsets_partition_5(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_5(),
    Rest :: binary().

decode_list_offsets_partition_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            current_leader_epoch => CurrentLeaderEpoch,
            timestamp => Timestamp
        },
        Bin3
    }.

-spec encode_list_offsets_topic_5(list_offsets_topic_5()) -> iodata().

encode_list_offsets_topic_5(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_list_offsets_partition_5/1)
    ];
encode_list_offsets_topic_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_5}
    }).

-spec decode_list_offsets_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_5(),
    Rest :: binary().

decode_list_offsets_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_5)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_list_offsets_request_6(list_offsets_request_6()) -> iodata().

encode_list_offsets_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?LIST_OFFSETS_REQUEST, 6, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int8(IsolationLevel),
        ?encode_compact_array(Topics, fun encode_list_offsets_topic_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        isolation_level => int8,
        topics => {array, list_offsets_topic_6}
    }).

-spec decode_list_offsets_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_6(),
    Rest :: binary().

decode_list_offsets_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int8(IsolationLevel, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_list_offsets_topic_6)),
    ?decode_tagged_fields(
        fun decode_list_offsets_request_6_tagged_field/3,
        Header#{
            replica_id => ReplicaId,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin3
    ).

-spec decode_list_offsets_request_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_request_6().

decode_list_offsets_request_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_partition_6(list_offsets_partition_6()) -> iodata().

encode_list_offsets_partition_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current leader epoch.
        current_leader_epoch := CurrentLeaderEpoch,
        % The current timestamp.
        timestamp := Timestamp
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(Timestamp)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(Timestamp),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_partition_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        current_leader_epoch => int32,
        timestamp => int64
    }).

-spec decode_list_offsets_partition_6(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_6(),
    Rest :: binary().

decode_list_offsets_partition_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_list_offsets_partition_6_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            current_leader_epoch => CurrentLeaderEpoch,
            timestamp => Timestamp
        },
        Bin3
    ).

-spec decode_list_offsets_partition_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_partition_6().

decode_list_offsets_partition_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_topic_6(list_offsets_topic_6()) -> iodata().

encode_list_offsets_topic_6(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_list_offsets_partition_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_topic_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_6}
    }).

-spec decode_list_offsets_topic_6(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_6(),
    Rest :: binary().

decode_list_offsets_topic_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_6)),
    ?decode_tagged_fields(
        fun decode_list_offsets_topic_6_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_list_offsets_topic_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_topic_6().

decode_list_offsets_topic_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_request_7(list_offsets_request_7()) -> iodata().

encode_list_offsets_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?LIST_OFFSETS_REQUEST, 7, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int8(IsolationLevel),
        ?encode_compact_array(Topics, fun encode_list_offsets_topic_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        isolation_level => int8,
        topics => {array, list_offsets_topic_7}
    }).

-spec decode_list_offsets_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_7(),
    Rest :: binary().

decode_list_offsets_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int8(IsolationLevel, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_list_offsets_topic_7)),
    ?decode_tagged_fields(
        fun decode_list_offsets_request_7_tagged_field/3,
        Header#{
            replica_id => ReplicaId,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin3
    ).

-spec decode_list_offsets_request_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_request_7().

decode_list_offsets_request_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_partition_7(list_offsets_partition_7()) -> iodata().

encode_list_offsets_partition_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current leader epoch.
        current_leader_epoch := CurrentLeaderEpoch,
        % The current timestamp.
        timestamp := Timestamp
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(Timestamp)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(Timestamp),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_partition_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        current_leader_epoch => int32,
        timestamp => int64
    }).

-spec decode_list_offsets_partition_7(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_7(),
    Rest :: binary().

decode_list_offsets_partition_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_list_offsets_partition_7_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            current_leader_epoch => CurrentLeaderEpoch,
            timestamp => Timestamp
        },
        Bin3
    ).

-spec decode_list_offsets_partition_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_partition_7().

decode_list_offsets_partition_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_topic_7(list_offsets_topic_7()) -> iodata().

encode_list_offsets_topic_7(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_list_offsets_partition_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_topic_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_7}
    }).

-spec decode_list_offsets_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_7(),
    Rest :: binary().

decode_list_offsets_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_7)),
    ?decode_tagged_fields(
        fun decode_list_offsets_topic_7_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_list_offsets_topic_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_topic_7().

decode_list_offsets_topic_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_request_8(list_offsets_request_8()) -> iodata().

encode_list_offsets_request_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the requester, or -1 if this request is being made by a normal consumer.
        replica_id := ReplicaId,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % Each topic in the request.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?LIST_OFFSETS_REQUEST, 8, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int8(IsolationLevel),
        ?encode_compact_array(Topics, fun encode_list_offsets_topic_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_request_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        isolation_level => int8,
        topics => {array, list_offsets_topic_8}
    }).

-spec decode_list_offsets_request_8(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_request_8(),
    Rest :: binary().

decode_list_offsets_request_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int8(IsolationLevel, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_list_offsets_topic_8)),
    ?decode_tagged_fields(
        fun decode_list_offsets_request_8_tagged_field/3,
        Header#{
            replica_id => ReplicaId,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin3
    ).

-spec decode_list_offsets_request_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_request_8().

decode_list_offsets_request_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_partition_8(list_offsets_partition_8()) -> iodata().

encode_list_offsets_partition_8(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The current leader epoch.
        current_leader_epoch := CurrentLeaderEpoch,
        % The current timestamp.
        timestamp := Timestamp
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(Timestamp)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(Timestamp),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_partition_8(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        current_leader_epoch => int32,
        timestamp => int64
    }).

-spec decode_list_offsets_partition_8(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_partition_8(),
    Rest :: binary().

decode_list_offsets_partition_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(Timestamp, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_list_offsets_partition_8_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            current_leader_epoch => CurrentLeaderEpoch,
            timestamp => Timestamp
        },
        Bin3
    ).

-spec decode_list_offsets_partition_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_partition_8().

decode_list_offsets_partition_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_offsets_topic_8(list_offsets_topic_8()) -> iodata().

encode_list_offsets_topic_8(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition in the request.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_list_offsets_partition_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_offsets_topic_8(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, list_offsets_partition_8}
    }).

-spec decode_list_offsets_topic_8(binary()) -> {Decoded, Rest} when
    Decoded :: list_offsets_topic_8(),
    Rest :: binary().

decode_list_offsets_topic_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_list_offsets_partition_8)),
    ?decode_tagged_fields(
        fun decode_list_offsets_topic_8_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_list_offsets_topic_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_offsets_topic_8().

decode_list_offsets_topic_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_offsets_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    topics := list(list_offsets_topic_0())
}.
-type list_offsets_partition_0() :: #{
    partition_index := integer(),
    timestamp := integer(),
    max_num_offsets := integer()
}.
-type list_offsets_topic_0() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_0())
}.
-type list_offsets_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    topics := list(list_offsets_topic_1())
}.
-type list_offsets_partition_1() :: #{
    partition_index := integer(),
    timestamp := integer()
}.
-type list_offsets_topic_1() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_1())
}.
-type list_offsets_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    isolation_level := integer(),
    topics := list(list_offsets_topic_2())
}.
-type list_offsets_partition_2() :: #{
    partition_index := integer(),
    timestamp := integer()
}.
-type list_offsets_topic_2() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_2())
}.
-type list_offsets_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    isolation_level := integer(),
    topics := list(list_offsets_topic_3())
}.
-type list_offsets_partition_3() :: #{
    partition_index := integer(),
    timestamp := integer()
}.
-type list_offsets_topic_3() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_3())
}.
-type list_offsets_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    isolation_level := integer(),
    topics := list(list_offsets_topic_4())
}.
-type list_offsets_partition_4() :: #{
    partition_index := integer(),
    current_leader_epoch := integer(),
    timestamp := integer()
}.
-type list_offsets_topic_4() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_4())
}.
-type list_offsets_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    isolation_level := integer(),
    topics := list(list_offsets_topic_5())
}.
-type list_offsets_partition_5() :: #{
    partition_index := integer(),
    current_leader_epoch := integer(),
    timestamp := integer()
}.
-type list_offsets_topic_5() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_5())
}.
-type list_offsets_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    isolation_level := integer(),
    topics := list(list_offsets_topic_6())
}.
-type list_offsets_partition_6() :: #{
    partition_index := integer(),
    current_leader_epoch := integer(),
    timestamp := integer()
}.
-type list_offsets_topic_6() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_6())
}.
-type list_offsets_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    isolation_level := integer(),
    topics := list(list_offsets_topic_7())
}.
-type list_offsets_partition_7() :: #{
    partition_index := integer(),
    current_leader_epoch := integer(),
    timestamp := integer()
}.
-type list_offsets_topic_7() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_7())
}.
-type list_offsets_request_8() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    isolation_level := integer(),
    topics := list(list_offsets_topic_8())
}.
-type list_offsets_partition_8() :: #{
    partition_index := integer(),
    current_leader_epoch := integer(),
    timestamp := integer()
}.
-type list_offsets_topic_8() :: #{
    name := binary(),
    partitions := list(list_offsets_partition_8())
}.
