-module(fetch_request).
-export([
    encode_fetch_request_0/1,
    decode_fetch_request_0/1,
    encode_fetch_request_1/1,
    decode_fetch_request_1/1,
    encode_fetch_request_2/1,
    decode_fetch_request_2/1,
    encode_fetch_request_3/1,
    decode_fetch_request_3/1,
    encode_fetch_request_4/1,
    decode_fetch_request_4/1,
    encode_fetch_request_5/1,
    decode_fetch_request_5/1,
    encode_fetch_request_6/1,
    decode_fetch_request_6/1,
    encode_fetch_request_7/1,
    decode_fetch_request_7/1,
    encode_fetch_request_8/1,
    decode_fetch_request_8/1,
    encode_fetch_request_9/1,
    decode_fetch_request_9/1,
    encode_fetch_request_10/1,
    decode_fetch_request_10/1,
    encode_fetch_request_11/1,
    decode_fetch_request_11/1,
    encode_fetch_request_12/1,
    decode_fetch_request_12/1,
    encode_fetch_request_13/1,
    decode_fetch_request_13/1,
    encode_fetch_request_14/1,
    decode_fetch_request_14/1,
    encode_fetch_request_15/1,
    decode_fetch_request_15/1
]).
-export_type([
    fetch_request_0/0,
    fetch_partition_0/0,
    fetch_topic_0/0,
    fetch_request_1/0,
    fetch_partition_1/0,
    fetch_topic_1/0,
    fetch_request_2/0,
    fetch_partition_2/0,
    fetch_topic_2/0,
    fetch_request_3/0,
    fetch_partition_3/0,
    fetch_topic_3/0,
    fetch_request_4/0,
    fetch_partition_4/0,
    fetch_topic_4/0,
    fetch_request_5/0,
    fetch_partition_5/0,
    fetch_topic_5/0,
    fetch_request_6/0,
    fetch_partition_6/0,
    fetch_topic_6/0,
    fetch_request_7/0,
    fetch_partition_7/0,
    fetch_topic_7/0,
    forgotten_topic_7/0,
    fetch_request_8/0,
    fetch_partition_8/0,
    fetch_topic_8/0,
    forgotten_topic_8/0,
    fetch_request_9/0,
    fetch_partition_9/0,
    fetch_topic_9/0,
    forgotten_topic_9/0,
    fetch_request_10/0,
    fetch_partition_10/0,
    fetch_topic_10/0,
    forgotten_topic_10/0,
    fetch_request_11/0,
    fetch_partition_11/0,
    fetch_topic_11/0,
    forgotten_topic_11/0,
    fetch_request_12/0,
    fetch_partition_12/0,
    fetch_topic_12/0,
    forgotten_topic_12/0,
    fetch_request_13/0,
    fetch_partition_13/0,
    fetch_topic_13/0,
    forgotten_topic_13/0,
    fetch_request_14/0,
    fetch_partition_14/0,
    fetch_topic_14/0,
    forgotten_topic_14/0,
    fetch_request_15/0,
    replica_state_15/0,
    fetch_partition_15/0,
    fetch_topic_15/0,
    forgotten_topic_15/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(FETCH_REQUEST, 1).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_fetch_request_0(fetch_request_0()) -> iodata().

encode_fetch_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The topics to fetch.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_array(Topics, fun encode_fetch_topic_0/1)
    ];
encode_fetch_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        topics => {array, fetch_topic_0}
    }).

-spec decode_fetch_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_0(),
    Rest :: binary().

decode_fetch_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_array(Topics, Bin3, Bin4, ?_decode_element(decode_fetch_topic_0)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            topics => Topics
        },
        Bin4
    }.

-spec encode_fetch_partition_0(fetch_partition_0()) -> iodata().

encode_fetch_partition_0(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_0(),
    Rest :: binary().

decode_fetch_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int32(PartitionMaxBytes, Bin2, Bin3),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin3
    }.

-spec encode_fetch_topic_0(fetch_topic_0()) -> iodata().

encode_fetch_topic_0(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_0/1)
    ];
encode_fetch_topic_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_0}
    }).

-spec decode_fetch_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_0(),
    Rest :: binary().

decode_fetch_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_0)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_1(fetch_request_1()) -> iodata().

encode_fetch_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The topics to fetch.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_array(Topics, fun encode_fetch_topic_1/1)
    ];
encode_fetch_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        topics => {array, fetch_topic_1}
    }).

-spec decode_fetch_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_1(),
    Rest :: binary().

decode_fetch_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_array(Topics, Bin3, Bin4, ?_decode_element(decode_fetch_topic_1)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            topics => Topics
        },
        Bin4
    }.

-spec encode_fetch_partition_1(fetch_partition_1()) -> iodata().

encode_fetch_partition_1(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_1(),
    Rest :: binary().

decode_fetch_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int32(PartitionMaxBytes, Bin2, Bin3),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin3
    }.

-spec encode_fetch_topic_1(fetch_topic_1()) -> iodata().

encode_fetch_topic_1(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_1/1)
    ];
encode_fetch_topic_1(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_1}
    }).

-spec decode_fetch_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_1(),
    Rest :: binary().

decode_fetch_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_1)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_2(fetch_request_2()) -> iodata().

encode_fetch_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The topics to fetch.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_array(Topics, fun encode_fetch_topic_2/1)
    ];
encode_fetch_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        topics => {array, fetch_topic_2}
    }).

-spec decode_fetch_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_2(),
    Rest :: binary().

decode_fetch_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_array(Topics, Bin3, Bin4, ?_decode_element(decode_fetch_topic_2)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            topics => Topics
        },
        Bin4
    }.

-spec encode_fetch_partition_2(fetch_partition_2()) -> iodata().

encode_fetch_partition_2(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_2(),
    Rest :: binary().

decode_fetch_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int32(PartitionMaxBytes, Bin2, Bin3),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin3
    }.

-spec encode_fetch_topic_2(fetch_topic_2()) -> iodata().

encode_fetch_topic_2(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_2/1)
    ];
encode_fetch_topic_2(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_2}
    }).

-spec decode_fetch_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_2(),
    Rest :: binary().

decode_fetch_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_2)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_3(fetch_request_3()) -> iodata().

encode_fetch_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % The topics to fetch.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_array(Topics, fun encode_fetch_topic_3/1)
    ];
encode_fetch_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        topics => {array, fetch_topic_3}
    }).

-spec decode_fetch_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_3(),
    Rest :: binary().

decode_fetch_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_fetch_topic_3)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            topics => Topics
        },
        Bin5
    }.

-spec encode_fetch_partition_3(fetch_partition_3()) -> iodata().

encode_fetch_partition_3(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_3(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_3(),
    Rest :: binary().

decode_fetch_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int32(PartitionMaxBytes, Bin2, Bin3),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin3
    }.

-spec encode_fetch_topic_3(fetch_topic_3()) -> iodata().

encode_fetch_topic_3(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_3/1)
    ];
encode_fetch_topic_3(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_3}
    }).

-spec decode_fetch_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_3(),
    Rest :: binary().

decode_fetch_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_3)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_4(fetch_request_4()) -> iodata().

encode_fetch_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The topics to fetch.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 4, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_array(Topics, fun encode_fetch_topic_4/1)
    ];
encode_fetch_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        topics => {array, fetch_topic_4}
    }).

-spec decode_fetch_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_4(),
    Rest :: binary().

decode_fetch_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_array(Topics, Bin5, Bin6, ?_decode_element(decode_fetch_topic_4)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin6
    }.

-spec encode_fetch_partition_4(fetch_partition_4()) -> iodata().

encode_fetch_partition_4(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_4(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_4(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_4(),
    Rest :: binary().

decode_fetch_partition_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int32(PartitionMaxBytes, Bin2, Bin3),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin3
    }.

-spec encode_fetch_topic_4(fetch_topic_4()) -> iodata().

encode_fetch_topic_4(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_4/1)
    ];
encode_fetch_topic_4(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_4}
    }).

-spec decode_fetch_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_4(),
    Rest :: binary().

decode_fetch_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_4)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_5(fetch_request_5()) -> iodata().

encode_fetch_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The topics to fetch.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 5, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_array(Topics, fun encode_fetch_topic_5/1)
    ];
encode_fetch_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        topics => {array, fetch_topic_5}
    }).

-spec decode_fetch_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_5(),
    Rest :: binary().

decode_fetch_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_array(Topics, Bin5, Bin6, ?_decode_element(decode_fetch_topic_5)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin6
    }.

-spec encode_fetch_partition_5(fetch_partition_5()) -> iodata().

encode_fetch_partition_5(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_5(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_5(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_5(),
    Rest :: binary().

decode_fetch_partition_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int64(LogStartOffset, Bin2, Bin3),
    ?_decode_int32(PartitionMaxBytes, Bin3, Bin4),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin4
    }.

-spec encode_fetch_topic_5(fetch_topic_5()) -> iodata().

encode_fetch_topic_5(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_5/1)
    ];
encode_fetch_topic_5(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_5}
    }).

-spec decode_fetch_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_5(),
    Rest :: binary().

decode_fetch_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_5)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_6(fetch_request_6()) -> iodata().

encode_fetch_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The topics to fetch.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 6, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_array(Topics, fun encode_fetch_topic_6/1)
    ];
encode_fetch_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        topics => {array, fetch_topic_6}
    }).

-spec decode_fetch_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_6(),
    Rest :: binary().

decode_fetch_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_array(Topics, Bin5, Bin6, ?_decode_element(decode_fetch_topic_6)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            topics => Topics
        },
        Bin6
    }.

-spec encode_fetch_partition_6(fetch_partition_6()) -> iodata().

encode_fetch_partition_6(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_6(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_6(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_6(),
    Rest :: binary().

decode_fetch_partition_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int64(LogStartOffset, Bin2, Bin3),
    ?_decode_int32(PartitionMaxBytes, Bin3, Bin4),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin4
    }.

-spec encode_fetch_topic_6(fetch_topic_6()) -> iodata().

encode_fetch_topic_6(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_6/1)
    ];
encode_fetch_topic_6(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_6}
    }).

-spec decode_fetch_topic_6(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_6(),
    Rest :: binary().

decode_fetch_topic_6(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_6)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_7(fetch_request_7()) -> iodata().

encode_fetch_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 7, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_array(Topics, fun encode_fetch_topic_7/1),
        ?encode_array(ForgottenTopicsData, fun encode_forgotten_topic_7/1)
    ];
encode_fetch_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_7},
        forgotten_topics_data => {array, forgotten_topic_7}
    }).

-spec decode_fetch_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_7(),
    Rest :: binary().

decode_fetch_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_int32(SessionId, Bin5, Bin6),
    ?_decode_int32(SessionEpoch, Bin6, Bin7),
    ?_decode_array(Topics, Bin7, Bin8, ?_decode_element(decode_fetch_topic_7)),
    ?_decode_array(ForgottenTopicsData, Bin8, Bin9, ?_decode_element(decode_forgotten_topic_7)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData
        },
        Bin9
    }.

-spec encode_fetch_partition_7(fetch_partition_7()) -> iodata().

encode_fetch_partition_7(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_7(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_7(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_7(),
    Rest :: binary().

decode_fetch_partition_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int64(LogStartOffset, Bin2, Bin3),
    ?_decode_int32(PartitionMaxBytes, Bin3, Bin4),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin4
    }.

-spec encode_fetch_topic_7(fetch_topic_7()) -> iodata().

encode_fetch_topic_7(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_7/1)
    ];
encode_fetch_topic_7(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_7}
    }).

-spec decode_fetch_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_7(),
    Rest :: binary().

decode_fetch_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_7)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_forgotten_topic_7(forgotten_topic_7()) -> iodata().

encode_forgotten_topic_7(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The partitions indexes to forget.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_forgotten_topic_7(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_7(),
    Rest :: binary().

decode_forgotten_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_8(fetch_request_8()) -> iodata().

encode_fetch_request_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 8, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_array(Topics, fun encode_fetch_topic_8/1),
        ?encode_array(ForgottenTopicsData, fun encode_forgotten_topic_8/1)
    ];
encode_fetch_request_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_8},
        forgotten_topics_data => {array, forgotten_topic_8}
    }).

-spec decode_fetch_request_8(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_8(),
    Rest :: binary().

decode_fetch_request_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_int32(SessionId, Bin5, Bin6),
    ?_decode_int32(SessionEpoch, Bin6, Bin7),
    ?_decode_array(Topics, Bin7, Bin8, ?_decode_element(decode_fetch_topic_8)),
    ?_decode_array(ForgottenTopicsData, Bin8, Bin9, ?_decode_element(decode_forgotten_topic_8)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData
        },
        Bin9
    }.

-spec encode_fetch_partition_8(fetch_partition_8()) -> iodata().

encode_fetch_partition_8(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The message offset.
        fetch_offset := FetchOffset,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int64(FetchOffset),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int64(FetchOffset),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_8(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        fetch_offset => int64,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_8(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_8(),
    Rest :: binary().

decode_fetch_partition_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int64(FetchOffset, Bin1, Bin2),
    ?_decode_int64(LogStartOffset, Bin2, Bin3),
    ?_decode_int32(PartitionMaxBytes, Bin3, Bin4),
    {
        #{
            partition => Partition,
            fetch_offset => FetchOffset,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin4
    }.

-spec encode_fetch_topic_8(fetch_topic_8()) -> iodata().

encode_fetch_topic_8(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_8/1)
    ];
encode_fetch_topic_8(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_8}
    }).

-spec decode_fetch_topic_8(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_8(),
    Rest :: binary().

decode_fetch_topic_8(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_8)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_forgotten_topic_8(forgotten_topic_8()) -> iodata().

encode_forgotten_topic_8(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The partitions indexes to forget.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_forgotten_topic_8(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_8(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_8(),
    Rest :: binary().

decode_forgotten_topic_8(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_9(fetch_request_9()) -> iodata().

encode_fetch_request_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 9, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_array(Topics, fun encode_fetch_topic_9/1),
        ?encode_array(ForgottenTopicsData, fun encode_forgotten_topic_9/1)
    ];
encode_fetch_request_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_9},
        forgotten_topics_data => {array, forgotten_topic_9}
    }).

-spec decode_fetch_request_9(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_9(),
    Rest :: binary().

decode_fetch_request_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_int32(SessionId, Bin5, Bin6),
    ?_decode_int32(SessionEpoch, Bin6, Bin7),
    ?_decode_array(Topics, Bin7, Bin8, ?_decode_element(decode_fetch_topic_9)),
    ?_decode_array(ForgottenTopicsData, Bin8, Bin9, ?_decode_element(decode_forgotten_topic_9)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData
        },
        Bin9
    }.

-spec encode_fetch_partition_9(fetch_partition_9()) -> iodata().

encode_fetch_partition_9(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The current leader epoch of the partition.
        current_leader_epoch := CurrentLeaderEpoch,
        % The message offset.
        fetch_offset := FetchOffset,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(FetchOffset),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(FetchOffset),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_9(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        fetch_offset => int64,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_9(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_9(),
    Rest :: binary().

decode_fetch_partition_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(FetchOffset, Bin2, Bin3),
    ?_decode_int64(LogStartOffset, Bin3, Bin4),
    ?_decode_int32(PartitionMaxBytes, Bin4, Bin5),
    {
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            fetch_offset => FetchOffset,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin5
    }.

-spec encode_fetch_topic_9(fetch_topic_9()) -> iodata().

encode_fetch_topic_9(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_9/1)
    ];
encode_fetch_topic_9(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_9}
    }).

-spec decode_fetch_topic_9(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_9(),
    Rest :: binary().

decode_fetch_topic_9(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_9)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_forgotten_topic_9(forgotten_topic_9()) -> iodata().

encode_forgotten_topic_9(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The partitions indexes to forget.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_forgotten_topic_9(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_9(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_9(),
    Rest :: binary().

decode_forgotten_topic_9(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_10(fetch_request_10()) -> iodata().

encode_fetch_request_10(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 10, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_array(Topics, fun encode_fetch_topic_10/1),
        ?encode_array(ForgottenTopicsData, fun encode_forgotten_topic_10/1)
    ];
encode_fetch_request_10(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_10},
        forgotten_topics_data => {array, forgotten_topic_10}
    }).

-spec decode_fetch_request_10(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_10(),
    Rest :: binary().

decode_fetch_request_10(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_int32(SessionId, Bin5, Bin6),
    ?_decode_int32(SessionEpoch, Bin6, Bin7),
    ?_decode_array(Topics, Bin7, Bin8, ?_decode_element(decode_fetch_topic_10)),
    ?_decode_array(ForgottenTopicsData, Bin8, Bin9, ?_decode_element(decode_forgotten_topic_10)),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData
        },
        Bin9
    }.

-spec encode_fetch_partition_10(fetch_partition_10()) -> iodata().

encode_fetch_partition_10(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The current leader epoch of the partition.
        current_leader_epoch := CurrentLeaderEpoch,
        % The message offset.
        fetch_offset := FetchOffset,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(FetchOffset),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(FetchOffset),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_10(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        fetch_offset => int64,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_10(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_10(),
    Rest :: binary().

decode_fetch_partition_10(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(FetchOffset, Bin2, Bin3),
    ?_decode_int64(LogStartOffset, Bin3, Bin4),
    ?_decode_int32(PartitionMaxBytes, Bin4, Bin5),
    {
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            fetch_offset => FetchOffset,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin5
    }.

-spec encode_fetch_topic_10(fetch_topic_10()) -> iodata().

encode_fetch_topic_10(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_10/1)
    ];
encode_fetch_topic_10(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_10}
    }).

-spec decode_fetch_topic_10(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_10(),
    Rest :: binary().

decode_fetch_topic_10(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_10)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_forgotten_topic_10(forgotten_topic_10()) -> iodata().

encode_forgotten_topic_10(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The partitions indexes to forget.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_forgotten_topic_10(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_10(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_10(),
    Rest :: binary().

decode_forgotten_topic_10(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_11(fetch_request_11()) -> iodata().

encode_fetch_request_11(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData,
        % Rack ID of the consumer making this request
        rack_id := RackId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData),
    ?is_string(RackId)
->
    [
        ?encode_request_header_1(?FETCH_REQUEST, 11, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_array(Topics, fun encode_fetch_topic_11/1),
        ?encode_array(ForgottenTopicsData, fun encode_forgotten_topic_11/1),
        ?encode_string(RackId)
    ];
encode_fetch_request_11(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_11},
        forgotten_topics_data => {array, forgotten_topic_11},
        rack_id => string
    }).

-spec decode_fetch_request_11(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_11(),
    Rest :: binary().

decode_fetch_request_11(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_int32(SessionId, Bin5, Bin6),
    ?_decode_int32(SessionEpoch, Bin6, Bin7),
    ?_decode_array(Topics, Bin7, Bin8, ?_decode_element(decode_fetch_topic_11)),
    ?_decode_array(ForgottenTopicsData, Bin8, Bin9, ?_decode_element(decode_forgotten_topic_11)),
    ?_decode_string(RackId, Bin9, Bin10),
    {
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData,
            rack_id => RackId
        },
        Bin10
    }.

-spec encode_fetch_partition_11(fetch_partition_11()) -> iodata().

encode_fetch_partition_11(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The current leader epoch of the partition.
        current_leader_epoch := CurrentLeaderEpoch,
        % The message offset.
        fetch_offset := FetchOffset,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(FetchOffset),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(FetchOffset),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes)
    ];
encode_fetch_partition_11(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        fetch_offset => int64,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_11(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_11(),
    Rest :: binary().

decode_fetch_partition_11(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(FetchOffset, Bin2, Bin3),
    ?_decode_int64(LogStartOffset, Bin3, Bin4),
    ?_decode_int32(PartitionMaxBytes, Bin4, Bin5),
    {
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            fetch_offset => FetchOffset,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin5
    }.

-spec encode_fetch_topic_11(fetch_topic_11()) -> iodata().

encode_fetch_topic_11(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_fetch_partition_11/1)
    ];
encode_fetch_topic_11(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_11}
    }).

-spec decode_fetch_topic_11(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_11(),
    Rest :: binary().

decode_fetch_topic_11(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_11)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_forgotten_topic_11(forgotten_topic_11()) -> iodata().

encode_forgotten_topic_11(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The partitions indexes to forget.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_forgotten_topic_11(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_11(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_11(),
    Rest :: binary().

decode_forgotten_topic_11(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_fetch_request_12(fetch_request_12()) -> iodata().

encode_fetch_request_12(
    Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData,
        % Rack ID of the consumer making this request
        rack_id := RackId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData),
    ?is_string(RackId)
->
    [
        ?encode_request_header_2(?FETCH_REQUEST, 12, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_compact_array(Topics, fun encode_fetch_topic_12/1),
        ?encode_compact_array(ForgottenTopicsData, fun encode_forgotten_topic_12/1),
        ?encode_compact_string(RackId),
        ?encode_tagged_fields(
            fun encode_fetch_request_12_tagged_field/2,
            Args
        )
    ];
encode_fetch_request_12(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_12},
        forgotten_topics_data => {array, forgotten_topic_12},
        rack_id => string
    }).

-spec encode_fetch_request_12_tagged_field(Key :: atom(), Value :: term()) -> iodata() | ignore.

encode_fetch_request_12_tagged_field(_Key = cluster_id, ClusterId) ->
    {0, ?encode_compact_nullable_string(ClusterId)};
encode_fetch_request_12_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_fetch_request_12(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_12(),
    Rest :: binary().

decode_fetch_request_12(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_int32(SessionId, Bin5, Bin6),
    ?_decode_int32(SessionEpoch, Bin6, Bin7),
    ?_decode_compact_array(Topics, Bin7, Bin8, ?_decode_element(decode_fetch_topic_12)),
    ?_decode_compact_array(ForgottenTopicsData, Bin8, Bin9, ?_decode_element(decode_forgotten_topic_12)),
    ?_decode_compact_string(RackId, Bin9, Bin10),
    ?decode_tagged_fields(
        fun decode_fetch_request_12_tagged_field/3,
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData,
            rack_id => RackId
        },
        Bin10
    ).

-spec decode_fetch_request_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

%% ClusterId
%% The clusterId if known. This is used to validate metadata fetches prior to broker registration.
decode_fetch_request_12_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_compact_nullable_string(ClusterId, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{cluster_id => ClusterId};
decode_fetch_request_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_partition_12(fetch_partition_12()) -> iodata().

encode_fetch_partition_12(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The current leader epoch of the partition.
        current_leader_epoch := CurrentLeaderEpoch,
        % The message offset.
        fetch_offset := FetchOffset,
        % The epoch of the last fetched record or -1 if there is none
        last_fetched_epoch := LastFetchedEpoch,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(FetchOffset),
    ?is_int32(LastFetchedEpoch),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(FetchOffset),
        ?encode_int32(LastFetchedEpoch),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_partition_12(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        fetch_offset => int64,
        last_fetched_epoch => int32,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_12(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_12(),
    Rest :: binary().

decode_fetch_partition_12(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(FetchOffset, Bin2, Bin3),
    ?_decode_int32(LastFetchedEpoch, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_int32(PartitionMaxBytes, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_fetch_partition_12_tagged_field/3,
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            fetch_offset => FetchOffset,
            last_fetched_epoch => LastFetchedEpoch,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin6
    ).

-spec decode_fetch_partition_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_partition_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_topic_12(fetch_topic_12()) -> iodata().

encode_fetch_topic_12(
    _Args = #{
        % The name of the topic to fetch.
        topic := Topic,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, fun encode_fetch_partition_12/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_topic_12(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, fetch_partition_12}
    }).

-spec decode_fetch_topic_12(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_12(),
    Rest :: binary().

decode_fetch_topic_12(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_12)),
    ?decode_tagged_fields(
        fun decode_fetch_topic_12_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetch_topic_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_topic_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_forgotten_topic_12(forgotten_topic_12()) -> iodata().

encode_forgotten_topic_12(
    _Args = #{
        % The topic name.
        topic := Topic,
        % The partitions indexes to forget.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_forgotten_topic_12(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_12(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_12(),
    Rest :: binary().

decode_forgotten_topic_12(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_forgotten_topic_12_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_forgotten_topic_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_forgotten_topic_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_request_13(fetch_request_13()) -> iodata().

encode_fetch_request_13(
    Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData,
        % Rack ID of the consumer making this request
        rack_id := RackId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData),
    ?is_string(RackId)
->
    [
        ?encode_request_header_2(?FETCH_REQUEST, 13, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_compact_array(Topics, fun encode_fetch_topic_13/1),
        ?encode_compact_array(ForgottenTopicsData, fun encode_forgotten_topic_13/1),
        ?encode_compact_string(RackId),
        ?encode_tagged_fields(
            fun encode_fetch_request_13_tagged_field/2,
            Args
        )
    ];
encode_fetch_request_13(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_13},
        forgotten_topics_data => {array, forgotten_topic_13},
        rack_id => string
    }).

-spec encode_fetch_request_13_tagged_field(Key :: atom(), Value :: term()) -> iodata() | ignore.

encode_fetch_request_13_tagged_field(_Key = cluster_id, ClusterId) ->
    {0, ?encode_compact_nullable_string(ClusterId)};
encode_fetch_request_13_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_fetch_request_13(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_13(),
    Rest :: binary().

decode_fetch_request_13(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_int32(SessionId, Bin5, Bin6),
    ?_decode_int32(SessionEpoch, Bin6, Bin7),
    ?_decode_compact_array(Topics, Bin7, Bin8, ?_decode_element(decode_fetch_topic_13)),
    ?_decode_compact_array(ForgottenTopicsData, Bin8, Bin9, ?_decode_element(decode_forgotten_topic_13)),
    ?_decode_compact_string(RackId, Bin9, Bin10),
    ?decode_tagged_fields(
        fun decode_fetch_request_13_tagged_field/3,
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData,
            rack_id => RackId
        },
        Bin10
    ).

-spec decode_fetch_request_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

%% ClusterId
%% The clusterId if known. This is used to validate metadata fetches prior to broker registration.
decode_fetch_request_13_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_compact_nullable_string(ClusterId, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{cluster_id => ClusterId};
decode_fetch_request_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_partition_13(fetch_partition_13()) -> iodata().

encode_fetch_partition_13(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The current leader epoch of the partition.
        current_leader_epoch := CurrentLeaderEpoch,
        % The message offset.
        fetch_offset := FetchOffset,
        % The epoch of the last fetched record or -1 if there is none
        last_fetched_epoch := LastFetchedEpoch,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(FetchOffset),
    ?is_int32(LastFetchedEpoch),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(FetchOffset),
        ?encode_int32(LastFetchedEpoch),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_partition_13(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        fetch_offset => int64,
        last_fetched_epoch => int32,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_13(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_13(),
    Rest :: binary().

decode_fetch_partition_13(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(FetchOffset, Bin2, Bin3),
    ?_decode_int32(LastFetchedEpoch, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_int32(PartitionMaxBytes, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_fetch_partition_13_tagged_field/3,
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            fetch_offset => FetchOffset,
            last_fetched_epoch => LastFetchedEpoch,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin6
    ).

-spec decode_fetch_partition_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_partition_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_topic_13(fetch_topic_13()) -> iodata().

encode_fetch_topic_13(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_fetch_partition_13/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_topic_13(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, fetch_partition_13}
    }).

-spec decode_fetch_topic_13(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_13(),
    Rest :: binary().

decode_fetch_topic_13(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_13)),
    ?decode_tagged_fields(
        fun decode_fetch_topic_13_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetch_topic_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_topic_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_forgotten_topic_13(forgotten_topic_13()) -> iodata().

encode_forgotten_topic_13(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The partitions indexes to forget.
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
encode_forgotten_topic_13(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_13(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_13(),
    Rest :: binary().

decode_forgotten_topic_13(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_forgotten_topic_13_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_forgotten_topic_13_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_forgotten_topic_13_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_request_14(fetch_request_14()) -> iodata().

encode_fetch_request_14(
    Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData,
        % Rack ID of the consumer making this request
        rack_id := RackId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData),
    ?is_string(RackId)
->
    [
        ?encode_request_header_2(?FETCH_REQUEST, 14, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_compact_array(Topics, fun encode_fetch_topic_14/1),
        ?encode_compact_array(ForgottenTopicsData, fun encode_forgotten_topic_14/1),
        ?encode_compact_string(RackId),
        ?encode_tagged_fields(
            fun encode_fetch_request_14_tagged_field/2,
            Args
        )
    ];
encode_fetch_request_14(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_14},
        forgotten_topics_data => {array, forgotten_topic_14},
        rack_id => string
    }).

-spec encode_fetch_request_14_tagged_field(Key :: atom(), Value :: term()) -> iodata() | ignore.

encode_fetch_request_14_tagged_field(_Key = cluster_id, ClusterId) ->
    {0, ?encode_compact_nullable_string(ClusterId)};
encode_fetch_request_14_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_fetch_request_14(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_14(),
    Rest :: binary().

decode_fetch_request_14(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxWaitMs, Bin1, Bin2),
    ?_decode_int32(MinBytes, Bin2, Bin3),
    ?_decode_int32(MaxBytes, Bin3, Bin4),
    ?_decode_int8(IsolationLevel, Bin4, Bin5),
    ?_decode_int32(SessionId, Bin5, Bin6),
    ?_decode_int32(SessionEpoch, Bin6, Bin7),
    ?_decode_compact_array(Topics, Bin7, Bin8, ?_decode_element(decode_fetch_topic_14)),
    ?_decode_compact_array(ForgottenTopicsData, Bin8, Bin9, ?_decode_element(decode_forgotten_topic_14)),
    ?_decode_compact_string(RackId, Bin9, Bin10),
    ?decode_tagged_fields(
        fun decode_fetch_request_14_tagged_field/3,
        Header#{
            replica_id => ReplicaId,
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData,
            rack_id => RackId
        },
        Bin10
    ).

-spec decode_fetch_request_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

%% ClusterId
%% The clusterId if known. This is used to validate metadata fetches prior to broker registration.
decode_fetch_request_14_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_compact_nullable_string(ClusterId, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{cluster_id => ClusterId};
decode_fetch_request_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_partition_14(fetch_partition_14()) -> iodata().

encode_fetch_partition_14(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The current leader epoch of the partition.
        current_leader_epoch := CurrentLeaderEpoch,
        % The message offset.
        fetch_offset := FetchOffset,
        % The epoch of the last fetched record or -1 if there is none
        last_fetched_epoch := LastFetchedEpoch,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(FetchOffset),
    ?is_int32(LastFetchedEpoch),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(FetchOffset),
        ?encode_int32(LastFetchedEpoch),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_partition_14(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        fetch_offset => int64,
        last_fetched_epoch => int32,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_14(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_14(),
    Rest :: binary().

decode_fetch_partition_14(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(FetchOffset, Bin2, Bin3),
    ?_decode_int32(LastFetchedEpoch, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_int32(PartitionMaxBytes, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_fetch_partition_14_tagged_field/3,
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            fetch_offset => FetchOffset,
            last_fetched_epoch => LastFetchedEpoch,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin6
    ).

-spec decode_fetch_partition_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_partition_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_topic_14(fetch_topic_14()) -> iodata().

encode_fetch_topic_14(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_fetch_partition_14/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_topic_14(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, fetch_partition_14}
    }).

-spec decode_fetch_topic_14(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_14(),
    Rest :: binary().

decode_fetch_topic_14(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_14)),
    ?decode_tagged_fields(
        fun decode_fetch_topic_14_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetch_topic_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_topic_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_forgotten_topic_14(forgotten_topic_14()) -> iodata().

encode_forgotten_topic_14(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The partitions indexes to forget.
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
encode_forgotten_topic_14(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_14(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_14(),
    Rest :: binary().

decode_forgotten_topic_14(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_forgotten_topic_14_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_forgotten_topic_14_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_forgotten_topic_14_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_request_15(fetch_request_15()) -> iodata().

encode_fetch_request_15(
    Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The maximum time in milliseconds to wait for the response.
        max_wait_ms := MaxWaitMs,
        % The minimum bytes to accumulate in the response.
        min_bytes := MinBytes,
        % The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.
        max_bytes := MaxBytes,
        % This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_level = 0) makes all records visible. With READ_COMMITTED (isolation_level = 1), non-transactional and COMMITTED transactional records are visible. To be more concrete, READ_COMMITTED returns all data from offsets smaller than the current LSO (last stable offset), and enables the inclusion of the list of aborted transactions in the result, which allows consumers to discard ABORTED transactional records
        isolation_level := IsolationLevel,
        % The fetch session ID.
        session_id := SessionId,
        % The fetch session epoch, which is used for ordering requests in a session.
        session_epoch := SessionEpoch,
        % The topics to fetch.
        topics := Topics,
        % In an incremental fetch request, the partitions to remove.
        forgotten_topics_data := ForgottenTopicsData,
        % Rack ID of the consumer making this request
        rack_id := RackId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(MaxWaitMs),
    ?is_int32(MinBytes),
    ?is_int32(MaxBytes),
    ?is_int8(IsolationLevel),
    ?is_int32(SessionId),
    ?is_int32(SessionEpoch),
    ?is_array(Topics),
    ?is_array(ForgottenTopicsData),
    ?is_string(RackId)
->
    [
        ?encode_request_header_2(?FETCH_REQUEST, 15, CorrelationId, ClientId),
        ?encode_int32(MaxWaitMs),
        ?encode_int32(MinBytes),
        ?encode_int32(MaxBytes),
        ?encode_int8(IsolationLevel),
        ?encode_int32(SessionId),
        ?encode_int32(SessionEpoch),
        ?encode_compact_array(Topics, fun encode_fetch_topic_15/1),
        ?encode_compact_array(ForgottenTopicsData, fun encode_forgotten_topic_15/1),
        ?encode_compact_string(RackId),
        ?encode_tagged_fields(
            fun encode_fetch_request_15_tagged_field/2,
            Args
        )
    ];
encode_fetch_request_15(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        max_wait_ms => int32,
        min_bytes => int32,
        max_bytes => int32,
        isolation_level => int8,
        session_id => int32,
        session_epoch => int32,
        topics => {array, fetch_topic_15},
        forgotten_topics_data => {array, forgotten_topic_15},
        rack_id => string
    }).

-spec encode_fetch_request_15_tagged_field(Key :: atom(), Value :: term()) -> iodata() | ignore.

encode_fetch_request_15_tagged_field(_Key = cluster_id, ClusterId) ->
    {0, ?encode_compact_nullable_string(ClusterId)};
encode_fetch_request_15_tagged_field(_Key = replica_state, ReplicaState) ->
    {1, encode_replica_state_15(ReplicaState)};
encode_fetch_request_15_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_fetch_request_15(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_request_15(),
    Rest :: binary().

decode_fetch_request_15(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(MaxWaitMs, Bin0, Bin1),
    ?_decode_int32(MinBytes, Bin1, Bin2),
    ?_decode_int32(MaxBytes, Bin2, Bin3),
    ?_decode_int8(IsolationLevel, Bin3, Bin4),
    ?_decode_int32(SessionId, Bin4, Bin5),
    ?_decode_int32(SessionEpoch, Bin5, Bin6),
    ?_decode_compact_array(Topics, Bin6, Bin7, ?_decode_element(decode_fetch_topic_15)),
    ?_decode_compact_array(ForgottenTopicsData, Bin7, Bin8, ?_decode_element(decode_forgotten_topic_15)),
    ?_decode_compact_string(RackId, Bin8, Bin9),
    ?decode_tagged_fields(
        fun decode_fetch_request_15_tagged_field/3,
        Header#{
            max_wait_ms => MaxWaitMs,
            min_bytes => MinBytes,
            max_bytes => MaxBytes,
            isolation_level => IsolationLevel,
            session_id => SessionId,
            session_epoch => SessionEpoch,
            topics => Topics,
            forgotten_topics_data => ForgottenTopicsData,
            rack_id => RackId
        },
        Bin9
    ).

-spec decode_fetch_request_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

%% ClusterId
%% The clusterId if known. This is used to validate metadata fetches prior to broker registration.
decode_fetch_request_15_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_compact_nullable_string(ClusterId, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{cluster_id => ClusterId};
%% ReplicaState
decode_fetch_request_15_tagged_field(_Tag = 1, Bin0, Acc) ->
    ?_decode_entity(ReplicaState, Bin0, Bin1, decode_replica_state_15),
    <<>> = Bin1,
    Acc#{replica_state => ReplicaState};
decode_fetch_request_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_replica_state_15(replica_state_15()) -> iodata().

encode_replica_state_15(
    _Args = #{
        % The replica ID of the follower, or -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % The epoch of this follower, or -1 if not available.
        replica_epoch := ReplicaEpoch
    }
) when
    ?is_int32(ReplicaId),
    ?is_int64(ReplicaEpoch)
->
    [
        ?encode_int32(ReplicaId),
        ?encode_int64(ReplicaEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_replica_state_15(Args) ->
    ?encoder_error(Args, #{
        replica_id => int32,
        replica_epoch => int64
    }).

-spec decode_replica_state_15(binary()) -> {Decoded, Rest} when
    Decoded :: replica_state_15(),
    Rest :: binary().

decode_replica_state_15(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int64(ReplicaEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_replica_state_15_tagged_field/3,
        #{
            replica_id => ReplicaId,
            replica_epoch => ReplicaEpoch
        },
        Bin2
    ).

-spec decode_replica_state_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_replica_state_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_partition_15(fetch_partition_15()) -> iodata().

encode_fetch_partition_15(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The current leader epoch of the partition.
        current_leader_epoch := CurrentLeaderEpoch,
        % The message offset.
        fetch_offset := FetchOffset,
        % The epoch of the last fetched record or -1 if there is none
        last_fetched_epoch := LastFetchedEpoch,
        % The earliest available offset of the follower replica.  The field is only used when the request is sent by the follower.
        log_start_offset := LogStartOffset,
        % The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be honored.
        partition_max_bytes := PartitionMaxBytes
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int64(FetchOffset),
    ?is_int32(LastFetchedEpoch),
    ?is_int64(LogStartOffset),
    ?is_int32(PartitionMaxBytes)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int64(FetchOffset),
        ?encode_int32(LastFetchedEpoch),
        ?encode_int64(LogStartOffset),
        ?encode_int32(PartitionMaxBytes),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_partition_15(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        fetch_offset => int64,
        last_fetched_epoch => int32,
        log_start_offset => int64,
        partition_max_bytes => int32
    }).

-spec decode_fetch_partition_15(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_partition_15(),
    Rest :: binary().

decode_fetch_partition_15(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int64(FetchOffset, Bin2, Bin3),
    ?_decode_int32(LastFetchedEpoch, Bin3, Bin4),
    ?_decode_int64(LogStartOffset, Bin4, Bin5),
    ?_decode_int32(PartitionMaxBytes, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_fetch_partition_15_tagged_field/3,
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            fetch_offset => FetchOffset,
            last_fetched_epoch => LastFetchedEpoch,
            log_start_offset => LogStartOffset,
            partition_max_bytes => PartitionMaxBytes
        },
        Bin6
    ).

-spec decode_fetch_partition_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_partition_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_fetch_topic_15(fetch_topic_15()) -> iodata().

encode_fetch_topic_15(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The partitions to fetch.
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_fetch_partition_15/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_topic_15(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, fetch_partition_15}
    }).

-spec decode_fetch_topic_15(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_topic_15(),
    Rest :: binary().

decode_fetch_topic_15(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_fetch_partition_15)),
    ?decode_tagged_fields(
        fun decode_fetch_topic_15_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_fetch_topic_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_topic_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_forgotten_topic_15(forgotten_topic_15()) -> iodata().

encode_forgotten_topic_15(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % The partitions indexes to forget.
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
encode_forgotten_topic_15(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, int32}
    }).

-spec decode_forgotten_topic_15(binary()) -> {Decoded, Rest} when
    Decoded :: forgotten_topic_15(),
    Rest :: binary().

decode_forgotten_topic_15(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_forgotten_topic_15_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_forgotten_topic_15_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_forgotten_topic_15_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type fetch_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    topics := list(fetch_topic_0())
}.
-type fetch_partition_0() :: #{
    partition := integer(),
    fetch_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_0() :: #{
    topic := binary(),
    partitions := list(fetch_partition_0())
}.
-type fetch_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    topics := list(fetch_topic_1())
}.
-type fetch_partition_1() :: #{
    partition := integer(),
    fetch_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_1() :: #{
    topic := binary(),
    partitions := list(fetch_partition_1())
}.
-type fetch_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    topics := list(fetch_topic_2())
}.
-type fetch_partition_2() :: #{
    partition := integer(),
    fetch_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_2() :: #{
    topic := binary(),
    partitions := list(fetch_partition_2())
}.
-type fetch_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    topics := list(fetch_topic_3())
}.
-type fetch_partition_3() :: #{
    partition := integer(),
    fetch_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_3() :: #{
    topic := binary(),
    partitions := list(fetch_partition_3())
}.
-type fetch_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    topics := list(fetch_topic_4())
}.
-type fetch_partition_4() :: #{
    partition := integer(),
    fetch_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_4() :: #{
    topic := binary(),
    partitions := list(fetch_partition_4())
}.
-type fetch_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    topics := list(fetch_topic_5())
}.
-type fetch_partition_5() :: #{
    partition := integer(),
    fetch_offset := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_5() :: #{
    topic := binary(),
    partitions := list(fetch_partition_5())
}.
-type fetch_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    topics := list(fetch_topic_6())
}.
-type fetch_partition_6() :: #{
    partition := integer(),
    fetch_offset := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_6() :: #{
    topic := binary(),
    partitions := list(fetch_partition_6())
}.
-type fetch_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_7()),
    forgotten_topics_data := list(forgotten_topic_7())
}.
-type fetch_partition_7() :: #{
    partition := integer(),
    fetch_offset := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_7() :: #{
    topic := binary(),
    partitions := list(fetch_partition_7())
}.
-type forgotten_topic_7() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type fetch_request_8() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_8()),
    forgotten_topics_data := list(forgotten_topic_8())
}.
-type fetch_partition_8() :: #{
    partition := integer(),
    fetch_offset := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_8() :: #{
    topic := binary(),
    partitions := list(fetch_partition_8())
}.
-type forgotten_topic_8() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type fetch_request_9() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_9()),
    forgotten_topics_data := list(forgotten_topic_9())
}.
-type fetch_partition_9() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    fetch_offset := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_9() :: #{
    topic := binary(),
    partitions := list(fetch_partition_9())
}.
-type forgotten_topic_9() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type fetch_request_10() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_10()),
    forgotten_topics_data := list(forgotten_topic_10())
}.
-type fetch_partition_10() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    fetch_offset := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_10() :: #{
    topic := binary(),
    partitions := list(fetch_partition_10())
}.
-type forgotten_topic_10() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type fetch_request_11() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_11()),
    forgotten_topics_data := list(forgotten_topic_11()),
    rack_id := binary()
}.
-type fetch_partition_11() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    fetch_offset := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_11() :: #{
    topic := binary(),
    partitions := list(fetch_partition_11())
}.
-type forgotten_topic_11() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type fetch_request_12() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    cluster_id := binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_12()),
    forgotten_topics_data := list(forgotten_topic_12()),
    rack_id := binary()
}.
-type fetch_partition_12() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    fetch_offset := integer(),
    last_fetched_epoch := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_12() :: #{
    topic := binary(),
    partitions := list(fetch_partition_12())
}.
-type forgotten_topic_12() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type fetch_request_13() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    cluster_id := binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_13()),
    forgotten_topics_data := list(forgotten_topic_13()),
    rack_id := binary()
}.
-type fetch_partition_13() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    fetch_offset := integer(),
    last_fetched_epoch := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_13() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(fetch_partition_13())
}.
-type forgotten_topic_13() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(integer())
}.
-type fetch_request_14() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    cluster_id := binary() | null,
    replica_id := integer(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_14()),
    forgotten_topics_data := list(forgotten_topic_14()),
    rack_id := binary()
}.
-type fetch_partition_14() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    fetch_offset := integer(),
    last_fetched_epoch := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_14() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(fetch_partition_14())
}.
-type forgotten_topic_14() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(integer())
}.
-type fetch_request_15() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    cluster_id := binary() | null,
    replica_state := replica_state_15(),
    max_wait_ms := integer(),
    min_bytes := integer(),
    max_bytes := integer(),
    isolation_level := integer(),
    session_id := integer(),
    session_epoch := integer(),
    topics := list(fetch_topic_15()),
    forgotten_topics_data := list(forgotten_topic_15()),
    rack_id := binary()
}.
-type replica_state_15() :: #{
    replica_id := integer(),
    replica_epoch := integer()
}.
-type fetch_partition_15() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    fetch_offset := integer(),
    last_fetched_epoch := integer(),
    log_start_offset := integer(),
    partition_max_bytes := integer()
}.
-type fetch_topic_15() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(fetch_partition_15())
}.
-type forgotten_topic_15() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(integer())
}.
