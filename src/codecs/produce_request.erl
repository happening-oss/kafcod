-module(produce_request).
-export([
    encode_produce_request_0/1,
    decode_produce_request_0/1,
    encode_produce_request_1/1,
    decode_produce_request_1/1,
    encode_produce_request_2/1,
    decode_produce_request_2/1,
    encode_produce_request_3/1,
    decode_produce_request_3/1,
    encode_produce_request_4/1,
    decode_produce_request_4/1,
    encode_produce_request_5/1,
    decode_produce_request_5/1,
    encode_produce_request_6/1,
    decode_produce_request_6/1,
    encode_produce_request_7/1,
    decode_produce_request_7/1,
    encode_produce_request_8/1,
    decode_produce_request_8/1,
    encode_produce_request_9/1,
    decode_produce_request_9/1,
    encode_produce_request_10/1,
    decode_produce_request_10/1,
    encode_produce_request_11/1,
    decode_produce_request_11/1
]).
-export_type([
    produce_request_0/0,
    partition_produce_data_0/0,
    topic_produce_data_0/0,
    produce_request_1/0,
    partition_produce_data_1/0,
    topic_produce_data_1/0,
    produce_request_2/0,
    partition_produce_data_2/0,
    topic_produce_data_2/0,
    produce_request_3/0,
    partition_produce_data_3/0,
    topic_produce_data_3/0,
    produce_request_4/0,
    partition_produce_data_4/0,
    topic_produce_data_4/0,
    produce_request_5/0,
    partition_produce_data_5/0,
    topic_produce_data_5/0,
    produce_request_6/0,
    partition_produce_data_6/0,
    topic_produce_data_6/0,
    produce_request_7/0,
    partition_produce_data_7/0,
    topic_produce_data_7/0,
    produce_request_8/0,
    partition_produce_data_8/0,
    topic_produce_data_8/0,
    produce_request_9/0,
    partition_produce_data_9/0,
    topic_produce_data_9/0,
    produce_request_10/0,
    partition_produce_data_10/0,
    topic_produce_data_10/0,
    produce_request_11/0,
    partition_produce_data_11/0,
    topic_produce_data_11/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(PRODUCE_REQUEST, 0).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_produce_request_0(produce_request_0()) -> iodata().

encode_produce_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_0/1)
    ];
encode_produce_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_0}
    }).

-spec decode_produce_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_0(),
    Rest :: binary().

decode_produce_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int16(Acks, Bin0, Bin1),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_array(TopicData, Bin2, Bin3, ?_decode_element(decode_topic_produce_data_0)),
    {
        Header#{
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin3
    }.

-spec encode_partition_produce_data_0(partition_produce_data_0()) -> iodata().

encode_partition_produce_data_0(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_0(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_0(),
    Rest :: binary().

decode_partition_produce_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_0(topic_produce_data_0()) -> iodata().

encode_topic_produce_data_0(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_0/1)
    ];
encode_topic_produce_data_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_0}
    }).

-spec decode_topic_produce_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_0(),
    Rest :: binary().

decode_topic_produce_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_0)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_1(produce_request_1()) -> iodata().

encode_produce_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_1/1)
    ];
encode_produce_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_1}
    }).

-spec decode_produce_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_1(),
    Rest :: binary().

decode_produce_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int16(Acks, Bin0, Bin1),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_array(TopicData, Bin2, Bin3, ?_decode_element(decode_topic_produce_data_1)),
    {
        Header#{
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin3
    }.

-spec encode_partition_produce_data_1(partition_produce_data_1()) -> iodata().

encode_partition_produce_data_1(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_1(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_1(),
    Rest :: binary().

decode_partition_produce_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_1(topic_produce_data_1()) -> iodata().

encode_topic_produce_data_1(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_1/1)
    ];
encode_topic_produce_data_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_1}
    }).

-spec decode_topic_produce_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_1(),
    Rest :: binary().

decode_topic_produce_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_1)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_2(produce_request_2()) -> iodata().

encode_produce_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_2/1)
    ];
encode_produce_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_2}
    }).

-spec decode_produce_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_2(),
    Rest :: binary().

decode_produce_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int16(Acks, Bin0, Bin1),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_array(TopicData, Bin2, Bin3, ?_decode_element(decode_topic_produce_data_2)),
    {
        Header#{
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin3
    }.

-spec encode_partition_produce_data_2(partition_produce_data_2()) -> iodata().

encode_partition_produce_data_2(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_2(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_2(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_2(),
    Rest :: binary().

decode_partition_produce_data_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_2(topic_produce_data_2()) -> iodata().

encode_topic_produce_data_2(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_2/1)
    ];
encode_topic_produce_data_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_2}
    }).

-spec decode_topic_produce_data_2(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_2(),
    Rest :: binary().

decode_topic_produce_data_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_2)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_3(produce_request_3()) -> iodata().

encode_produce_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 3, CorrelationId, ClientId),
        ?encode_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_3/1)
    ];
encode_produce_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_3}
    }).

-spec decode_produce_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_3(),
    Rest :: binary().

decode_produce_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_3)),
    {
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    }.

-spec encode_partition_produce_data_3(partition_produce_data_3()) -> iodata().

encode_partition_produce_data_3(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_3(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_3(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_3(),
    Rest :: binary().

decode_partition_produce_data_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_3(topic_produce_data_3()) -> iodata().

encode_topic_produce_data_3(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_3/1)
    ];
encode_topic_produce_data_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_3}
    }).

-spec decode_topic_produce_data_3(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_3(),
    Rest :: binary().

decode_topic_produce_data_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_3)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_4(produce_request_4()) -> iodata().

encode_produce_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 4, CorrelationId, ClientId),
        ?encode_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_4/1)
    ];
encode_produce_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_4}
    }).

-spec decode_produce_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_4(),
    Rest :: binary().

decode_produce_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_4)),
    {
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    }.

-spec encode_partition_produce_data_4(partition_produce_data_4()) -> iodata().

encode_partition_produce_data_4(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_4(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_4(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_4(),
    Rest :: binary().

decode_partition_produce_data_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_4(topic_produce_data_4()) -> iodata().

encode_topic_produce_data_4(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_4/1)
    ];
encode_topic_produce_data_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_4}
    }).

-spec decode_topic_produce_data_4(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_4(),
    Rest :: binary().

decode_topic_produce_data_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_4)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_5(produce_request_5()) -> iodata().

encode_produce_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 5, CorrelationId, ClientId),
        ?encode_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_5/1)
    ];
encode_produce_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_5}
    }).

-spec decode_produce_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_5(),
    Rest :: binary().

decode_produce_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_5)),
    {
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    }.

-spec encode_partition_produce_data_5(partition_produce_data_5()) -> iodata().

encode_partition_produce_data_5(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_5(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_5(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_5(),
    Rest :: binary().

decode_partition_produce_data_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_5(topic_produce_data_5()) -> iodata().

encode_topic_produce_data_5(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_5/1)
    ];
encode_topic_produce_data_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_5}
    }).

-spec decode_topic_produce_data_5(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_5(),
    Rest :: binary().

decode_topic_produce_data_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_5)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_6(produce_request_6()) -> iodata().

encode_produce_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 6, CorrelationId, ClientId),
        ?encode_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_6/1)
    ];
encode_produce_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_6}
    }).

-spec decode_produce_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_6(),
    Rest :: binary().

decode_produce_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_6)),
    {
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    }.

-spec encode_partition_produce_data_6(partition_produce_data_6()) -> iodata().

encode_partition_produce_data_6(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_6(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_6(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_6(),
    Rest :: binary().

decode_partition_produce_data_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_6(topic_produce_data_6()) -> iodata().

encode_topic_produce_data_6(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_6/1)
    ];
encode_topic_produce_data_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_6}
    }).

-spec decode_topic_produce_data_6(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_6(),
    Rest :: binary().

decode_topic_produce_data_6(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_6)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_7(produce_request_7()) -> iodata().

encode_produce_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 7, CorrelationId, ClientId),
        ?encode_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_7/1)
    ];
encode_produce_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_7}
    }).

-spec decode_produce_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_7(),
    Rest :: binary().

decode_produce_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_7)),
    {
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    }.

-spec encode_partition_produce_data_7(partition_produce_data_7()) -> iodata().

encode_partition_produce_data_7(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_7(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_7(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_7(),
    Rest :: binary().

decode_partition_produce_data_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_7(topic_produce_data_7()) -> iodata().

encode_topic_produce_data_7(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_7/1)
    ];
encode_topic_produce_data_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_7}
    }).

-spec decode_topic_produce_data_7(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_7(),
    Rest :: binary().

decode_topic_produce_data_7(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_7)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_8(produce_request_8()) -> iodata().

encode_produce_request_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_1(?PRODUCE_REQUEST, 8, CorrelationId, ClientId),
        ?encode_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_array(TopicData, fun encode_topic_produce_data_8/1)
    ];
encode_produce_request_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_8}
    }).

-spec decode_produce_request_8(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_8(),
    Rest :: binary().

decode_produce_request_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_8)),
    {
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    }.

-spec encode_partition_produce_data_8(partition_produce_data_8()) -> iodata().

encode_partition_produce_data_8(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_nullable_records(Records)
    ];
encode_partition_produce_data_8(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_8(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_8(),
    Rest :: binary().

decode_partition_produce_data_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_nullable_records(Records, Bin1, Bin2),
    {
        #{
            index => Index,
            records => Records
        },
        Bin2
    }.

-spec encode_topic_produce_data_8(topic_produce_data_8()) -> iodata().

encode_topic_produce_data_8(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionData, fun encode_partition_produce_data_8/1)
    ];
encode_topic_produce_data_8(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_8}
    }).

-spec decode_topic_produce_data_8(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_8(),
    Rest :: binary().

decode_topic_produce_data_8(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_8)),
    {
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    }.

-spec encode_produce_request_9(produce_request_9()) -> iodata().

encode_produce_request_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_2(?PRODUCE_REQUEST, 9, CorrelationId, ClientId),
        ?encode_compact_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_compact_array(TopicData, fun encode_topic_produce_data_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_produce_request_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_9}
    }).

-spec decode_produce_request_9(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_9(),
    Rest :: binary().

decode_produce_request_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_compact_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_9)),
    ?decode_tagged_fields(
        fun decode_produce_request_9_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    ).

-spec decode_produce_request_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: produce_request_9().

decode_produce_request_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_produce_data_9(partition_produce_data_9()) -> iodata().

encode_partition_produce_data_9(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_compact_nullable_records(Records),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_produce_data_9(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_9(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_9(),
    Rest :: binary().

decode_partition_produce_data_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_compact_nullable_records(Records, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_partition_produce_data_9_tagged_field/3,
        #{
            index => Index,
            records => Records
        },
        Bin2
    ).

-spec decode_partition_produce_data_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_produce_data_9().

decode_partition_produce_data_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_produce_data_9(topic_produce_data_9()) -> iodata().

encode_topic_produce_data_9(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionData, fun encode_partition_produce_data_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_produce_data_9(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_9}
    }).

-spec decode_topic_produce_data_9(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_9(),
    Rest :: binary().

decode_topic_produce_data_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_9)),
    ?decode_tagged_fields(
        fun decode_topic_produce_data_9_tagged_field/3,
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    ).

-spec decode_topic_produce_data_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_produce_data_9().

decode_topic_produce_data_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_produce_request_10(produce_request_10()) -> iodata().

encode_produce_request_10(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_2(?PRODUCE_REQUEST, 10, CorrelationId, ClientId),
        ?encode_compact_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_compact_array(TopicData, fun encode_topic_produce_data_10/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_produce_request_10(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_10}
    }).

-spec decode_produce_request_10(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_10(),
    Rest :: binary().

decode_produce_request_10(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_compact_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_10)),
    ?decode_tagged_fields(
        fun decode_produce_request_10_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    ).

-spec decode_produce_request_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: produce_request_10().

decode_produce_request_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_produce_data_10(partition_produce_data_10()) -> iodata().

encode_partition_produce_data_10(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_compact_nullable_records(Records),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_produce_data_10(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_10(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_10(),
    Rest :: binary().

decode_partition_produce_data_10(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_compact_nullable_records(Records, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_partition_produce_data_10_tagged_field/3,
        #{
            index => Index,
            records => Records
        },
        Bin2
    ).

-spec decode_partition_produce_data_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_produce_data_10().

decode_partition_produce_data_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_produce_data_10(topic_produce_data_10()) -> iodata().

encode_topic_produce_data_10(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionData, fun encode_partition_produce_data_10/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_produce_data_10(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_10}
    }).

-spec decode_topic_produce_data_10(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_10(),
    Rest :: binary().

decode_topic_produce_data_10(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_10)),
    ?decode_tagged_fields(
        fun decode_topic_produce_data_10_tagged_field/3,
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    ).

-spec decode_topic_produce_data_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_produce_data_10().

decode_topic_produce_data_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_produce_request_11(produce_request_11()) -> iodata().

encode_produce_request_11(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transactional ID, or null if the producer is not transactional.
        transactional_id := TransactionalId,
        % The number of acknowledgments the producer requires the leader to have received before considering a request complete. Allowed values: 0 for no acknowledgments, 1 for only the leader and -1 for the full ISR.
        acks := Acks,
        % The timeout to await a response in milliseconds.
        timeout_ms := TimeoutMs,
        % Each topic to produce to.
        topic_data := TopicData
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(TransactionalId),
    ?is_int16(Acks),
    ?is_int32(TimeoutMs),
    ?is_array(TopicData)
->
    [
        ?encode_request_header_2(?PRODUCE_REQUEST, 11, CorrelationId, ClientId),
        ?encode_compact_nullable_string(TransactionalId),
        ?encode_int16(Acks),
        ?encode_int32(TimeoutMs),
        ?encode_compact_array(TopicData, fun encode_topic_produce_data_11/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_produce_request_11(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => nullable_string,
        acks => int16,
        timeout_ms => int32,
        topic_data => {array, topic_produce_data_11}
    }).

-spec decode_produce_request_11(binary()) -> {Decoded, Rest} when
    Decoded :: produce_request_11(),
    Rest :: binary().

decode_produce_request_11(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(TransactionalId, Bin0, Bin1),
    ?_decode_int16(Acks, Bin1, Bin2),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?_decode_compact_array(TopicData, Bin3, Bin4, ?_decode_element(decode_topic_produce_data_11)),
    ?decode_tagged_fields(
        fun decode_produce_request_11_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            acks => Acks,
            timeout_ms => TimeoutMs,
            topic_data => TopicData
        },
        Bin4
    ).

-spec decode_produce_request_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: produce_request_11().

decode_produce_request_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_produce_data_11(partition_produce_data_11()) -> iodata().

encode_partition_produce_data_11(
    _Args = #{
        % The partition index.
        index := Index,
        % The record data to be produced.
        records := Records
    }
) when
    ?is_int32(Index),
    ?is_nullable_records(Records)
->
    [
        ?encode_int32(Index),
        ?encode_compact_nullable_records(Records),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_produce_data_11(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        records => nullable_records
    }).

-spec decode_partition_produce_data_11(binary()) -> {Decoded, Rest} when
    Decoded :: partition_produce_data_11(),
    Rest :: binary().

decode_partition_produce_data_11(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_compact_nullable_records(Records, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_partition_produce_data_11_tagged_field/3,
        #{
            index => Index,
            records => Records
        },
        Bin2
    ).

-spec decode_partition_produce_data_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_produce_data_11().

decode_partition_produce_data_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_produce_data_11(topic_produce_data_11()) -> iodata().

encode_topic_produce_data_11(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to produce to.
        partition_data := PartitionData
    }
) when
    ?is_string(Name),
    ?is_array(PartitionData)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionData, fun encode_partition_produce_data_11/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_produce_data_11(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_data => {array, partition_produce_data_11}
    }).

-spec decode_topic_produce_data_11(binary()) -> {Decoded, Rest} when
    Decoded :: topic_produce_data_11(),
    Rest :: binary().

decode_topic_produce_data_11(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionData, Bin1, Bin2, ?_decode_element(decode_partition_produce_data_11)),
    ?decode_tagged_fields(
        fun decode_topic_produce_data_11_tagged_field/3,
        #{
            name => Name,
            partition_data => PartitionData
        },
        Bin2
    ).

-spec decode_topic_produce_data_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_produce_data_11().

decode_topic_produce_data_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type produce_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_0())
}.
-type partition_produce_data_0() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_0() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_0())
}.
-type produce_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_1())
}.
-type partition_produce_data_1() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_1() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_1())
}.
-type produce_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_2())
}.
-type partition_produce_data_2() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_2() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_2())
}.
-type produce_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_3())
}.
-type partition_produce_data_3() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_3() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_3())
}.
-type produce_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_4())
}.
-type partition_produce_data_4() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_4() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_4())
}.
-type produce_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_5())
}.
-type partition_produce_data_5() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_5() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_5())
}.
-type produce_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_6())
}.
-type partition_produce_data_6() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_6() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_6())
}.
-type produce_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_7())
}.
-type partition_produce_data_7() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_7() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_7())
}.
-type produce_request_8() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_8())
}.
-type partition_produce_data_8() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_8() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_8())
}.
-type produce_request_9() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_9())
}.
-type partition_produce_data_9() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_9() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_9())
}.
-type produce_request_10() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_10())
}.
-type partition_produce_data_10() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_10() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_10())
}.
-type produce_request_11() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary() | null,
    acks := integer(),
    timeout_ms := integer(),
    topic_data := list(topic_produce_data_11())
}.
-type partition_produce_data_11() :: #{
    index := integer(),
    records := kafcod_records:records()
}.
-type topic_produce_data_11() :: #{
    name := binary(),
    partition_data := list(partition_produce_data_11())
}.
