-module(alter_replica_log_dirs_response).
-export([
    encode_alter_replica_log_dirs_response_0/1,
    decode_alter_replica_log_dirs_response_0/1,
    encode_alter_replica_log_dirs_response_1/1,
    decode_alter_replica_log_dirs_response_1/1,
    encode_alter_replica_log_dirs_response_2/1,
    decode_alter_replica_log_dirs_response_2/1
]).
-export_type([
    alter_replica_log_dirs_response_0/0,
    alter_replica_log_dir_partition_result_0/0,
    alter_replica_log_dir_topic_result_0/0,
    alter_replica_log_dirs_response_1/0,
    alter_replica_log_dir_partition_result_1/0,
    alter_replica_log_dir_topic_result_1/0,
    alter_replica_log_dirs_response_2/0,
    alter_replica_log_dir_partition_result_2/0,
    alter_replica_log_dir_topic_result_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_alter_replica_log_dirs_response_0(alter_replica_log_dirs_response_0()) -> iodata().

encode_alter_replica_log_dirs_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Results, fun encode_alter_replica_log_dir_topic_result_0/1)
    ];
encode_alter_replica_log_dirs_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, alter_replica_log_dir_topic_result_0}
    }).

-spec decode_alter_replica_log_dirs_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dirs_response_0(),
    Rest :: binary().

decode_alter_replica_log_dirs_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_topic_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_alter_replica_log_dir_partition_result_0(alter_replica_log_dir_partition_result_0()) -> iodata().

encode_alter_replica_log_dir_partition_result_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_alter_replica_log_dir_partition_result_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_alter_replica_log_dir_partition_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_partition_result_0(),
    Rest :: binary().

decode_alter_replica_log_dir_partition_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_alter_replica_log_dir_topic_result_0(alter_replica_log_dir_topic_result_0()) -> iodata().

encode_alter_replica_log_dir_topic_result_0(
    _Args = #{
        % The name of the topic.
        topic_name := TopicName,
        % The results for each partition.
        partitions := Partitions
    }
) when
    ?is_string(TopicName),
    ?is_array(Partitions)
->
    [
        ?encode_string(TopicName),
        ?encode_array(Partitions, fun encode_alter_replica_log_dir_partition_result_0/1)
    ];
encode_alter_replica_log_dir_topic_result_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partitions => {array, alter_replica_log_dir_partition_result_0}
    }).

-spec decode_alter_replica_log_dir_topic_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_topic_result_0(),
    Rest :: binary().

decode_alter_replica_log_dir_topic_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_partition_result_0)),
    {
        #{
            topic_name => TopicName,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_alter_replica_log_dirs_response_1(alter_replica_log_dirs_response_1()) -> iodata().

encode_alter_replica_log_dirs_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Results, fun encode_alter_replica_log_dir_topic_result_1/1)
    ];
encode_alter_replica_log_dirs_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, alter_replica_log_dir_topic_result_1}
    }).

-spec decode_alter_replica_log_dirs_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dirs_response_1(),
    Rest :: binary().

decode_alter_replica_log_dirs_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_topic_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_alter_replica_log_dir_partition_result_1(alter_replica_log_dir_partition_result_1()) -> iodata().

encode_alter_replica_log_dir_partition_result_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_alter_replica_log_dir_partition_result_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_alter_replica_log_dir_partition_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_partition_result_1(),
    Rest :: binary().

decode_alter_replica_log_dir_partition_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_alter_replica_log_dir_topic_result_1(alter_replica_log_dir_topic_result_1()) -> iodata().

encode_alter_replica_log_dir_topic_result_1(
    _Args = #{
        % The name of the topic.
        topic_name := TopicName,
        % The results for each partition.
        partitions := Partitions
    }
) when
    ?is_string(TopicName),
    ?is_array(Partitions)
->
    [
        ?encode_string(TopicName),
        ?encode_array(Partitions, fun encode_alter_replica_log_dir_partition_result_1/1)
    ];
encode_alter_replica_log_dir_topic_result_1(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partitions => {array, alter_replica_log_dir_partition_result_1}
    }).

-spec decode_alter_replica_log_dir_topic_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_topic_result_1(),
    Rest :: binary().

decode_alter_replica_log_dir_topic_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_partition_result_1)),
    {
        #{
            topic_name => TopicName,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_alter_replica_log_dirs_response_2(alter_replica_log_dirs_response_2()) -> iodata().

encode_alter_replica_log_dirs_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Results, fun encode_alter_replica_log_dir_topic_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_replica_log_dirs_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, alter_replica_log_dir_topic_result_2}
    }).

-spec decode_alter_replica_log_dirs_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dirs_response_2(),
    Rest :: binary().

decode_alter_replica_log_dirs_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Results, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_topic_result_2)),
    ?decode_tagged_fields(
        fun decode_alter_replica_log_dirs_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    ).

-spec decode_alter_replica_log_dirs_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_replica_log_dirs_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_replica_log_dir_partition_result_2(alter_replica_log_dir_partition_result_2()) -> iodata().

encode_alter_replica_log_dir_partition_result_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_replica_log_dir_partition_result_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_alter_replica_log_dir_partition_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_partition_result_2(),
    Rest :: binary().

decode_alter_replica_log_dir_partition_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_alter_replica_log_dir_partition_result_2_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_alter_replica_log_dir_partition_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_replica_log_dir_partition_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_replica_log_dir_topic_result_2(alter_replica_log_dir_topic_result_2()) -> iodata().

encode_alter_replica_log_dir_topic_result_2(
    _Args = #{
        % The name of the topic.
        topic_name := TopicName,
        % The results for each partition.
        partitions := Partitions
    }
) when
    ?is_string(TopicName),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_compact_array(Partitions, fun encode_alter_replica_log_dir_partition_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_replica_log_dir_topic_result_2(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partitions => {array, alter_replica_log_dir_partition_result_2}
    }).

-spec decode_alter_replica_log_dir_topic_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_topic_result_2(),
    Rest :: binary().

decode_alter_replica_log_dir_topic_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_partition_result_2)),
    ?decode_tagged_fields(
        fun decode_alter_replica_log_dir_topic_result_2_tagged_field/3,
        #{
            topic_name => TopicName,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_alter_replica_log_dir_topic_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_replica_log_dir_topic_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type alter_replica_log_dirs_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(alter_replica_log_dir_topic_result_0())
}.
-type alter_replica_log_dir_partition_result_0() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type alter_replica_log_dir_topic_result_0() :: #{
    topic_name := binary(),
    partitions := list(alter_replica_log_dir_partition_result_0())
}.
-type alter_replica_log_dirs_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(alter_replica_log_dir_topic_result_1())
}.
-type alter_replica_log_dir_partition_result_1() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type alter_replica_log_dir_topic_result_1() :: #{
    topic_name := binary(),
    partitions := list(alter_replica_log_dir_partition_result_1())
}.
-type alter_replica_log_dirs_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(alter_replica_log_dir_topic_result_2())
}.
-type alter_replica_log_dir_partition_result_2() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type alter_replica_log_dir_topic_result_2() :: #{
    topic_name := binary(),
    partitions := list(alter_replica_log_dir_partition_result_2())
}.
