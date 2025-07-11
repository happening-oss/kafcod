-module(add_partitions_to_txn_response).
-export([
    encode_add_partitions_to_txn_response_0/1,
    decode_add_partitions_to_txn_response_0/1,
    encode_add_partitions_to_txn_response_1/1,
    decode_add_partitions_to_txn_response_1/1,
    encode_add_partitions_to_txn_response_2/1,
    decode_add_partitions_to_txn_response_2/1,
    encode_add_partitions_to_txn_response_3/1,
    decode_add_partitions_to_txn_response_3/1,
    encode_add_partitions_to_txn_response_4/1,
    decode_add_partitions_to_txn_response_4/1,
    encode_add_partitions_to_txn_response_5/1,
    decode_add_partitions_to_txn_response_5/1
]).
-export_type([
    add_partitions_to_txn_response_0/0,
    add_partitions_to_txn_partition_result_0/0,
    add_partitions_to_txn_topic_result_0/0,
    add_partitions_to_txn_response_1/0,
    add_partitions_to_txn_partition_result_1/0,
    add_partitions_to_txn_topic_result_1/0,
    add_partitions_to_txn_response_2/0,
    add_partitions_to_txn_partition_result_2/0,
    add_partitions_to_txn_topic_result_2/0,
    add_partitions_to_txn_response_3/0,
    add_partitions_to_txn_partition_result_3/0,
    add_partitions_to_txn_topic_result_3/0,
    add_partitions_to_txn_response_4/0,
    add_partitions_to_txn_result_4/0,
    add_partitions_to_txn_partition_result_4/0,
    add_partitions_to_txn_topic_result_4/0,
    add_partitions_to_txn_response_5/0,
    add_partitions_to_txn_result_5/0,
    add_partitions_to_txn_partition_result_5/0,
    add_partitions_to_txn_topic_result_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_add_partitions_to_txn_response_0(add_partitions_to_txn_response_0()) -> iodata().

encode_add_partitions_to_txn_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic.
        results_by_topic_v3_and_below := ResultsByTopicV3AndBelow
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(ResultsByTopicV3AndBelow)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(ResultsByTopicV3AndBelow, fun encode_add_partitions_to_txn_topic_result_0/1)
    ];
encode_add_partitions_to_txn_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results_by_topic_v3_and_below => {array, add_partitions_to_txn_topic_result_0}
    }).

-spec decode_add_partitions_to_txn_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_response_0(),
    Rest :: binary().

decode_add_partitions_to_txn_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(ResultsByTopicV3AndBelow, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_topic_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results_by_topic_v3_and_below => ResultsByTopicV3AndBelow
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_partition_result_0(add_partitions_to_txn_partition_result_0()) -> iodata().

encode_add_partitions_to_txn_partition_result_0(
    _Args = #{
        % The partition indexes.
        partition_index := PartitionIndex,
        % The response error code.
        partition_error_code := PartitionErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(PartitionErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(PartitionErrorCode)
    ];
encode_add_partitions_to_txn_partition_result_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_error_code => int16
    }).

-spec decode_add_partitions_to_txn_partition_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_partition_result_0(),
    Rest :: binary().

decode_add_partitions_to_txn_partition_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(PartitionErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            partition_error_code => PartitionErrorCode
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_topic_result_0(add_partitions_to_txn_topic_result_0()) -> iodata().

encode_add_partitions_to_txn_topic_result_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The results for each partition
        results_by_partition := ResultsByPartition
    }
) when
    ?is_string(Name),
    ?is_array(ResultsByPartition)
->
    [
        ?encode_string(Name),
        ?encode_array(ResultsByPartition, fun encode_add_partitions_to_txn_partition_result_0/1)
    ];
encode_add_partitions_to_txn_topic_result_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        results_by_partition => {array, add_partitions_to_txn_partition_result_0}
    }).

-spec decode_add_partitions_to_txn_topic_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_result_0(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(ResultsByPartition, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_partition_result_0)),
    {
        #{
            name => Name,
            results_by_partition => ResultsByPartition
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_response_1(add_partitions_to_txn_response_1()) -> iodata().

encode_add_partitions_to_txn_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic.
        results_by_topic_v3_and_below := ResultsByTopicV3AndBelow
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(ResultsByTopicV3AndBelow)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(ResultsByTopicV3AndBelow, fun encode_add_partitions_to_txn_topic_result_1/1)
    ];
encode_add_partitions_to_txn_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results_by_topic_v3_and_below => {array, add_partitions_to_txn_topic_result_1}
    }).

-spec decode_add_partitions_to_txn_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_response_1(),
    Rest :: binary().

decode_add_partitions_to_txn_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(ResultsByTopicV3AndBelow, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_topic_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results_by_topic_v3_and_below => ResultsByTopicV3AndBelow
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_partition_result_1(add_partitions_to_txn_partition_result_1()) -> iodata().

encode_add_partitions_to_txn_partition_result_1(
    _Args = #{
        % The partition indexes.
        partition_index := PartitionIndex,
        % The response error code.
        partition_error_code := PartitionErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(PartitionErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(PartitionErrorCode)
    ];
encode_add_partitions_to_txn_partition_result_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_error_code => int16
    }).

-spec decode_add_partitions_to_txn_partition_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_partition_result_1(),
    Rest :: binary().

decode_add_partitions_to_txn_partition_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(PartitionErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            partition_error_code => PartitionErrorCode
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_topic_result_1(add_partitions_to_txn_topic_result_1()) -> iodata().

encode_add_partitions_to_txn_topic_result_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The results for each partition
        results_by_partition := ResultsByPartition
    }
) when
    ?is_string(Name),
    ?is_array(ResultsByPartition)
->
    [
        ?encode_string(Name),
        ?encode_array(ResultsByPartition, fun encode_add_partitions_to_txn_partition_result_1/1)
    ];
encode_add_partitions_to_txn_topic_result_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        results_by_partition => {array, add_partitions_to_txn_partition_result_1}
    }).

-spec decode_add_partitions_to_txn_topic_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_result_1(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(ResultsByPartition, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_partition_result_1)),
    {
        #{
            name => Name,
            results_by_partition => ResultsByPartition
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_response_2(add_partitions_to_txn_response_2()) -> iodata().

encode_add_partitions_to_txn_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic.
        results_by_topic_v3_and_below := ResultsByTopicV3AndBelow
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(ResultsByTopicV3AndBelow)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(ResultsByTopicV3AndBelow, fun encode_add_partitions_to_txn_topic_result_2/1)
    ];
encode_add_partitions_to_txn_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results_by_topic_v3_and_below => {array, add_partitions_to_txn_topic_result_2}
    }).

-spec decode_add_partitions_to_txn_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_response_2(),
    Rest :: binary().

decode_add_partitions_to_txn_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(ResultsByTopicV3AndBelow, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_topic_result_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results_by_topic_v3_and_below => ResultsByTopicV3AndBelow
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_partition_result_2(add_partitions_to_txn_partition_result_2()) -> iodata().

encode_add_partitions_to_txn_partition_result_2(
    _Args = #{
        % The partition indexes.
        partition_index := PartitionIndex,
        % The response error code.
        partition_error_code := PartitionErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(PartitionErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(PartitionErrorCode)
    ];
encode_add_partitions_to_txn_partition_result_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_error_code => int16
    }).

-spec decode_add_partitions_to_txn_partition_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_partition_result_2(),
    Rest :: binary().

decode_add_partitions_to_txn_partition_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(PartitionErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            partition_error_code => PartitionErrorCode
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_topic_result_2(add_partitions_to_txn_topic_result_2()) -> iodata().

encode_add_partitions_to_txn_topic_result_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The results for each partition
        results_by_partition := ResultsByPartition
    }
) when
    ?is_string(Name),
    ?is_array(ResultsByPartition)
->
    [
        ?encode_string(Name),
        ?encode_array(ResultsByPartition, fun encode_add_partitions_to_txn_partition_result_2/1)
    ];
encode_add_partitions_to_txn_topic_result_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        results_by_partition => {array, add_partitions_to_txn_partition_result_2}
    }).

-spec decode_add_partitions_to_txn_topic_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_result_2(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(ResultsByPartition, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_partition_result_2)),
    {
        #{
            name => Name,
            results_by_partition => ResultsByPartition
        },
        Bin2
    }.

-spec encode_add_partitions_to_txn_response_3(add_partitions_to_txn_response_3()) -> iodata().

encode_add_partitions_to_txn_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic.
        results_by_topic_v3_and_below := ResultsByTopicV3AndBelow
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(ResultsByTopicV3AndBelow)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(ResultsByTopicV3AndBelow, fun encode_add_partitions_to_txn_topic_result_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results_by_topic_v3_and_below => {array, add_partitions_to_txn_topic_result_3}
    }).

-spec decode_add_partitions_to_txn_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_response_3(),
    Rest :: binary().

decode_add_partitions_to_txn_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(ResultsByTopicV3AndBelow, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_topic_result_3)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results_by_topic_v3_and_below => ResultsByTopicV3AndBelow
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_response_3().

decode_add_partitions_to_txn_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_partition_result_3(add_partitions_to_txn_partition_result_3()) -> iodata().

encode_add_partitions_to_txn_partition_result_3(
    _Args = #{
        % The partition indexes.
        partition_index := PartitionIndex,
        % The response error code.
        partition_error_code := PartitionErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(PartitionErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(PartitionErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_partition_result_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_error_code => int16
    }).

-spec decode_add_partitions_to_txn_partition_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_partition_result_3(),
    Rest :: binary().

decode_add_partitions_to_txn_partition_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(PartitionErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_partition_result_3_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            partition_error_code => PartitionErrorCode
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_partition_result_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_partition_result_3().

decode_add_partitions_to_txn_partition_result_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_topic_result_3(add_partitions_to_txn_topic_result_3()) -> iodata().

encode_add_partitions_to_txn_topic_result_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The results for each partition
        results_by_partition := ResultsByPartition
    }
) when
    ?is_string(Name),
    ?is_array(ResultsByPartition)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(ResultsByPartition, fun encode_add_partitions_to_txn_partition_result_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_topic_result_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        results_by_partition => {array, add_partitions_to_txn_partition_result_3}
    }).

-spec decode_add_partitions_to_txn_topic_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_result_3(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(ResultsByPartition, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_partition_result_3)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_topic_result_3_tagged_field/3,
        #{
            name => Name,
            results_by_partition => ResultsByPartition
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_topic_result_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_topic_result_3().

decode_add_partitions_to_txn_topic_result_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_response_4(add_partitions_to_txn_response_4()) -> iodata().

encode_add_partitions_to_txn_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The response top level error code.
        error_code := ErrorCode,
        % Results categorized by transactional ID.
        results_by_transaction := ResultsByTransaction
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(ResultsByTransaction)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(ResultsByTransaction, fun encode_add_partitions_to_txn_result_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        results_by_transaction => {array, add_partitions_to_txn_result_4}
    }).

-spec decode_add_partitions_to_txn_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_response_4(),
    Rest :: binary().

decode_add_partitions_to_txn_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(ResultsByTransaction, Bin2, Bin3, ?_decode_element(decode_add_partitions_to_txn_result_4)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            results_by_transaction => ResultsByTransaction
        },
        Bin3
    ).

-spec decode_add_partitions_to_txn_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_response_4().

decode_add_partitions_to_txn_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_result_4(add_partitions_to_txn_result_4()) -> iodata().

encode_add_partitions_to_txn_result_4(
    _Args = #{
        % The transactional id corresponding to the transaction.
        transactional_id := TransactionalId,
        % The results for each topic.
        topic_results := TopicResults
    }
) when
    ?is_string(TransactionalId),
    ?is_array(TopicResults)
->
    [
        ?encode_compact_string(TransactionalId),
        ?encode_compact_array(TopicResults, fun encode_add_partitions_to_txn_topic_result_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_result_4(Args) ->
    ?encoder_error(Args, #{
        transactional_id => string,
        topic_results => {array, add_partitions_to_txn_topic_result_4}
    }).

-spec decode_add_partitions_to_txn_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_result_4(),
    Rest :: binary().

decode_add_partitions_to_txn_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TransactionalId, Bin0, Bin1),
    ?_decode_compact_array(TopicResults, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_topic_result_4)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_result_4_tagged_field/3,
        #{
            transactional_id => TransactionalId,
            topic_results => TopicResults
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_result_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_result_4().

decode_add_partitions_to_txn_result_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_partition_result_4(add_partitions_to_txn_partition_result_4()) -> iodata().

encode_add_partitions_to_txn_partition_result_4(
    _Args = #{
        % The partition indexes.
        partition_index := PartitionIndex,
        % The response error code.
        partition_error_code := PartitionErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(PartitionErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(PartitionErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_partition_result_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_error_code => int16
    }).

-spec decode_add_partitions_to_txn_partition_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_partition_result_4(),
    Rest :: binary().

decode_add_partitions_to_txn_partition_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(PartitionErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_partition_result_4_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            partition_error_code => PartitionErrorCode
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_partition_result_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_partition_result_4().

decode_add_partitions_to_txn_partition_result_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_topic_result_4(add_partitions_to_txn_topic_result_4()) -> iodata().

encode_add_partitions_to_txn_topic_result_4(
    _Args = #{
        % The topic name.
        name := Name,
        % The results for each partition
        results_by_partition := ResultsByPartition
    }
) when
    ?is_string(Name),
    ?is_array(ResultsByPartition)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(ResultsByPartition, fun encode_add_partitions_to_txn_partition_result_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_topic_result_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        results_by_partition => {array, add_partitions_to_txn_partition_result_4}
    }).

-spec decode_add_partitions_to_txn_topic_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_result_4(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(ResultsByPartition, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_partition_result_4)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_topic_result_4_tagged_field/3,
        #{
            name => Name,
            results_by_partition => ResultsByPartition
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_topic_result_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_topic_result_4().

decode_add_partitions_to_txn_topic_result_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_response_5(add_partitions_to_txn_response_5()) -> iodata().

encode_add_partitions_to_txn_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The response top level error code.
        error_code := ErrorCode,
        % Results categorized by transactional ID.
        results_by_transaction := ResultsByTransaction
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(ResultsByTransaction)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(ResultsByTransaction, fun encode_add_partitions_to_txn_result_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        results_by_transaction => {array, add_partitions_to_txn_result_5}
    }).

-spec decode_add_partitions_to_txn_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_response_5(),
    Rest :: binary().

decode_add_partitions_to_txn_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(ResultsByTransaction, Bin2, Bin3, ?_decode_element(decode_add_partitions_to_txn_result_5)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            results_by_transaction => ResultsByTransaction
        },
        Bin3
    ).

-spec decode_add_partitions_to_txn_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_response_5().

decode_add_partitions_to_txn_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_result_5(add_partitions_to_txn_result_5()) -> iodata().

encode_add_partitions_to_txn_result_5(
    _Args = #{
        % The transactional id corresponding to the transaction.
        transactional_id := TransactionalId,
        % The results for each topic.
        topic_results := TopicResults
    }
) when
    ?is_string(TransactionalId),
    ?is_array(TopicResults)
->
    [
        ?encode_compact_string(TransactionalId),
        ?encode_compact_array(TopicResults, fun encode_add_partitions_to_txn_topic_result_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_result_5(Args) ->
    ?encoder_error(Args, #{
        transactional_id => string,
        topic_results => {array, add_partitions_to_txn_topic_result_5}
    }).

-spec decode_add_partitions_to_txn_result_5(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_result_5(),
    Rest :: binary().

decode_add_partitions_to_txn_result_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TransactionalId, Bin0, Bin1),
    ?_decode_compact_array(TopicResults, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_topic_result_5)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_result_5_tagged_field/3,
        #{
            transactional_id => TransactionalId,
            topic_results => TopicResults
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_result_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_result_5().

decode_add_partitions_to_txn_result_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_partition_result_5(add_partitions_to_txn_partition_result_5()) -> iodata().

encode_add_partitions_to_txn_partition_result_5(
    _Args = #{
        % The partition indexes.
        partition_index := PartitionIndex,
        % The response error code.
        partition_error_code := PartitionErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(PartitionErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(PartitionErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_partition_result_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        partition_error_code => int16
    }).

-spec decode_add_partitions_to_txn_partition_result_5(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_partition_result_5(),
    Rest :: binary().

decode_add_partitions_to_txn_partition_result_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(PartitionErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_partition_result_5_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            partition_error_code => PartitionErrorCode
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_partition_result_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_partition_result_5().

decode_add_partitions_to_txn_partition_result_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_add_partitions_to_txn_topic_result_5(add_partitions_to_txn_topic_result_5()) -> iodata().

encode_add_partitions_to_txn_topic_result_5(
    _Args = #{
        % The topic name.
        name := Name,
        % The results for each partition
        results_by_partition := ResultsByPartition
    }
) when
    ?is_string(Name),
    ?is_array(ResultsByPartition)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(ResultsByPartition, fun encode_add_partitions_to_txn_partition_result_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_add_partitions_to_txn_topic_result_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        results_by_partition => {array, add_partitions_to_txn_partition_result_5}
    }).

-spec decode_add_partitions_to_txn_topic_result_5(binary()) -> {Decoded, Rest} when
    Decoded :: add_partitions_to_txn_topic_result_5(),
    Rest :: binary().

decode_add_partitions_to_txn_topic_result_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(ResultsByPartition, Bin1, Bin2, ?_decode_element(decode_add_partitions_to_txn_partition_result_5)),
    ?decode_tagged_fields(
        fun decode_add_partitions_to_txn_topic_result_5_tagged_field/3,
        #{
            name => Name,
            results_by_partition => ResultsByPartition
        },
        Bin2
    ).

-spec decode_add_partitions_to_txn_topic_result_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: add_partitions_to_txn_topic_result_5().

decode_add_partitions_to_txn_topic_result_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type add_partitions_to_txn_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results_by_topic_v3_and_below := list(add_partitions_to_txn_topic_result_0())
}.
-type add_partitions_to_txn_partition_result_0() :: #{
    partition_index := integer(),
    partition_error_code := integer()
}.
-type add_partitions_to_txn_topic_result_0() :: #{
    name := binary(),
    results_by_partition := list(add_partitions_to_txn_partition_result_0())
}.
-type add_partitions_to_txn_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results_by_topic_v3_and_below := list(add_partitions_to_txn_topic_result_1())
}.
-type add_partitions_to_txn_partition_result_1() :: #{
    partition_index := integer(),
    partition_error_code := integer()
}.
-type add_partitions_to_txn_topic_result_1() :: #{
    name := binary(),
    results_by_partition := list(add_partitions_to_txn_partition_result_1())
}.
-type add_partitions_to_txn_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results_by_topic_v3_and_below := list(add_partitions_to_txn_topic_result_2())
}.
-type add_partitions_to_txn_partition_result_2() :: #{
    partition_index := integer(),
    partition_error_code := integer()
}.
-type add_partitions_to_txn_topic_result_2() :: #{
    name := binary(),
    results_by_partition := list(add_partitions_to_txn_partition_result_2())
}.
-type add_partitions_to_txn_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results_by_topic_v3_and_below := list(add_partitions_to_txn_topic_result_3())
}.
-type add_partitions_to_txn_partition_result_3() :: #{
    partition_index := integer(),
    partition_error_code := integer()
}.
-type add_partitions_to_txn_topic_result_3() :: #{
    name := binary(),
    results_by_partition := list(add_partitions_to_txn_partition_result_3())
}.
-type add_partitions_to_txn_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    results_by_transaction := list(add_partitions_to_txn_result_4())
}.
-type add_partitions_to_txn_result_4() :: #{
    transactional_id := binary(),
    topic_results := list(add_partitions_to_txn_topic_result_4())
}.
-type add_partitions_to_txn_partition_result_4() :: #{
    partition_index := integer(),
    partition_error_code := integer()
}.
-type add_partitions_to_txn_topic_result_4() :: #{
    name := binary(),
    results_by_partition := list(add_partitions_to_txn_partition_result_4())
}.
-type add_partitions_to_txn_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    results_by_transaction := list(add_partitions_to_txn_result_5())
}.
-type add_partitions_to_txn_result_5() :: #{
    transactional_id := binary(),
    topic_results := list(add_partitions_to_txn_topic_result_5())
}.
-type add_partitions_to_txn_partition_result_5() :: #{
    partition_index := integer(),
    partition_error_code := integer()
}.
-type add_partitions_to_txn_topic_result_5() :: #{
    name := binary(),
    results_by_partition := list(add_partitions_to_txn_partition_result_5())
}.
