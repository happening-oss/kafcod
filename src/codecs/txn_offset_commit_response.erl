-module(txn_offset_commit_response).
-export([
    encode_txn_offset_commit_response_0/1,
    decode_txn_offset_commit_response_0/1,
    encode_txn_offset_commit_response_1/1,
    decode_txn_offset_commit_response_1/1,
    encode_txn_offset_commit_response_2/1,
    decode_txn_offset_commit_response_2/1,
    encode_txn_offset_commit_response_3/1,
    decode_txn_offset_commit_response_3/1,
    encode_txn_offset_commit_response_4/1,
    decode_txn_offset_commit_response_4/1
]).
-export_type([
    txn_offset_commit_response_0/0,
    txn_offset_commit_response_partition_0/0,
    txn_offset_commit_response_topic_0/0,
    txn_offset_commit_response_1/0,
    txn_offset_commit_response_partition_1/0,
    txn_offset_commit_response_topic_1/0,
    txn_offset_commit_response_2/0,
    txn_offset_commit_response_partition_2/0,
    txn_offset_commit_response_topic_2/0,
    txn_offset_commit_response_3/0,
    txn_offset_commit_response_partition_3/0,
    txn_offset_commit_response_topic_3/0,
    txn_offset_commit_response_4/0,
    txn_offset_commit_response_partition_4/0,
    txn_offset_commit_response_topic_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_txn_offset_commit_response_0(txn_offset_commit_response_0()) -> iodata().

encode_txn_offset_commit_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_txn_offset_commit_response_topic_0/1)
    ];
encode_txn_offset_commit_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, txn_offset_commit_response_topic_0}
    }).

-spec decode_txn_offset_commit_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_0(),
    Rest :: binary().

decode_txn_offset_commit_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_topic_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_partition_0(txn_offset_commit_response_partition_0()) -> iodata().

encode_txn_offset_commit_response_partition_0(
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
encode_txn_offset_commit_response_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_txn_offset_commit_response_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_partition_0(),
    Rest :: binary().

decode_txn_offset_commit_response_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_topic_0(txn_offset_commit_response_topic_0()) -> iodata().

encode_txn_offset_commit_response_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_txn_offset_commit_response_partition_0/1)
    ];
encode_txn_offset_commit_response_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_response_partition_0}
    }).

-spec decode_txn_offset_commit_response_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_topic_0(),
    Rest :: binary().

decode_txn_offset_commit_response_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_1(txn_offset_commit_response_1()) -> iodata().

encode_txn_offset_commit_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_txn_offset_commit_response_topic_1/1)
    ];
encode_txn_offset_commit_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, txn_offset_commit_response_topic_1}
    }).

-spec decode_txn_offset_commit_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_1(),
    Rest :: binary().

decode_txn_offset_commit_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_topic_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_partition_1(txn_offset_commit_response_partition_1()) -> iodata().

encode_txn_offset_commit_response_partition_1(
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
encode_txn_offset_commit_response_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_txn_offset_commit_response_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_partition_1(),
    Rest :: binary().

decode_txn_offset_commit_response_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_topic_1(txn_offset_commit_response_topic_1()) -> iodata().

encode_txn_offset_commit_response_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_txn_offset_commit_response_partition_1/1)
    ];
encode_txn_offset_commit_response_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_response_partition_1}
    }).

-spec decode_txn_offset_commit_response_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_topic_1(),
    Rest :: binary().

decode_txn_offset_commit_response_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_partition_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_2(txn_offset_commit_response_2()) -> iodata().

encode_txn_offset_commit_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_txn_offset_commit_response_topic_2/1)
    ];
encode_txn_offset_commit_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, txn_offset_commit_response_topic_2}
    }).

-spec decode_txn_offset_commit_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_2(),
    Rest :: binary().

decode_txn_offset_commit_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_topic_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_partition_2(txn_offset_commit_response_partition_2()) -> iodata().

encode_txn_offset_commit_response_partition_2(
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
encode_txn_offset_commit_response_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_txn_offset_commit_response_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_partition_2(),
    Rest :: binary().

decode_txn_offset_commit_response_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_topic_2(txn_offset_commit_response_topic_2()) -> iodata().

encode_txn_offset_commit_response_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_txn_offset_commit_response_partition_2/1)
    ];
encode_txn_offset_commit_response_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_response_partition_2}
    }).

-spec decode_txn_offset_commit_response_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_topic_2(),
    Rest :: binary().

decode_txn_offset_commit_response_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_partition_2)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_txn_offset_commit_response_3(txn_offset_commit_response_3()) -> iodata().

encode_txn_offset_commit_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_txn_offset_commit_response_topic_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_txn_offset_commit_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, txn_offset_commit_response_topic_3}
    }).

-spec decode_txn_offset_commit_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_3(),
    Rest :: binary().

decode_txn_offset_commit_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_topic_3)),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_txn_offset_commit_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_txn_offset_commit_response_partition_3(txn_offset_commit_response_partition_3()) -> iodata().

encode_txn_offset_commit_response_partition_3(
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
encode_txn_offset_commit_response_partition_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_txn_offset_commit_response_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_partition_3(),
    Rest :: binary().

decode_txn_offset_commit_response_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_response_partition_3_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_txn_offset_commit_response_partition_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_response_partition_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_txn_offset_commit_response_topic_3(txn_offset_commit_response_topic_3()) -> iodata().

encode_txn_offset_commit_response_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_txn_offset_commit_response_partition_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_txn_offset_commit_response_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_response_partition_3}
    }).

-spec decode_txn_offset_commit_response_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_topic_3(),
    Rest :: binary().

decode_txn_offset_commit_response_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_partition_3)),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_response_topic_3_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_txn_offset_commit_response_topic_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_response_topic_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_txn_offset_commit_response_4(txn_offset_commit_response_4()) -> iodata().

encode_txn_offset_commit_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_txn_offset_commit_response_topic_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_txn_offset_commit_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, txn_offset_commit_response_topic_4}
    }).

-spec decode_txn_offset_commit_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_4(),
    Rest :: binary().

decode_txn_offset_commit_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_topic_4)),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_txn_offset_commit_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_txn_offset_commit_response_partition_4(txn_offset_commit_response_partition_4()) -> iodata().

encode_txn_offset_commit_response_partition_4(
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
encode_txn_offset_commit_response_partition_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_txn_offset_commit_response_partition_4(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_partition_4(),
    Rest :: binary().

decode_txn_offset_commit_response_partition_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_response_partition_4_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_txn_offset_commit_response_partition_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_response_partition_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_txn_offset_commit_response_topic_4(txn_offset_commit_response_topic_4()) -> iodata().

encode_txn_offset_commit_response_topic_4(
    _Args = #{
        % The topic name.
        name := Name,
        % The responses for each partition in the topic.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_txn_offset_commit_response_partition_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_txn_offset_commit_response_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_response_partition_4}
    }).

-spec decode_txn_offset_commit_response_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_response_topic_4(),
    Rest :: binary().

decode_txn_offset_commit_response_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_response_partition_4)),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_response_topic_4_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_txn_offset_commit_response_topic_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_response_topic_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type txn_offset_commit_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(txn_offset_commit_response_topic_0())
}.
-type txn_offset_commit_response_partition_0() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type txn_offset_commit_response_topic_0() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_response_partition_0())
}.
-type txn_offset_commit_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(txn_offset_commit_response_topic_1())
}.
-type txn_offset_commit_response_partition_1() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type txn_offset_commit_response_topic_1() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_response_partition_1())
}.
-type txn_offset_commit_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(txn_offset_commit_response_topic_2())
}.
-type txn_offset_commit_response_partition_2() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type txn_offset_commit_response_topic_2() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_response_partition_2())
}.
-type txn_offset_commit_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(txn_offset_commit_response_topic_3())
}.
-type txn_offset_commit_response_partition_3() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type txn_offset_commit_response_topic_3() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_response_partition_3())
}.
-type txn_offset_commit_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(txn_offset_commit_response_topic_4())
}.
-type txn_offset_commit_response_partition_4() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type txn_offset_commit_response_topic_4() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_response_partition_4())
}.
