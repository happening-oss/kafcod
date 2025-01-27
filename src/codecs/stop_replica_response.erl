-module(stop_replica_response).
-export([
    encode_stop_replica_response_0/1,
    decode_stop_replica_response_0/1,
    encode_stop_replica_response_1/1,
    decode_stop_replica_response_1/1,
    encode_stop_replica_response_2/1,
    decode_stop_replica_response_2/1,
    encode_stop_replica_response_3/1,
    decode_stop_replica_response_3/1,
    encode_stop_replica_response_4/1,
    decode_stop_replica_response_4/1
]).
-export_type([
    stop_replica_response_0/0,
    stop_replica_partition_error_0/0,
    stop_replica_response_1/0,
    stop_replica_partition_error_1/0,
    stop_replica_response_2/0,
    stop_replica_partition_error_2/0,
    stop_replica_response_3/0,
    stop_replica_partition_error_3/0,
    stop_replica_response_4/0,
    stop_replica_partition_error_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_stop_replica_response_0(stop_replica_response_0()) -> iodata().

encode_stop_replica_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code, or 0 if there was no top-level error.
        error_code := ErrorCode,
        % The responses for each partition.
        partition_errors := PartitionErrors
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(PartitionErrors)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(PartitionErrors, fun encode_stop_replica_partition_error_0/1)
    ];
encode_stop_replica_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, stop_replica_partition_error_0}
    }).

-spec decode_stop_replica_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_response_0(),
    Rest :: binary().

decode_stop_replica_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_stop_replica_partition_error_0)),
    {
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    }.

-spec encode_stop_replica_partition_error_0(stop_replica_partition_error_0()) -> iodata().

encode_stop_replica_partition_error_0(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no partition error.
        error_code := ErrorCode
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_stop_replica_partition_error_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_stop_replica_partition_error_0(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_partition_error_0(),
    Rest :: binary().

decode_stop_replica_partition_error_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin3
    }.

-spec encode_stop_replica_response_1(stop_replica_response_1()) -> iodata().

encode_stop_replica_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code, or 0 if there was no top-level error.
        error_code := ErrorCode,
        % The responses for each partition.
        partition_errors := PartitionErrors
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(PartitionErrors)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(PartitionErrors, fun encode_stop_replica_partition_error_1/1)
    ];
encode_stop_replica_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, stop_replica_partition_error_1}
    }).

-spec decode_stop_replica_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_response_1(),
    Rest :: binary().

decode_stop_replica_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_stop_replica_partition_error_1)),
    {
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    }.

-spec encode_stop_replica_partition_error_1(stop_replica_partition_error_1()) -> iodata().

encode_stop_replica_partition_error_1(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no partition error.
        error_code := ErrorCode
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode)
    ];
encode_stop_replica_partition_error_1(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_stop_replica_partition_error_1(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_partition_error_1(),
    Rest :: binary().

decode_stop_replica_partition_error_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    {
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin3
    }.

-spec encode_stop_replica_response_2(stop_replica_response_2()) -> iodata().

encode_stop_replica_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code, or 0 if there was no top-level error.
        error_code := ErrorCode,
        % The responses for each partition.
        partition_errors := PartitionErrors
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(PartitionErrors)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(PartitionErrors, fun encode_stop_replica_partition_error_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, stop_replica_partition_error_2}
    }).

-spec decode_stop_replica_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_response_2(),
    Rest :: binary().

decode_stop_replica_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_stop_replica_partition_error_2)),
    ?decode_tagged_fields(
        fun decode_stop_replica_response_2_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    ).

-spec decode_stop_replica_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_partition_error_2(stop_replica_partition_error_2()) -> iodata().

encode_stop_replica_partition_error_2(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no partition error.
        error_code := ErrorCode
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_partition_error_2(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_stop_replica_partition_error_2(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_partition_error_2(),
    Rest :: binary().

decode_stop_replica_partition_error_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_stop_replica_partition_error_2_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin3
    ).

-spec decode_stop_replica_partition_error_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_partition_error_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_response_3(stop_replica_response_3()) -> iodata().

encode_stop_replica_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code, or 0 if there was no top-level error.
        error_code := ErrorCode,
        % The responses for each partition.
        partition_errors := PartitionErrors
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(PartitionErrors)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(PartitionErrors, fun encode_stop_replica_partition_error_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, stop_replica_partition_error_3}
    }).

-spec decode_stop_replica_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_response_3(),
    Rest :: binary().

decode_stop_replica_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_stop_replica_partition_error_3)),
    ?decode_tagged_fields(
        fun decode_stop_replica_response_3_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    ).

-spec decode_stop_replica_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_partition_error_3(stop_replica_partition_error_3()) -> iodata().

encode_stop_replica_partition_error_3(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no partition error.
        error_code := ErrorCode
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_partition_error_3(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_stop_replica_partition_error_3(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_partition_error_3(),
    Rest :: binary().

decode_stop_replica_partition_error_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_stop_replica_partition_error_3_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin3
    ).

-spec decode_stop_replica_partition_error_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_partition_error_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_response_4(stop_replica_response_4()) -> iodata().

encode_stop_replica_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code, or 0 if there was no top-level error.
        error_code := ErrorCode,
        % The responses for each partition.
        partition_errors := PartitionErrors
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(PartitionErrors)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(PartitionErrors, fun encode_stop_replica_partition_error_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, stop_replica_partition_error_4}
    }).

-spec decode_stop_replica_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_response_4(),
    Rest :: binary().

decode_stop_replica_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_stop_replica_partition_error_4)),
    ?decode_tagged_fields(
        fun decode_stop_replica_response_4_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    ).

-spec decode_stop_replica_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_stop_replica_partition_error_4(stop_replica_partition_error_4()) -> iodata().

encode_stop_replica_partition_error_4(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no partition error.
        error_code := ErrorCode
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_stop_replica_partition_error_4(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_stop_replica_partition_error_4(binary()) -> {Decoded, Rest} when
    Decoded :: stop_replica_partition_error_4(),
    Rest :: binary().

decode_stop_replica_partition_error_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_stop_replica_partition_error_4_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin3
    ).

-spec decode_stop_replica_partition_error_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_stop_replica_partition_error_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type stop_replica_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(stop_replica_partition_error_0())
}.
-type stop_replica_partition_error_0() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type stop_replica_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(stop_replica_partition_error_1())
}.
-type stop_replica_partition_error_1() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type stop_replica_response_2() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(stop_replica_partition_error_2())
}.
-type stop_replica_partition_error_2() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type stop_replica_response_3() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(stop_replica_partition_error_3())
}.
-type stop_replica_partition_error_3() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type stop_replica_response_4() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(stop_replica_partition_error_4())
}.
-type stop_replica_partition_error_4() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
