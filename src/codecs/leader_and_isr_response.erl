-module(leader_and_isr_response).
-export([
    encode_leader_and_isr_response_0/1,
    decode_leader_and_isr_response_0/1,
    encode_leader_and_isr_response_1/1,
    decode_leader_and_isr_response_1/1,
    encode_leader_and_isr_response_2/1,
    decode_leader_and_isr_response_2/1,
    encode_leader_and_isr_response_3/1,
    decode_leader_and_isr_response_3/1,
    encode_leader_and_isr_response_4/1,
    decode_leader_and_isr_response_4/1,
    encode_leader_and_isr_response_5/1,
    decode_leader_and_isr_response_5/1,
    encode_leader_and_isr_response_6/1,
    decode_leader_and_isr_response_6/1,
    encode_leader_and_isr_response_7/1,
    decode_leader_and_isr_response_7/1
]).
-export_type([
    leader_and_isr_response_0/0,
    leader_and_isr_partition_error_0/0,
    leader_and_isr_response_1/0,
    leader_and_isr_partition_error_1/0,
    leader_and_isr_response_2/0,
    leader_and_isr_partition_error_2/0,
    leader_and_isr_response_3/0,
    leader_and_isr_partition_error_3/0,
    leader_and_isr_response_4/0,
    leader_and_isr_partition_error_4/0,
    leader_and_isr_response_5/0,
    leader_and_isr_topic_error_5/0,
    leader_and_isr_partition_error_5/0,
    leader_and_isr_response_6/0,
    leader_and_isr_topic_error_6/0,
    leader_and_isr_partition_error_6/0,
    leader_and_isr_response_7/0,
    leader_and_isr_topic_error_7/0,
    leader_and_isr_partition_error_7/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_leader_and_isr_response_0(leader_and_isr_response_0()) -> iodata().

encode_leader_and_isr_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each partition in v0 to v4 message.
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
        ?encode_array(PartitionErrors, fun encode_leader_and_isr_partition_error_0/1)
    ];
encode_leader_and_isr_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, leader_and_isr_partition_error_0}
    }).

-spec decode_leader_and_isr_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_response_0(),
    Rest :: binary().

decode_leader_and_isr_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_error_0)),
    {
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    }.

-spec encode_leader_and_isr_partition_error_0(leader_and_isr_partition_error_0()) -> iodata().

encode_leader_and_isr_partition_error_0(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
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
encode_leader_and_isr_partition_error_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_leader_and_isr_partition_error_0(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_error_0(),
    Rest :: binary().

decode_leader_and_isr_partition_error_0(Bin0) when is_binary(Bin0) ->
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

-spec encode_leader_and_isr_response_1(leader_and_isr_response_1()) -> iodata().

encode_leader_and_isr_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each partition in v0 to v4 message.
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
        ?encode_array(PartitionErrors, fun encode_leader_and_isr_partition_error_1/1)
    ];
encode_leader_and_isr_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, leader_and_isr_partition_error_1}
    }).

-spec decode_leader_and_isr_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_response_1(),
    Rest :: binary().

decode_leader_and_isr_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_error_1)),
    {
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    }.

-spec encode_leader_and_isr_partition_error_1(leader_and_isr_partition_error_1()) -> iodata().

encode_leader_and_isr_partition_error_1(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
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
encode_leader_and_isr_partition_error_1(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_leader_and_isr_partition_error_1(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_error_1(),
    Rest :: binary().

decode_leader_and_isr_partition_error_1(Bin0) when is_binary(Bin0) ->
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

-spec encode_leader_and_isr_response_2(leader_and_isr_response_2()) -> iodata().

encode_leader_and_isr_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each partition in v0 to v4 message.
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
        ?encode_array(PartitionErrors, fun encode_leader_and_isr_partition_error_2/1)
    ];
encode_leader_and_isr_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, leader_and_isr_partition_error_2}
    }).

-spec decode_leader_and_isr_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_response_2(),
    Rest :: binary().

decode_leader_and_isr_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_error_2)),
    {
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    }.

-spec encode_leader_and_isr_partition_error_2(leader_and_isr_partition_error_2()) -> iodata().

encode_leader_and_isr_partition_error_2(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
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
encode_leader_and_isr_partition_error_2(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_leader_and_isr_partition_error_2(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_error_2(),
    Rest :: binary().

decode_leader_and_isr_partition_error_2(Bin0) when is_binary(Bin0) ->
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

-spec encode_leader_and_isr_response_3(leader_and_isr_response_3()) -> iodata().

encode_leader_and_isr_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each partition in v0 to v4 message.
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
        ?encode_array(PartitionErrors, fun encode_leader_and_isr_partition_error_3/1)
    ];
encode_leader_and_isr_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, leader_and_isr_partition_error_3}
    }).

-spec decode_leader_and_isr_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_response_3(),
    Rest :: binary().

decode_leader_and_isr_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_error_3)),
    {
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    }.

-spec encode_leader_and_isr_partition_error_3(leader_and_isr_partition_error_3()) -> iodata().

encode_leader_and_isr_partition_error_3(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
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
encode_leader_and_isr_partition_error_3(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_leader_and_isr_partition_error_3(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_error_3(),
    Rest :: binary().

decode_leader_and_isr_partition_error_3(Bin0) when is_binary(Bin0) ->
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

-spec encode_leader_and_isr_response_4(leader_and_isr_response_4()) -> iodata().

encode_leader_and_isr_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each partition in v0 to v4 message.
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
        ?encode_compact_array(PartitionErrors, fun encode_leader_and_isr_partition_error_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        partition_errors => {array, leader_and_isr_partition_error_4}
    }).

-spec decode_leader_and_isr_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_response_4(),
    Rest :: binary().

decode_leader_and_isr_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_error_4)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_response_4_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            partition_errors => PartitionErrors
        },
        Bin2
    ).

-spec decode_leader_and_isr_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_response_4().

decode_leader_and_isr_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_partition_error_4(leader_and_isr_partition_error_4()) -> iodata().

encode_leader_and_isr_partition_error_4(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
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
encode_leader_and_isr_partition_error_4(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32,
        error_code => int16
    }).

-spec decode_leader_and_isr_partition_error_4(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_error_4(),
    Rest :: binary().

decode_leader_and_isr_partition_error_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_partition_error_4_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin3
    ).

-spec decode_leader_and_isr_partition_error_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_partition_error_4().

decode_leader_and_isr_partition_error_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_response_5(leader_and_isr_response_5()) -> iodata().

encode_leader_and_isr_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each topic
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_leader_and_isr_topic_error_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        topics => {array, leader_and_isr_topic_error_5}
    }).

-spec decode_leader_and_isr_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_response_5(),
    Rest :: binary().

decode_leader_and_isr_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_topic_error_5)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_response_5_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            topics => Topics
        },
        Bin2
    ).

-spec decode_leader_and_isr_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_response_5().

decode_leader_and_isr_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_topic_error_5(leader_and_isr_topic_error_5()) -> iodata().

encode_leader_and_isr_topic_error_5(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % Each partition.
        partition_errors := PartitionErrors
    }
) when
    ?is_uuid(TopicId),
    ?is_array(PartitionErrors)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(PartitionErrors, fun encode_leader_and_isr_partition_error_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_topic_error_5(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partition_errors => {array, leader_and_isr_partition_error_5}
    }).

-spec decode_leader_and_isr_topic_error_5(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_error_5(),
    Rest :: binary().

decode_leader_and_isr_topic_error_5(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_error_5)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_topic_error_5_tagged_field/3,
        #{
            topic_id => TopicId,
            partition_errors => PartitionErrors
        },
        Bin2
    ).

-spec decode_leader_and_isr_topic_error_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_topic_error_5().

decode_leader_and_isr_topic_error_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_partition_error_5(leader_and_isr_partition_error_5()) -> iodata().

encode_leader_and_isr_partition_error_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
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
encode_leader_and_isr_partition_error_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_leader_and_isr_partition_error_5(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_error_5(),
    Rest :: binary().

decode_leader_and_isr_partition_error_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_partition_error_5_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_leader_and_isr_partition_error_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_partition_error_5().

decode_leader_and_isr_partition_error_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_response_6(leader_and_isr_response_6()) -> iodata().

encode_leader_and_isr_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each topic
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_leader_and_isr_topic_error_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        topics => {array, leader_and_isr_topic_error_6}
    }).

-spec decode_leader_and_isr_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_response_6(),
    Rest :: binary().

decode_leader_and_isr_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_topic_error_6)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_response_6_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            topics => Topics
        },
        Bin2
    ).

-spec decode_leader_and_isr_response_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_response_6().

decode_leader_and_isr_response_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_topic_error_6(leader_and_isr_topic_error_6()) -> iodata().

encode_leader_and_isr_topic_error_6(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % Each partition.
        partition_errors := PartitionErrors
    }
) when
    ?is_uuid(TopicId),
    ?is_array(PartitionErrors)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(PartitionErrors, fun encode_leader_and_isr_partition_error_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_topic_error_6(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partition_errors => {array, leader_and_isr_partition_error_6}
    }).

-spec decode_leader_and_isr_topic_error_6(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_error_6(),
    Rest :: binary().

decode_leader_and_isr_topic_error_6(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_error_6)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_topic_error_6_tagged_field/3,
        #{
            topic_id => TopicId,
            partition_errors => PartitionErrors
        },
        Bin2
    ).

-spec decode_leader_and_isr_topic_error_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_topic_error_6().

decode_leader_and_isr_topic_error_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_partition_error_6(leader_and_isr_partition_error_6()) -> iodata().

encode_leader_and_isr_partition_error_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
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
encode_leader_and_isr_partition_error_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_leader_and_isr_partition_error_6(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_error_6(),
    Rest :: binary().

decode_leader_and_isr_partition_error_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_partition_error_6_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_leader_and_isr_partition_error_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_partition_error_6().

decode_leader_and_isr_partition_error_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_response_7(leader_and_isr_response_7()) -> iodata().

encode_leader_and_isr_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each topic
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_leader_and_isr_topic_error_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        topics => {array, leader_and_isr_topic_error_7}
    }).

-spec decode_leader_and_isr_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_response_7(),
    Rest :: binary().

decode_leader_and_isr_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_topic_error_7)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_response_7_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            topics => Topics
        },
        Bin2
    ).

-spec decode_leader_and_isr_response_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_response_7().

decode_leader_and_isr_response_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_topic_error_7(leader_and_isr_topic_error_7()) -> iodata().

encode_leader_and_isr_topic_error_7(
    _Args = #{
        % The unique topic ID
        topic_id := TopicId,
        % Each partition.
        partition_errors := PartitionErrors
    }
) when
    ?is_uuid(TopicId),
    ?is_array(PartitionErrors)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(PartitionErrors, fun encode_leader_and_isr_partition_error_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_and_isr_topic_error_7(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partition_errors => {array, leader_and_isr_partition_error_7}
    }).

-spec decode_leader_and_isr_topic_error_7(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_topic_error_7(),
    Rest :: binary().

decode_leader_and_isr_topic_error_7(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(PartitionErrors, Bin1, Bin2, ?_decode_element(decode_leader_and_isr_partition_error_7)),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_topic_error_7_tagged_field/3,
        #{
            topic_id => TopicId,
            partition_errors => PartitionErrors
        },
        Bin2
    ).

-spec decode_leader_and_isr_topic_error_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_topic_error_7().

decode_leader_and_isr_topic_error_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_and_isr_partition_error_7(leader_and_isr_partition_error_7()) -> iodata().

encode_leader_and_isr_partition_error_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The partition error code, or 0 if there was no error.
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
encode_leader_and_isr_partition_error_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_leader_and_isr_partition_error_7(binary()) -> {Decoded, Rest} when
    Decoded :: leader_and_isr_partition_error_7(),
    Rest :: binary().

decode_leader_and_isr_partition_error_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_and_isr_partition_error_7_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_leader_and_isr_partition_error_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_and_isr_partition_error_7().

decode_leader_and_isr_partition_error_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type leader_and_isr_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(leader_and_isr_partition_error_0())
}.
-type leader_and_isr_partition_error_0() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type leader_and_isr_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(leader_and_isr_partition_error_1())
}.
-type leader_and_isr_partition_error_1() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type leader_and_isr_response_2() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(leader_and_isr_partition_error_2())
}.
-type leader_and_isr_partition_error_2() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type leader_and_isr_response_3() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(leader_and_isr_partition_error_3())
}.
-type leader_and_isr_partition_error_3() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type leader_and_isr_response_4() :: #{
    correlation_id => integer(),
    error_code := integer(),
    partition_errors := list(leader_and_isr_partition_error_4())
}.
-type leader_and_isr_partition_error_4() :: #{
    topic_name := binary(),
    partition_index := integer(),
    error_code := integer()
}.
-type leader_and_isr_response_5() :: #{
    correlation_id => integer(),
    error_code := integer(),
    topics := list(leader_and_isr_topic_error_5())
}.
-type leader_and_isr_topic_error_5() :: #{
    topic_id := kafcod:uuid(),
    partition_errors := list(leader_and_isr_partition_error_5())
}.
-type leader_and_isr_partition_error_5() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type leader_and_isr_response_6() :: #{
    correlation_id => integer(),
    error_code := integer(),
    topics := list(leader_and_isr_topic_error_6())
}.
-type leader_and_isr_topic_error_6() :: #{
    topic_id := kafcod:uuid(),
    partition_errors := list(leader_and_isr_partition_error_6())
}.
-type leader_and_isr_partition_error_6() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type leader_and_isr_response_7() :: #{
    correlation_id => integer(),
    error_code := integer(),
    topics := list(leader_and_isr_topic_error_7())
}.
-type leader_and_isr_topic_error_7() :: #{
    topic_id := kafcod:uuid(),
    partition_errors := list(leader_and_isr_partition_error_7())
}.
-type leader_and_isr_partition_error_7() :: #{
    partition_index := integer(),
    error_code := integer()
}.
