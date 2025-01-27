-module(delete_topics_response).
-export([
    encode_delete_topics_response_0/1,
    decode_delete_topics_response_0/1,
    encode_delete_topics_response_1/1,
    decode_delete_topics_response_1/1,
    encode_delete_topics_response_2/1,
    decode_delete_topics_response_2/1,
    encode_delete_topics_response_3/1,
    decode_delete_topics_response_3/1,
    encode_delete_topics_response_4/1,
    decode_delete_topics_response_4/1,
    encode_delete_topics_response_5/1,
    decode_delete_topics_response_5/1,
    encode_delete_topics_response_6/1,
    decode_delete_topics_response_6/1
]).
-export_type([
    delete_topics_response_0/0,
    deletable_topic_result_0/0,
    delete_topics_response_1/0,
    deletable_topic_result_1/0,
    delete_topics_response_2/0,
    deletable_topic_result_2/0,
    delete_topics_response_3/0,
    deletable_topic_result_3/0,
    delete_topics_response_4/0,
    deletable_topic_result_4/0,
    delete_topics_response_5/0,
    deletable_topic_result_5/0,
    delete_topics_response_6/0,
    deletable_topic_result_6/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_delete_topics_response_0(delete_topics_response_0()) -> iodata().

encode_delete_topics_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The results for each topic we tried to delete.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Responses, fun encode_deletable_topic_result_0/1)
    ];
encode_delete_topics_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        responses => {array, deletable_topic_result_0}
    }).

-spec decode_delete_topics_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_response_0(),
    Rest :: binary().

decode_delete_topics_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Responses, Bin0, Bin1, ?_decode_element(decode_deletable_topic_result_0)),
    {
        Header#{
            responses => Responses
        },
        Bin1
    }.

-spec encode_deletable_topic_result_0(deletable_topic_result_0()) -> iodata().

encode_deletable_topic_result_0(
    _Args = #{
        % The topic name
        name := Name,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode)
    ];
encode_deletable_topic_result_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16
    }).

-spec decode_deletable_topic_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_topic_result_0(),
    Rest :: binary().

decode_deletable_topic_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            name => Name,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_delete_topics_response_1(delete_topics_response_1()) -> iodata().

encode_delete_topics_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic we tried to delete.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_deletable_topic_result_1/1)
    ];
encode_delete_topics_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, deletable_topic_result_1}
    }).

-spec decode_delete_topics_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_response_1(),
    Rest :: binary().

decode_delete_topics_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_deletable_topic_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_deletable_topic_result_1(deletable_topic_result_1()) -> iodata().

encode_deletable_topic_result_1(
    _Args = #{
        % The topic name
        name := Name,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode)
    ];
encode_deletable_topic_result_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16
    }).

-spec decode_deletable_topic_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_topic_result_1(),
    Rest :: binary().

decode_deletable_topic_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            name => Name,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_delete_topics_response_2(delete_topics_response_2()) -> iodata().

encode_delete_topics_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic we tried to delete.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_deletable_topic_result_2/1)
    ];
encode_delete_topics_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, deletable_topic_result_2}
    }).

-spec decode_delete_topics_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_response_2(),
    Rest :: binary().

decode_delete_topics_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_deletable_topic_result_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_deletable_topic_result_2(deletable_topic_result_2()) -> iodata().

encode_deletable_topic_result_2(
    _Args = #{
        % The topic name
        name := Name,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode)
    ];
encode_deletable_topic_result_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16
    }).

-spec decode_deletable_topic_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_topic_result_2(),
    Rest :: binary().

decode_deletable_topic_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            name => Name,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_delete_topics_response_3(delete_topics_response_3()) -> iodata().

encode_delete_topics_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic we tried to delete.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_deletable_topic_result_3/1)
    ];
encode_delete_topics_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, deletable_topic_result_3}
    }).

-spec decode_delete_topics_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_response_3(),
    Rest :: binary().

decode_delete_topics_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_deletable_topic_result_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_deletable_topic_result_3(deletable_topic_result_3()) -> iodata().

encode_deletable_topic_result_3(
    _Args = #{
        % The topic name
        name := Name,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode)
    ];
encode_deletable_topic_result_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16
    }).

-spec decode_deletable_topic_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_topic_result_3(),
    Rest :: binary().

decode_deletable_topic_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            name => Name,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_delete_topics_response_4(delete_topics_response_4()) -> iodata().

encode_delete_topics_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic we tried to delete.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Responses, fun encode_deletable_topic_result_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_topics_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, deletable_topic_result_4}
    }).

-spec decode_delete_topics_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_response_4(),
    Rest :: binary().

decode_delete_topics_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Responses, Bin1, Bin2, ?_decode_element(decode_deletable_topic_result_4)),
    ?decode_tagged_fields(
        fun decode_delete_topics_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    ).

-spec decode_delete_topics_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_topics_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_deletable_topic_result_4(deletable_topic_result_4()) -> iodata().

encode_deletable_topic_result_4(
    _Args = #{
        % The topic name
        name := Name,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode)
->
    [
        ?encode_compact_string(Name),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_deletable_topic_result_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16
    }).

-spec decode_deletable_topic_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_topic_result_4(),
    Rest :: binary().

decode_deletable_topic_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_deletable_topic_result_4_tagged_field/3,
        #{
            name => Name,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_deletable_topic_result_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_deletable_topic_result_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_topics_response_5(delete_topics_response_5()) -> iodata().

encode_delete_topics_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic we tried to delete.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Responses, fun encode_deletable_topic_result_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_topics_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, deletable_topic_result_5}
    }).

-spec decode_delete_topics_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_response_5(),
    Rest :: binary().

decode_delete_topics_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Responses, Bin1, Bin2, ?_decode_element(decode_deletable_topic_result_5)),
    ?decode_tagged_fields(
        fun decode_delete_topics_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    ).

-spec decode_delete_topics_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_topics_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_deletable_topic_result_5(deletable_topic_result_5()) -> iodata().

encode_deletable_topic_result_5(
    _Args = #{
        % The topic name
        name := Name,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_compact_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_deletable_topic_result_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_deletable_topic_result_5(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_topic_result_5(),
    Rest :: binary().

decode_deletable_topic_result_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_deletable_topic_result_5_tagged_field/3,
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    ).

-spec decode_deletable_topic_result_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_deletable_topic_result_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_topics_response_6(delete_topics_response_6()) -> iodata().

encode_delete_topics_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each topic we tried to delete.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Responses, fun encode_deletable_topic_result_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_topics_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, deletable_topic_result_6}
    }).

-spec decode_delete_topics_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_response_6(),
    Rest :: binary().

decode_delete_topics_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Responses, Bin1, Bin2, ?_decode_element(decode_deletable_topic_result_6)),
    ?decode_tagged_fields(
        fun decode_delete_topics_response_6_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    ).

-spec decode_delete_topics_response_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_topics_response_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_deletable_topic_result_6(deletable_topic_result_6()) -> iodata().

encode_deletable_topic_result_6(
    _Args = #{
        % The topic name
        name := Name,
        % the unique topic ID
        topic_id := TopicId,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_nullable_string(Name),
    ?is_uuid(TopicId),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_compact_nullable_string(Name),
        ?encode_uuid(TopicId),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_deletable_topic_result_6(Args) ->
    ?encoder_error(Args, #{
        name => nullable_string,
        topic_id => uuid,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_deletable_topic_result_6(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_topic_result_6(),
    Rest :: binary().

decode_deletable_topic_result_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_nullable_string(Name, Bin0, Bin1),
    ?_decode_uuid(TopicId, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?_decode_compact_nullable_string(ErrorMessage, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_deletable_topic_result_6_tagged_field/3,
        #{
            name => Name,
            topic_id => TopicId,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin4
    ).

-spec decode_deletable_topic_result_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_deletable_topic_result_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type delete_topics_response_0() :: #{
    correlation_id => integer(),
    responses := list(deletable_topic_result_0())
}.
-type deletable_topic_result_0() :: #{
    name := binary(),
    error_code := integer()
}.
-type delete_topics_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(deletable_topic_result_1())
}.
-type deletable_topic_result_1() :: #{
    name := binary(),
    error_code := integer()
}.
-type delete_topics_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(deletable_topic_result_2())
}.
-type deletable_topic_result_2() :: #{
    name := binary(),
    error_code := integer()
}.
-type delete_topics_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(deletable_topic_result_3())
}.
-type deletable_topic_result_3() :: #{
    name := binary(),
    error_code := integer()
}.
-type delete_topics_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(deletable_topic_result_4())
}.
-type deletable_topic_result_4() :: #{
    name := binary(),
    error_code := integer()
}.
-type delete_topics_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(deletable_topic_result_5())
}.
-type deletable_topic_result_5() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type delete_topics_response_6() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(deletable_topic_result_6())
}.
-type deletable_topic_result_6() :: #{
    name := binary() | null,
    topic_id := kafcod:uuid(),
    error_code := integer(),
    error_message := binary() | null
}.
