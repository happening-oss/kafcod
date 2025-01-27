-module(create_partitions_response).
-export([
    encode_create_partitions_response_0/1,
    decode_create_partitions_response_0/1,
    encode_create_partitions_response_1/1,
    decode_create_partitions_response_1/1,
    encode_create_partitions_response_2/1,
    decode_create_partitions_response_2/1,
    encode_create_partitions_response_3/1,
    decode_create_partitions_response_3/1
]).
-export_type([
    create_partitions_response_0/0,
    create_partitions_topic_result_0/0,
    create_partitions_response_1/0,
    create_partitions_topic_result_1/0,
    create_partitions_response_2/0,
    create_partitions_topic_result_2/0,
    create_partitions_response_3/0,
    create_partitions_topic_result_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_create_partitions_response_0(create_partitions_response_0()) -> iodata().

encode_create_partitions_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The partition creation results for each topic.
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
        ?encode_array(Results, fun encode_create_partitions_topic_result_0/1)
    ];
encode_create_partitions_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, create_partitions_topic_result_0}
    }).

-spec decode_create_partitions_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_response_0(),
    Rest :: binary().

decode_create_partitions_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_create_partitions_topic_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_create_partitions_topic_result_0(create_partitions_topic_result_0()) -> iodata().

encode_create_partitions_topic_result_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The result error, or zero if there was no error.
        error_code := ErrorCode,
        % The result message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_create_partitions_topic_result_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_create_partitions_topic_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_topic_result_0(),
    Rest :: binary().

decode_create_partitions_topic_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    {
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    }.

-spec encode_create_partitions_response_1(create_partitions_response_1()) -> iodata().

encode_create_partitions_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The partition creation results for each topic.
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
        ?encode_array(Results, fun encode_create_partitions_topic_result_1/1)
    ];
encode_create_partitions_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, create_partitions_topic_result_1}
    }).

-spec decode_create_partitions_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_response_1(),
    Rest :: binary().

decode_create_partitions_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_create_partitions_topic_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_create_partitions_topic_result_1(create_partitions_topic_result_1()) -> iodata().

encode_create_partitions_topic_result_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The result error, or zero if there was no error.
        error_code := ErrorCode,
        % The result message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_create_partitions_topic_result_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_create_partitions_topic_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_topic_result_1(),
    Rest :: binary().

decode_create_partitions_topic_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    {
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    }.

-spec encode_create_partitions_response_2(create_partitions_response_2()) -> iodata().

encode_create_partitions_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The partition creation results for each topic.
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
        ?encode_compact_array(Results, fun encode_create_partitions_topic_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_partitions_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, create_partitions_topic_result_2}
    }).

-spec decode_create_partitions_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_response_2(),
    Rest :: binary().

decode_create_partitions_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Results, Bin1, Bin2, ?_decode_element(decode_create_partitions_topic_result_2)),
    ?decode_tagged_fields(
        fun decode_create_partitions_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    ).

-spec decode_create_partitions_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_partitions_topic_result_2(create_partitions_topic_result_2()) -> iodata().

encode_create_partitions_topic_result_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The result error, or zero if there was no error.
        error_code := ErrorCode,
        % The result message, or null if there was no error.
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
encode_create_partitions_topic_result_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_create_partitions_topic_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_topic_result_2(),
    Rest :: binary().

decode_create_partitions_topic_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_create_partitions_topic_result_2_tagged_field/3,
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    ).

-spec decode_create_partitions_topic_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_topic_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_partitions_response_3(create_partitions_response_3()) -> iodata().

encode_create_partitions_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The partition creation results for each topic.
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
        ?encode_compact_array(Results, fun encode_create_partitions_topic_result_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_partitions_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, create_partitions_topic_result_3}
    }).

-spec decode_create_partitions_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_response_3(),
    Rest :: binary().

decode_create_partitions_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Results, Bin1, Bin2, ?_decode_element(decode_create_partitions_topic_result_3)),
    ?decode_tagged_fields(
        fun decode_create_partitions_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    ).

-spec decode_create_partitions_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_partitions_topic_result_3(create_partitions_topic_result_3()) -> iodata().

encode_create_partitions_topic_result_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The result error, or zero if there was no error.
        error_code := ErrorCode,
        % The result message, or null if there was no error.
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
encode_create_partitions_topic_result_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_create_partitions_topic_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_topic_result_3(),
    Rest :: binary().

decode_create_partitions_topic_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_create_partitions_topic_result_3_tagged_field/3,
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    ).

-spec decode_create_partitions_topic_result_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_topic_result_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type create_partitions_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(create_partitions_topic_result_0())
}.
-type create_partitions_topic_result_0() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type create_partitions_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(create_partitions_topic_result_1())
}.
-type create_partitions_topic_result_1() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type create_partitions_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(create_partitions_topic_result_2())
}.
-type create_partitions_topic_result_2() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type create_partitions_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(create_partitions_topic_result_3())
}.
-type create_partitions_topic_result_3() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
