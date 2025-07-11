-module(heartbeat_response).
-export([
    encode_heartbeat_response_0/1,
    decode_heartbeat_response_0/1,
    encode_heartbeat_response_1/1,
    decode_heartbeat_response_1/1,
    encode_heartbeat_response_2/1,
    decode_heartbeat_response_2/1,
    encode_heartbeat_response_3/1,
    decode_heartbeat_response_3/1,
    encode_heartbeat_response_4/1,
    decode_heartbeat_response_4/1
]).
-export_type([
    heartbeat_response_0/0,
    heartbeat_response_1/0,
    heartbeat_response_2/0,
    heartbeat_response_3/0,
    heartbeat_response_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_heartbeat_response_0(heartbeat_response_0()) -> iodata().

encode_heartbeat_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode)
    ];
encode_heartbeat_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_heartbeat_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_response_0(),
    Rest :: binary().

decode_heartbeat_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    {
        Header#{
            error_code => ErrorCode
        },
        Bin1
    }.

-spec encode_heartbeat_response_1(heartbeat_response_1()) -> iodata().

encode_heartbeat_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode)
    ];
encode_heartbeat_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16
    }).

-spec decode_heartbeat_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_response_1(),
    Rest :: binary().

decode_heartbeat_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_heartbeat_response_2(heartbeat_response_2()) -> iodata().

encode_heartbeat_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode)
    ];
encode_heartbeat_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16
    }).

-spec decode_heartbeat_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_response_2(),
    Rest :: binary().

decode_heartbeat_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_heartbeat_response_3(heartbeat_response_3()) -> iodata().

encode_heartbeat_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode)
    ];
encode_heartbeat_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16
    }).

-spec decode_heartbeat_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_response_3(),
    Rest :: binary().

decode_heartbeat_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_heartbeat_response_4(heartbeat_response_4()) -> iodata().

encode_heartbeat_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_heartbeat_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16
    }).

-spec decode_heartbeat_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_response_4(),
    Rest :: binary().

decode_heartbeat_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_heartbeat_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_heartbeat_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: heartbeat_response_4().

decode_heartbeat_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type heartbeat_response_0() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type heartbeat_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer()
}.
-type heartbeat_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer()
}.
-type heartbeat_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer()
}.
-type heartbeat_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer()
}.
