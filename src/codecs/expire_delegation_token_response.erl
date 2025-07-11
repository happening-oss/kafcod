-module(expire_delegation_token_response).
-export([
    encode_expire_delegation_token_response_0/1,
    decode_expire_delegation_token_response_0/1,
    encode_expire_delegation_token_response_1/1,
    decode_expire_delegation_token_response_1/1,
    encode_expire_delegation_token_response_2/1,
    decode_expire_delegation_token_response_2/1
]).
-export_type([
    expire_delegation_token_response_0/0,
    expire_delegation_token_response_1/0,
    expire_delegation_token_response_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_expire_delegation_token_response_0(expire_delegation_token_response_0()) -> iodata().

encode_expire_delegation_token_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp in milliseconds at which this token expires.
        expiry_timestamp_ms := ExpiryTimestampMs,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_int64(ExpiryTimestampMs),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_int64(ExpiryTimestampMs),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_expire_delegation_token_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        expiry_timestamp_ms => int64,
        throttle_time_ms => int32
    }).

-spec decode_expire_delegation_token_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: expire_delegation_token_response_0(),
    Rest :: binary().

decode_expire_delegation_token_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int64(ExpiryTimestampMs, Bin1, Bin2),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    {
        Header#{
            error_code => ErrorCode,
            expiry_timestamp_ms => ExpiryTimestampMs,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    }.

-spec encode_expire_delegation_token_response_1(expire_delegation_token_response_1()) -> iodata().

encode_expire_delegation_token_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp in milliseconds at which this token expires.
        expiry_timestamp_ms := ExpiryTimestampMs,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_int64(ExpiryTimestampMs),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_int64(ExpiryTimestampMs),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_expire_delegation_token_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        expiry_timestamp_ms => int64,
        throttle_time_ms => int32
    }).

-spec decode_expire_delegation_token_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: expire_delegation_token_response_1(),
    Rest :: binary().

decode_expire_delegation_token_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int64(ExpiryTimestampMs, Bin1, Bin2),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    {
        Header#{
            error_code => ErrorCode,
            expiry_timestamp_ms => ExpiryTimestampMs,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    }.

-spec encode_expire_delegation_token_response_2(expire_delegation_token_response_2()) -> iodata().

encode_expire_delegation_token_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The timestamp in milliseconds at which this token expires.
        expiry_timestamp_ms := ExpiryTimestampMs,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_int64(ExpiryTimestampMs),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_int64(ExpiryTimestampMs),
        ?encode_int32(ThrottleTimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_expire_delegation_token_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        expiry_timestamp_ms => int64,
        throttle_time_ms => int32
    }).

-spec decode_expire_delegation_token_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: expire_delegation_token_response_2(),
    Rest :: binary().

decode_expire_delegation_token_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int64(ExpiryTimestampMs, Bin1, Bin2),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_expire_delegation_token_response_2_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            expiry_timestamp_ms => ExpiryTimestampMs,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    ).

-spec decode_expire_delegation_token_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: expire_delegation_token_response_2().

decode_expire_delegation_token_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type expire_delegation_token_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    expiry_timestamp_ms := integer(),
    throttle_time_ms := integer()
}.
-type expire_delegation_token_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    expiry_timestamp_ms := integer(),
    throttle_time_ms := integer()
}.
-type expire_delegation_token_response_2() :: #{
    correlation_id => integer(),
    error_code := integer(),
    expiry_timestamp_ms := integer(),
    throttle_time_ms := integer()
}.
