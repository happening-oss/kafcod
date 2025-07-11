-module(sasl_authenticate_response).
-export([
    encode_sasl_authenticate_response_0/1,
    decode_sasl_authenticate_response_0/1,
    encode_sasl_authenticate_response_1/1,
    decode_sasl_authenticate_response_1/1,
    encode_sasl_authenticate_response_2/1,
    decode_sasl_authenticate_response_2/1
]).
-export_type([
    sasl_authenticate_response_0/0,
    sasl_authenticate_response_1/0,
    sasl_authenticate_response_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_sasl_authenticate_response_0(sasl_authenticate_response_0()) -> iodata().

encode_sasl_authenticate_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % The SASL authentication bytes from the server, as defined by the SASL mechanism.
        auth_bytes := AuthBytes
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_bytes(AuthBytes)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_bytes(AuthBytes)
    ];
encode_sasl_authenticate_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        error_message => nullable_string,
        auth_bytes => bytes
    }).

-spec decode_sasl_authenticate_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_authenticate_response_0(),
    Rest :: binary().

decode_sasl_authenticate_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_bytes(AuthBytes, Bin2, Bin3),
    {
        Header#{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            auth_bytes => AuthBytes
        },
        Bin3
    }.

-spec encode_sasl_authenticate_response_1(sasl_authenticate_response_1()) -> iodata().

encode_sasl_authenticate_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % The SASL authentication bytes from the server, as defined by the SASL mechanism.
        auth_bytes := AuthBytes,
        % Number of milliseconds after which only re-authentication over the existing connection to create a new session can occur.
        session_lifetime_ms := SessionLifetimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_bytes(AuthBytes),
    ?is_int64(SessionLifetimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_bytes(AuthBytes),
        ?encode_int64(SessionLifetimeMs)
    ];
encode_sasl_authenticate_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        error_message => nullable_string,
        auth_bytes => bytes,
        session_lifetime_ms => int64
    }).

-spec decode_sasl_authenticate_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_authenticate_response_1(),
    Rest :: binary().

decode_sasl_authenticate_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_bytes(AuthBytes, Bin2, Bin3),
    ?_decode_int64(SessionLifetimeMs, Bin3, Bin4),
    {
        Header#{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            auth_bytes => AuthBytes,
            session_lifetime_ms => SessionLifetimeMs
        },
        Bin4
    }.

-spec encode_sasl_authenticate_response_2(sasl_authenticate_response_2()) -> iodata().

encode_sasl_authenticate_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % The SASL authentication bytes from the server, as defined by the SASL mechanism.
        auth_bytes := AuthBytes,
        % Number of milliseconds after which only re-authentication over the existing connection to create a new session can occur.
        session_lifetime_ms := SessionLifetimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_bytes(AuthBytes),
    ?is_int64(SessionLifetimeMs)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_bytes(AuthBytes),
        ?encode_int64(SessionLifetimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_sasl_authenticate_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        error_message => nullable_string,
        auth_bytes => bytes,
        session_lifetime_ms => int64
    }).

-spec decode_sasl_authenticate_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_authenticate_response_2(),
    Rest :: binary().

decode_sasl_authenticate_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_compact_bytes(AuthBytes, Bin2, Bin3),
    ?_decode_int64(SessionLifetimeMs, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_sasl_authenticate_response_2_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            auth_bytes => AuthBytes,
            session_lifetime_ms => SessionLifetimeMs
        },
        Bin4
    ).

-spec decode_sasl_authenticate_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: sasl_authenticate_response_2().

decode_sasl_authenticate_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type sasl_authenticate_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    error_message := binary() | null,
    auth_bytes := kafcod:bytes()
}.
-type sasl_authenticate_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    error_message := binary() | null,
    auth_bytes := kafcod:bytes(),
    session_lifetime_ms := integer()
}.
-type sasl_authenticate_response_2() :: #{
    correlation_id => integer(),
    error_code := integer(),
    error_message := binary() | null,
    auth_bytes := kafcod:bytes(),
    session_lifetime_ms := integer()
}.
