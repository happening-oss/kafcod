-module(sasl_handshake_response).
-export([
    encode_sasl_handshake_response_0/1,
    decode_sasl_handshake_response_0/1,
    encode_sasl_handshake_response_1/1,
    decode_sasl_handshake_response_1/1
]).
-export_type([
    sasl_handshake_response_0/0,
    sasl_handshake_response_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_sasl_handshake_response_0(sasl_handshake_response_0()) -> iodata().

encode_sasl_handshake_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The mechanisms enabled in the server.
        mechanisms := Mechanisms
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Mechanisms)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(Mechanisms, ?encode_string_)
    ];
encode_sasl_handshake_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        mechanisms => {array, string}
    }).

-spec decode_sasl_handshake_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_handshake_response_0(),
    Rest :: binary().

decode_sasl_handshake_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(Mechanisms, Bin1, Bin2, ?decode_string_),
    {
        Header#{
            error_code => ErrorCode,
            mechanisms => Mechanisms
        },
        Bin2
    }.

-spec encode_sasl_handshake_response_1(sasl_handshake_response_1()) -> iodata().

encode_sasl_handshake_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The mechanisms enabled in the server.
        mechanisms := Mechanisms
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Mechanisms)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(Mechanisms, ?encode_string_)
    ];
encode_sasl_handshake_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        mechanisms => {array, string}
    }).

-spec decode_sasl_handshake_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_handshake_response_1(),
    Rest :: binary().

decode_sasl_handshake_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(Mechanisms, Bin1, Bin2, ?decode_string_),
    {
        Header#{
            error_code => ErrorCode,
            mechanisms => Mechanisms
        },
        Bin2
    }.

-type sasl_handshake_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    mechanisms := list(binary())
}.
-type sasl_handshake_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    mechanisms := list(binary())
}.
