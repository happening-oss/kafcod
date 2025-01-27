-module(envelope_response).
-export([
    encode_envelope_response_0/1,
    decode_envelope_response_0/1
]).
-export_type([
    envelope_response_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_envelope_response_0(envelope_response_0()) -> iodata().

encode_envelope_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The embedded response header and data.
        response_data := ResponseData,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_bytes(ResponseData),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_compact_nullable_bytes(ResponseData),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_envelope_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        response_data => nullable_bytes,
        error_code => int16
    }).

-spec decode_envelope_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: envelope_response_0(),
    Rest :: binary().

decode_envelope_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_compact_nullable_bytes(ResponseData, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_envelope_response_0_tagged_field/3,
        Header#{
            response_data => ResponseData,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_envelope_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_envelope_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type envelope_response_0() :: #{
    correlation_id => integer(),
    response_data := kafcod:nullable_bytes(),
    error_code := integer()
}.
