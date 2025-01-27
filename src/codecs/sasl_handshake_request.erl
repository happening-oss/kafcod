-module(sasl_handshake_request).
-export([
    encode_sasl_handshake_request_0/1,
    decode_sasl_handshake_request_0/1,
    encode_sasl_handshake_request_1/1,
    decode_sasl_handshake_request_1/1
]).
-export_type([
    sasl_handshake_request_0/0,
    sasl_handshake_request_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(SASL_HANDSHAKE_REQUEST, 17).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_sasl_handshake_request_0(sasl_handshake_request_0()) -> iodata().

encode_sasl_handshake_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The SASL mechanism chosen by the client.
        mechanism := Mechanism
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(Mechanism)
->
    [
        ?encode_request_header_1(?SASL_HANDSHAKE_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(Mechanism)
    ];
encode_sasl_handshake_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        mechanism => string
    }).

-spec decode_sasl_handshake_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_handshake_request_0(),
    Rest :: binary().

decode_sasl_handshake_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(Mechanism, Bin0, Bin1),
    {
        Header#{
            mechanism => Mechanism
        },
        Bin1
    }.

-spec encode_sasl_handshake_request_1(sasl_handshake_request_1()) -> iodata().

encode_sasl_handshake_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The SASL mechanism chosen by the client.
        mechanism := Mechanism
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(Mechanism)
->
    [
        ?encode_request_header_1(?SASL_HANDSHAKE_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(Mechanism)
    ];
encode_sasl_handshake_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        mechanism => string
    }).

-spec decode_sasl_handshake_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_handshake_request_1(),
    Rest :: binary().

decode_sasl_handshake_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(Mechanism, Bin0, Bin1),
    {
        Header#{
            mechanism => Mechanism
        },
        Bin1
    }.

-type sasl_handshake_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    mechanism := binary()
}.
-type sasl_handshake_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    mechanism := binary()
}.
