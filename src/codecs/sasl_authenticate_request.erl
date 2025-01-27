-module(sasl_authenticate_request).
-export([
    encode_sasl_authenticate_request_0/1,
    decode_sasl_authenticate_request_0/1,
    encode_sasl_authenticate_request_1/1,
    decode_sasl_authenticate_request_1/1,
    encode_sasl_authenticate_request_2/1,
    decode_sasl_authenticate_request_2/1
]).
-export_type([
    sasl_authenticate_request_0/0,
    sasl_authenticate_request_1/0,
    sasl_authenticate_request_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(SASL_AUTHENTICATE_REQUEST, 36).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_sasl_authenticate_request_0(sasl_authenticate_request_0()) -> iodata().

encode_sasl_authenticate_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The SASL authentication bytes from the client, as defined by the SASL mechanism.
        auth_bytes := AuthBytes
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bytes(AuthBytes)
->
    [
        ?encode_request_header_1(?SASL_AUTHENTICATE_REQUEST, 0, CorrelationId, ClientId),
        ?encode_bytes(AuthBytes)
    ];
encode_sasl_authenticate_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        auth_bytes => bytes
    }).

-spec decode_sasl_authenticate_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_authenticate_request_0(),
    Rest :: binary().

decode_sasl_authenticate_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_bytes(AuthBytes, Bin0, Bin1),
    {
        Header#{
            auth_bytes => AuthBytes
        },
        Bin1
    }.

-spec encode_sasl_authenticate_request_1(sasl_authenticate_request_1()) -> iodata().

encode_sasl_authenticate_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The SASL authentication bytes from the client, as defined by the SASL mechanism.
        auth_bytes := AuthBytes
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bytes(AuthBytes)
->
    [
        ?encode_request_header_1(?SASL_AUTHENTICATE_REQUEST, 1, CorrelationId, ClientId),
        ?encode_bytes(AuthBytes)
    ];
encode_sasl_authenticate_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        auth_bytes => bytes
    }).

-spec decode_sasl_authenticate_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_authenticate_request_1(),
    Rest :: binary().

decode_sasl_authenticate_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_bytes(AuthBytes, Bin0, Bin1),
    {
        Header#{
            auth_bytes => AuthBytes
        },
        Bin1
    }.

-spec encode_sasl_authenticate_request_2(sasl_authenticate_request_2()) -> iodata().

encode_sasl_authenticate_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The SASL authentication bytes from the client, as defined by the SASL mechanism.
        auth_bytes := AuthBytes
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bytes(AuthBytes)
->
    [
        ?encode_request_header_2(?SASL_AUTHENTICATE_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_bytes(AuthBytes),
        ?EMPTY_TAG_BUFFER
    ];
encode_sasl_authenticate_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        auth_bytes => bytes
    }).

-spec decode_sasl_authenticate_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: sasl_authenticate_request_2(),
    Rest :: binary().

decode_sasl_authenticate_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_bytes(AuthBytes, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_sasl_authenticate_request_2_tagged_field/3,
        Header#{
            auth_bytes => AuthBytes
        },
        Bin1
    ).

-spec decode_sasl_authenticate_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_sasl_authenticate_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type sasl_authenticate_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    auth_bytes := kafcod:bytes()
}.
-type sasl_authenticate_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    auth_bytes := kafcod:bytes()
}.
-type sasl_authenticate_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    auth_bytes := kafcod:bytes()
}.
