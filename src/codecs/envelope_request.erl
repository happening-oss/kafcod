-module(envelope_request).
-export([
    encode_envelope_request_0/1,
    decode_envelope_request_0/1
]).
-export_type([
    envelope_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ENVELOPE_REQUEST, 58).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_envelope_request_0(envelope_request_0()) -> iodata().

encode_envelope_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The embedded request header and data.
        request_data := RequestData,
        % Value of the initial client principal when the request is redirected by a broker.
        request_principal := RequestPrincipal,
        % The original client's address in bytes.
        client_host_address := ClientHostAddress
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bytes(RequestData),
    ?is_nullable_bytes(RequestPrincipal),
    ?is_bytes(ClientHostAddress)
->
    [
        ?encode_request_header_2(?ENVELOPE_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_bytes(RequestData),
        ?encode_compact_nullable_bytes(RequestPrincipal),
        ?encode_compact_bytes(ClientHostAddress),
        ?EMPTY_TAG_BUFFER
    ];
encode_envelope_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        request_data => bytes,
        request_principal => nullable_bytes,
        client_host_address => bytes
    }).

-spec decode_envelope_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: envelope_request_0(),
    Rest :: binary().

decode_envelope_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_bytes(RequestData, Bin0, Bin1),
    ?_decode_compact_nullable_bytes(RequestPrincipal, Bin1, Bin2),
    ?_decode_compact_bytes(ClientHostAddress, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_envelope_request_0_tagged_field/3,
        Header#{
            request_data => RequestData,
            request_principal => RequestPrincipal,
            client_host_address => ClientHostAddress
        },
        Bin3
    ).

-spec decode_envelope_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: envelope_request_0().

decode_envelope_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type envelope_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    request_data := kafcod:bytes(),
    request_principal := kafcod:nullable_bytes(),
    client_host_address := kafcod:bytes()
}.
