-module(api_versions_request).
-export([
    encode_api_versions_request_0/1,
    decode_api_versions_request_0/1,
    encode_api_versions_request_1/1,
    decode_api_versions_request_1/1,
    encode_api_versions_request_2/1,
    decode_api_versions_request_2/1,
    encode_api_versions_request_3/1,
    decode_api_versions_request_3/1
]).
-export_type([
    api_versions_request_0/0,
    api_versions_request_1/0,
    api_versions_request_2/0,
    api_versions_request_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(API_VERSIONS_REQUEST, 18).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_api_versions_request_0(api_versions_request_0()) -> iodata().

encode_api_versions_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId)
->
    [
        ?encode_request_header_1(?API_VERSIONS_REQUEST, 0, CorrelationId, ClientId)
    ];
encode_api_versions_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string
    }).

-spec decode_api_versions_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: api_versions_request_0(),
    Rest :: binary().

decode_api_versions_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    {
        Header#{

        },
        Bin0
    }.

-spec encode_api_versions_request_1(api_versions_request_1()) -> iodata().

encode_api_versions_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId)
->
    [
        ?encode_request_header_1(?API_VERSIONS_REQUEST, 1, CorrelationId, ClientId)
    ];
encode_api_versions_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string
    }).

-spec decode_api_versions_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: api_versions_request_1(),
    Rest :: binary().

decode_api_versions_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    {
        Header#{

        },
        Bin0
    }.

-spec encode_api_versions_request_2(api_versions_request_2()) -> iodata().

encode_api_versions_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId)
->
    [
        ?encode_request_header_1(?API_VERSIONS_REQUEST, 2, CorrelationId, ClientId)
    ];
encode_api_versions_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string
    }).

-spec decode_api_versions_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: api_versions_request_2(),
    Rest :: binary().

decode_api_versions_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    {
        Header#{

        },
        Bin0
    }.

-spec encode_api_versions_request_3(api_versions_request_3()) -> iodata().

encode_api_versions_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The name of the client.
        client_software_name := ClientSoftwareName,
        % The version of the client.
        client_software_version := ClientSoftwareVersion
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(ClientSoftwareName),
    ?is_string(ClientSoftwareVersion)
->
    [
        ?encode_request_header_2(?API_VERSIONS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_string(ClientSoftwareName),
        ?encode_compact_string(ClientSoftwareVersion),
        ?EMPTY_TAG_BUFFER
    ];
encode_api_versions_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        client_software_name => string,
        client_software_version => string
    }).

-spec decode_api_versions_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: api_versions_request_3(),
    Rest :: binary().

decode_api_versions_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(ClientSoftwareName, Bin0, Bin1),
    ?_decode_compact_string(ClientSoftwareVersion, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_api_versions_request_3_tagged_field/3,
        Header#{
            client_software_name => ClientSoftwareName,
            client_software_version => ClientSoftwareVersion
        },
        Bin2
    ).

-spec decode_api_versions_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_api_versions_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type api_versions_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null
}.
-type api_versions_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null
}.
-type api_versions_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null
}.
-type api_versions_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    client_software_name := binary(),
    client_software_version := binary()
}.
