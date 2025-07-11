-module(list_client_metrics_resources_request).
-export([
    encode_list_client_metrics_resources_request_0/1,
    decode_list_client_metrics_resources_request_0/1
]).
-export_type([
    list_client_metrics_resources_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(LIST_CLIENT_METRICS_RESOURCES_REQUEST, 74).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_client_metrics_resources_request_0(list_client_metrics_resources_request_0()) -> iodata().

encode_list_client_metrics_resources_request_0(
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
        ?encode_request_header_2(?LIST_CLIENT_METRICS_RESOURCES_REQUEST, 0, CorrelationId, ClientId),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_client_metrics_resources_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string
    }).

-spec decode_list_client_metrics_resources_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_client_metrics_resources_request_0(),
    Rest :: binary().

decode_list_client_metrics_resources_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?decode_tagged_fields(
        fun decode_list_client_metrics_resources_request_0_tagged_field/3,
        Header#{

        },
        Bin0
    ).

-spec decode_list_client_metrics_resources_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_client_metrics_resources_request_0().

decode_list_client_metrics_resources_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_client_metrics_resources_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null
}.
