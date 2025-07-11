-module(list_client_metrics_resources_response).
-export([
    encode_list_client_metrics_resources_response_0/1,
    decode_list_client_metrics_resources_response_0/1
]).
-export_type([
    list_client_metrics_resources_response_0/0,
    client_metrics_resource_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_client_metrics_resources_response_0(list_client_metrics_resources_response_0()) -> iodata().

encode_list_client_metrics_resources_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each client metrics resource in the response.
        client_metrics_resources := ClientMetricsResources
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(ClientMetricsResources)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(ClientMetricsResources, fun encode_client_metrics_resource_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_client_metrics_resources_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        client_metrics_resources => {array, client_metrics_resource_0}
    }).

-spec decode_list_client_metrics_resources_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_client_metrics_resources_response_0(),
    Rest :: binary().

decode_list_client_metrics_resources_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(ClientMetricsResources, Bin2, Bin3, ?_decode_element(decode_client_metrics_resource_0)),
    ?decode_tagged_fields(
        fun decode_list_client_metrics_resources_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            client_metrics_resources => ClientMetricsResources
        },
        Bin3
    ).

-spec decode_list_client_metrics_resources_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_client_metrics_resources_response_0().

decode_list_client_metrics_resources_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_client_metrics_resource_0(client_metrics_resource_0()) -> iodata().

encode_client_metrics_resource_0(
    _Args = #{
        % The resource name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_compact_string(Name),
        ?EMPTY_TAG_BUFFER
    ];
encode_client_metrics_resource_0(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_client_metrics_resource_0(binary()) -> {Decoded, Rest} when
    Decoded :: client_metrics_resource_0(),
    Rest :: binary().

decode_client_metrics_resource_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_client_metrics_resource_0_tagged_field/3,
        #{
            name => Name
        },
        Bin1
    ).

-spec decode_client_metrics_resource_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: client_metrics_resource_0().

decode_client_metrics_resource_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_client_metrics_resources_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    client_metrics_resources := list(client_metrics_resource_0())
}.
-type client_metrics_resource_0() :: #{
    name := binary()
}.
