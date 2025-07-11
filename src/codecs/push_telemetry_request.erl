-module(push_telemetry_request).
-export([
    encode_push_telemetry_request_0/1,
    decode_push_telemetry_request_0/1
]).
-export_type([
    push_telemetry_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(PUSH_TELEMETRY_REQUEST, 72).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_push_telemetry_request_0(push_telemetry_request_0()) -> iodata().

encode_push_telemetry_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Unique id for this client instance.
        client_instance_id := ClientInstanceId,
        % Unique identifier for the current subscription.
        subscription_id := SubscriptionId,
        % Client is terminating the connection.
        terminating := Terminating,
        % Compression codec used to compress the metrics.
        compression_type := CompressionType,
        % Metrics encoded in OpenTelemetry MetricsData v1 protobuf format.
        metrics := Metrics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_uuid(ClientInstanceId),
    ?is_int32(SubscriptionId),
    ?is_bool(Terminating),
    ?is_int8(CompressionType),
    ?is_bytes(Metrics)
->
    [
        ?encode_request_header_2(?PUSH_TELEMETRY_REQUEST, 0, CorrelationId, ClientId),
        ?encode_uuid(ClientInstanceId),
        ?encode_int32(SubscriptionId),
        ?encode_bool(Terminating),
        ?encode_int8(CompressionType),
        ?encode_compact_bytes(Metrics),
        ?EMPTY_TAG_BUFFER
    ];
encode_push_telemetry_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        client_instance_id => uuid,
        subscription_id => int32,
        terminating => bool,
        compression_type => int8,
        metrics => bytes
    }).

-spec decode_push_telemetry_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: push_telemetry_request_0(),
    Rest :: binary().

decode_push_telemetry_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_uuid(ClientInstanceId, Bin0, Bin1),
    ?_decode_int32(SubscriptionId, Bin1, Bin2),
    ?_decode_bool(Terminating, Bin2, Bin3),
    ?_decode_int8(CompressionType, Bin3, Bin4),
    ?_decode_compact_bytes(Metrics, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_push_telemetry_request_0_tagged_field/3,
        Header#{
            client_instance_id => ClientInstanceId,
            subscription_id => SubscriptionId,
            terminating => Terminating,
            compression_type => CompressionType,
            metrics => Metrics
        },
        Bin5
    ).

-spec decode_push_telemetry_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: push_telemetry_request_0().

decode_push_telemetry_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type push_telemetry_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    client_instance_id := kafcod:uuid(),
    subscription_id := integer(),
    terminating := boolean(),
    compression_type := integer(),
    metrics := kafcod:bytes()
}.
