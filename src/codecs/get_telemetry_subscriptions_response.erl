-module(get_telemetry_subscriptions_response).
-export([
    encode_get_telemetry_subscriptions_response_0/1,
    decode_get_telemetry_subscriptions_response_0/1
]).
-export_type([
    get_telemetry_subscriptions_response_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_get_telemetry_subscriptions_response_0(get_telemetry_subscriptions_response_0()) -> iodata().

encode_get_telemetry_subscriptions_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Assigned client instance id if ClientInstanceId was 0 in the request, else 0.
        client_instance_id := ClientInstanceId,
        % Unique identifier for the current subscription set for this client instance.
        subscription_id := SubscriptionId,
        % Compression types that broker accepts for the PushTelemetryRequest.
        accepted_compression_types := AcceptedCompressionTypes,
        % Configured push interval, which is the lowest configured interval in the current subscription set.
        push_interval_ms := PushIntervalMs,
        % The maximum bytes of binary data the broker accepts in PushTelemetryRequest.
        telemetry_max_bytes := TelemetryMaxBytes,
        % Flag to indicate monotonic/counter metrics are to be emitted as deltas or cumulative values
        delta_temporality := DeltaTemporality,
        % Requested metrics prefix string match. Empty array: No metrics subscribed, Array[0] empty string: All metrics subscribed.
        requested_metrics := RequestedMetrics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_uuid(ClientInstanceId),
    ?is_int32(SubscriptionId),
    ?is_array(AcceptedCompressionTypes),
    ?is_int32(PushIntervalMs),
    ?is_int32(TelemetryMaxBytes),
    ?is_bool(DeltaTemporality),
    ?is_array(RequestedMetrics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_uuid(ClientInstanceId),
        ?encode_int32(SubscriptionId),
        ?encode_compact_array(AcceptedCompressionTypes, ?encode_int8_),
        ?encode_int32(PushIntervalMs),
        ?encode_int32(TelemetryMaxBytes),
        ?encode_bool(DeltaTemporality),
        ?encode_compact_array(RequestedMetrics, ?encode_compact_string_),
        ?EMPTY_TAG_BUFFER
    ];
encode_get_telemetry_subscriptions_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        client_instance_id => uuid,
        subscription_id => int32,
        accepted_compression_types => {array, int8},
        push_interval_ms => int32,
        telemetry_max_bytes => int32,
        delta_temporality => bool,
        requested_metrics => {array, string}
    }).

-spec decode_get_telemetry_subscriptions_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: get_telemetry_subscriptions_response_0(),
    Rest :: binary().

decode_get_telemetry_subscriptions_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_uuid(ClientInstanceId, Bin2, Bin3),
    ?_decode_int32(SubscriptionId, Bin3, Bin4),
    ?_decode_compact_array(AcceptedCompressionTypes, Bin4, Bin5, ?decode_int8_),
    ?_decode_int32(PushIntervalMs, Bin5, Bin6),
    ?_decode_int32(TelemetryMaxBytes, Bin6, Bin7),
    ?_decode_bool(DeltaTemporality, Bin7, Bin8),
    ?_decode_compact_array(RequestedMetrics, Bin8, Bin9, ?decode_string_),
    ?decode_tagged_fields(
        fun decode_get_telemetry_subscriptions_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            client_instance_id => ClientInstanceId,
            subscription_id => SubscriptionId,
            accepted_compression_types => AcceptedCompressionTypes,
            push_interval_ms => PushIntervalMs,
            telemetry_max_bytes => TelemetryMaxBytes,
            delta_temporality => DeltaTemporality,
            requested_metrics => RequestedMetrics
        },
        Bin9
    ).

-spec decode_get_telemetry_subscriptions_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_get_telemetry_subscriptions_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type get_telemetry_subscriptions_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    client_instance_id := kafcod:uuid(),
    subscription_id := integer(),
    accepted_compression_types := list(integer()),
    push_interval_ms := integer(),
    telemetry_max_bytes := integer(),
    delta_temporality := boolean(),
    requested_metrics := list(binary())
}.
