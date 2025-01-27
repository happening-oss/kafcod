-module(broker_heartbeat_response).
-export([
    encode_broker_heartbeat_response_0/1,
    decode_broker_heartbeat_response_0/1
]).
-export_type([
    broker_heartbeat_response_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_broker_heartbeat_response_0(broker_heartbeat_response_0()) -> iodata().

encode_broker_heartbeat_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % True if the broker has approximately caught up with the latest metadata.
        is_caught_up := IsCaughtUp,
        % True if the broker is fenced.
        is_fenced := IsFenced,
        % True if the broker should proceed with its shutdown.
        should_shut_down := ShouldShutDown
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_bool(IsCaughtUp),
    ?is_bool(IsFenced),
    ?is_bool(ShouldShutDown)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_bool(IsCaughtUp),
        ?encode_bool(IsFenced),
        ?encode_bool(ShouldShutDown),
        ?EMPTY_TAG_BUFFER
    ];
encode_broker_heartbeat_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        is_caught_up => bool,
        is_fenced => bool,
        should_shut_down => bool
    }).

-spec decode_broker_heartbeat_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: broker_heartbeat_response_0(),
    Rest :: binary().

decode_broker_heartbeat_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_bool(IsCaughtUp, Bin2, Bin3),
    ?_decode_bool(IsFenced, Bin3, Bin4),
    ?_decode_bool(ShouldShutDown, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_broker_heartbeat_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            is_caught_up => IsCaughtUp,
            is_fenced => IsFenced,
            should_shut_down => ShouldShutDown
        },
        Bin5
    ).

-spec decode_broker_heartbeat_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_broker_heartbeat_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type broker_heartbeat_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    is_caught_up := boolean(),
    is_fenced := boolean(),
    should_shut_down := boolean()
}.
