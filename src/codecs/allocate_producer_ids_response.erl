-module(allocate_producer_ids_response).
-export([
    encode_allocate_producer_ids_response_0/1,
    decode_allocate_producer_ids_response_0/1
]).
-export_type([
    allocate_producer_ids_response_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_allocate_producer_ids_response_0(allocate_producer_ids_response_0()) -> iodata().

encode_allocate_producer_ids_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code
        error_code := ErrorCode,
        % The first producer ID in this range, inclusive
        producer_id_start := ProducerIdStart,
        % The number of producer IDs in this range
        producer_id_len := ProducerIdLen
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int64(ProducerIdStart),
    ?is_int32(ProducerIdLen)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int64(ProducerIdStart),
        ?encode_int32(ProducerIdLen),
        ?EMPTY_TAG_BUFFER
    ];
encode_allocate_producer_ids_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        producer_id_start => int64,
        producer_id_len => int32
    }).

-spec decode_allocate_producer_ids_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: allocate_producer_ids_response_0(),
    Rest :: binary().

decode_allocate_producer_ids_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(ProducerIdStart, Bin2, Bin3),
    ?_decode_int32(ProducerIdLen, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_allocate_producer_ids_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            producer_id_start => ProducerIdStart,
            producer_id_len => ProducerIdLen
        },
        Bin4
    ).

-spec decode_allocate_producer_ids_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_allocate_producer_ids_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type allocate_producer_ids_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    producer_id_start := integer(),
    producer_id_len := integer()
}.
