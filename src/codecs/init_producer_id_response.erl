-module(init_producer_id_response).
-export([
    encode_init_producer_id_response_0/1,
    decode_init_producer_id_response_0/1,
    encode_init_producer_id_response_1/1,
    decode_init_producer_id_response_1/1,
    encode_init_producer_id_response_2/1,
    decode_init_producer_id_response_2/1,
    encode_init_producer_id_response_3/1,
    decode_init_producer_id_response_3/1,
    encode_init_producer_id_response_4/1,
    decode_init_producer_id_response_4/1,
    encode_init_producer_id_response_5/1,
    decode_init_producer_id_response_5/1
]).
-export_type([
    init_producer_id_response_0/0,
    init_producer_id_response_1/0,
    init_producer_id_response_2/0,
    init_producer_id_response_3/0,
    init_producer_id_response_4/0,
    init_producer_id_response_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_init_producer_id_response_0(init_producer_id_response_0()) -> iodata().

encode_init_producer_id_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The current producer id.
        producer_id := ProducerId,
        % The current epoch associated with the producer id.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch)
    ];
encode_init_producer_id_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_response_0(),
    Rest :: binary().

decode_init_producer_id_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    }.

-spec encode_init_producer_id_response_1(init_producer_id_response_1()) -> iodata().

encode_init_producer_id_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The current producer id.
        producer_id := ProducerId,
        % The current epoch associated with the producer id.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch)
    ];
encode_init_producer_id_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_response_1(),
    Rest :: binary().

decode_init_producer_id_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    }.

-spec encode_init_producer_id_response_2(init_producer_id_response_2()) -> iodata().

encode_init_producer_id_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The current producer id.
        producer_id := ProducerId,
        % The current epoch associated with the producer id.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_init_producer_id_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_response_2(),
    Rest :: binary().

decode_init_producer_id_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_init_producer_id_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    ).

-spec decode_init_producer_id_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_init_producer_id_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_init_producer_id_response_3(init_producer_id_response_3()) -> iodata().

encode_init_producer_id_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The current producer id.
        producer_id := ProducerId,
        % The current epoch associated with the producer id.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_init_producer_id_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_response_3(),
    Rest :: binary().

decode_init_producer_id_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_init_producer_id_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    ).

-spec decode_init_producer_id_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_init_producer_id_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_init_producer_id_response_4(init_producer_id_response_4()) -> iodata().

encode_init_producer_id_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The current producer id.
        producer_id := ProducerId,
        % The current epoch associated with the producer id.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_init_producer_id_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_response_4(),
    Rest :: binary().

decode_init_producer_id_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_init_producer_id_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    ).

-spec decode_init_producer_id_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_init_producer_id_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_init_producer_id_response_5(init_producer_id_response_5()) -> iodata().

encode_init_producer_id_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The current producer id.
        producer_id := ProducerId,
        % The current epoch associated with the producer id.
        producer_epoch := ProducerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_init_producer_id_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        producer_id => int64,
        producer_epoch => int16
    }).

-spec decode_init_producer_id_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: init_producer_id_response_5(),
    Rest :: binary().

decode_init_producer_id_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_init_producer_id_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch
        },
        Bin4
    ).

-spec decode_init_producer_id_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_init_producer_id_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type init_producer_id_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
-type init_producer_id_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
-type init_producer_id_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
-type init_producer_id_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
-type init_producer_id_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
-type init_producer_id_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    producer_id := integer(),
    producer_epoch := integer()
}.
