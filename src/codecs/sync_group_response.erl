-module(sync_group_response).
-export([
    encode_sync_group_response_0/1,
    decode_sync_group_response_0/1,
    encode_sync_group_response_1/1,
    decode_sync_group_response_1/1,
    encode_sync_group_response_2/1,
    decode_sync_group_response_2/1,
    encode_sync_group_response_3/1,
    decode_sync_group_response_3/1,
    encode_sync_group_response_4/1,
    decode_sync_group_response_4/1,
    encode_sync_group_response_5/1,
    decode_sync_group_response_5/1
]).
-export_type([
    sync_group_response_0/0,
    sync_group_response_1/0,
    sync_group_response_2/0,
    sync_group_response_3/0,
    sync_group_response_4/0,
    sync_group_response_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_sync_group_response_0(sync_group_response_0()) -> iodata().

encode_sync_group_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_bytes(Assignment)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_bytes(Assignment)
    ];
encode_sync_group_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        assignment => bytes
    }).

-spec decode_sync_group_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_response_0(),
    Rest :: binary().

decode_sync_group_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_bytes(Assignment, Bin1, Bin2),
    {
        Header#{
            error_code => ErrorCode,
            assignment => Assignment
        },
        Bin2
    }.

-spec encode_sync_group_response_1(sync_group_response_1()) -> iodata().

encode_sync_group_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_bytes(Assignment)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_bytes(Assignment)
    ];
encode_sync_group_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        assignment => bytes
    }).

-spec decode_sync_group_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_response_1(),
    Rest :: binary().

decode_sync_group_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_bytes(Assignment, Bin2, Bin3),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            assignment => Assignment
        },
        Bin3
    }.

-spec encode_sync_group_response_2(sync_group_response_2()) -> iodata().

encode_sync_group_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_bytes(Assignment)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_bytes(Assignment)
    ];
encode_sync_group_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        assignment => bytes
    }).

-spec decode_sync_group_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_response_2(),
    Rest :: binary().

decode_sync_group_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_bytes(Assignment, Bin2, Bin3),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            assignment => Assignment
        },
        Bin3
    }.

-spec encode_sync_group_response_3(sync_group_response_3()) -> iodata().

encode_sync_group_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_bytes(Assignment)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_bytes(Assignment)
    ];
encode_sync_group_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        assignment => bytes
    }).

-spec decode_sync_group_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_response_3(),
    Rest :: binary().

decode_sync_group_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_bytes(Assignment, Bin2, Bin3),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            assignment => Assignment
        },
        Bin3
    }.

-spec encode_sync_group_response_4(sync_group_response_4()) -> iodata().

encode_sync_group_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_bytes(Assignment)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_bytes(Assignment),
        ?EMPTY_TAG_BUFFER
    ];
encode_sync_group_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        assignment => bytes
    }).

-spec decode_sync_group_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_response_4(),
    Rest :: binary().

decode_sync_group_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_bytes(Assignment, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_sync_group_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            assignment => Assignment
        },
        Bin3
    ).

-spec decode_sync_group_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_sync_group_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_sync_group_response_5(sync_group_response_5()) -> iodata().

encode_sync_group_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The group protocol type.
        protocol_type := ProtocolType,
        % The group protocol name.
        protocol_name := ProtocolName,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ProtocolType),
    ?is_nullable_string(ProtocolName),
    ?is_bytes(Assignment)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ProtocolType),
        ?encode_compact_nullable_string(ProtocolName),
        ?encode_compact_bytes(Assignment),
        ?EMPTY_TAG_BUFFER
    ];
encode_sync_group_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        protocol_type => nullable_string,
        protocol_name => nullable_string,
        assignment => bytes
    }).

-spec decode_sync_group_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_response_5(),
    Rest :: binary().

decode_sync_group_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ProtocolType, Bin2, Bin3),
    ?_decode_compact_nullable_string(ProtocolName, Bin3, Bin4),
    ?_decode_compact_bytes(Assignment, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_sync_group_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            protocol_type => ProtocolType,
            protocol_name => ProtocolName,
            assignment => Assignment
        },
        Bin5
    ).

-spec decode_sync_group_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_sync_group_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type sync_group_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    assignment := kafcod:bytes()
}.
-type sync_group_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    assignment := kafcod:bytes()
}.
-type sync_group_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    assignment := kafcod:bytes()
}.
-type sync_group_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    assignment := kafcod:bytes()
}.
-type sync_group_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    assignment := kafcod:bytes()
}.
-type sync_group_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    protocol_type := binary() | null,
    protocol_name := binary() | null,
    assignment := kafcod:bytes()
}.
