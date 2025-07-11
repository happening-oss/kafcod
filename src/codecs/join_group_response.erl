-module(join_group_response).
-export([
    encode_join_group_response_0/1,
    decode_join_group_response_0/1,
    encode_join_group_response_1/1,
    decode_join_group_response_1/1,
    encode_join_group_response_2/1,
    decode_join_group_response_2/1,
    encode_join_group_response_3/1,
    decode_join_group_response_3/1,
    encode_join_group_response_4/1,
    decode_join_group_response_4/1,
    encode_join_group_response_5/1,
    decode_join_group_response_5/1,
    encode_join_group_response_6/1,
    decode_join_group_response_6/1,
    encode_join_group_response_7/1,
    decode_join_group_response_7/1,
    encode_join_group_response_8/1,
    decode_join_group_response_8/1,
    encode_join_group_response_9/1,
    decode_join_group_response_9/1
]).
-export_type([
    join_group_response_0/0,
    join_group_response_member_0/0,
    join_group_response_1/0,
    join_group_response_member_1/0,
    join_group_response_2/0,
    join_group_response_member_2/0,
    join_group_response_3/0,
    join_group_response_member_3/0,
    join_group_response_4/0,
    join_group_response_member_4/0,
    join_group_response_5/0,
    join_group_response_member_5/0,
    join_group_response_6/0,
    join_group_response_member_6/0,
    join_group_response_7/0,
    join_group_response_member_7/0,
    join_group_response_8/0,
    join_group_response_member_8/0,
    join_group_response_9/0,
    join_group_response_member_9/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_join_group_response_0(join_group_response_0()) -> iodata().

encode_join_group_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_string(ProtocolName),
        ?encode_string(Leader),
        ?encode_string(MemberId),
        ?encode_array(Members, fun encode_join_group_response_member_0/1)
    ];
encode_join_group_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        generation_id => int32,
        protocol_name => string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_0}
    }).

-spec decode_join_group_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_0(),
    Rest :: binary().

decode_join_group_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(ProtocolName, Bin2, Bin3),
    ?_decode_string(Leader, Bin3, Bin4),
    ?_decode_string(MemberId, Bin4, Bin5),
    ?_decode_array(Members, Bin5, Bin6, ?_decode_element(decode_join_group_response_member_0)),
    {
        Header#{
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin6
    }.

-spec encode_join_group_response_member_0(join_group_response_member_0()) -> iodata().

encode_join_group_response_member_0(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Metadata)
    ];
encode_join_group_response_member_0(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_0(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_0(),
    Rest :: binary().

decode_join_group_response_member_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_response_1(join_group_response_1()) -> iodata().

encode_join_group_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_string(ProtocolName),
        ?encode_string(Leader),
        ?encode_string(MemberId),
        ?encode_array(Members, fun encode_join_group_response_member_1/1)
    ];
encode_join_group_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        generation_id => int32,
        protocol_name => string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_1}
    }).

-spec decode_join_group_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_1(),
    Rest :: binary().

decode_join_group_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(ProtocolName, Bin2, Bin3),
    ?_decode_string(Leader, Bin3, Bin4),
    ?_decode_string(MemberId, Bin4, Bin5),
    ?_decode_array(Members, Bin5, Bin6, ?_decode_element(decode_join_group_response_member_1)),
    {
        Header#{
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin6
    }.

-spec encode_join_group_response_member_1(join_group_response_member_1()) -> iodata().

encode_join_group_response_member_1(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Metadata)
    ];
encode_join_group_response_member_1(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_1(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_1(),
    Rest :: binary().

decode_join_group_response_member_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_response_2(join_group_response_2()) -> iodata().

encode_join_group_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_string(ProtocolName),
        ?encode_string(Leader),
        ?encode_string(MemberId),
        ?encode_array(Members, fun encode_join_group_response_member_2/1)
    ];
encode_join_group_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        generation_id => int32,
        protocol_name => string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_2}
    }).

-spec decode_join_group_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_2(),
    Rest :: binary().

decode_join_group_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(GenerationId, Bin2, Bin3),
    ?_decode_string(ProtocolName, Bin3, Bin4),
    ?_decode_string(Leader, Bin4, Bin5),
    ?_decode_string(MemberId, Bin5, Bin6),
    ?_decode_array(Members, Bin6, Bin7, ?_decode_element(decode_join_group_response_member_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin7
    }.

-spec encode_join_group_response_member_2(join_group_response_member_2()) -> iodata().

encode_join_group_response_member_2(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Metadata)
    ];
encode_join_group_response_member_2(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_2(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_2(),
    Rest :: binary().

decode_join_group_response_member_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_response_3(join_group_response_3()) -> iodata().

encode_join_group_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_string(ProtocolName),
        ?encode_string(Leader),
        ?encode_string(MemberId),
        ?encode_array(Members, fun encode_join_group_response_member_3/1)
    ];
encode_join_group_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        generation_id => int32,
        protocol_name => string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_3}
    }).

-spec decode_join_group_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_3(),
    Rest :: binary().

decode_join_group_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(GenerationId, Bin2, Bin3),
    ?_decode_string(ProtocolName, Bin3, Bin4),
    ?_decode_string(Leader, Bin4, Bin5),
    ?_decode_string(MemberId, Bin5, Bin6),
    ?_decode_array(Members, Bin6, Bin7, ?_decode_element(decode_join_group_response_member_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin7
    }.

-spec encode_join_group_response_member_3(join_group_response_member_3()) -> iodata().

encode_join_group_response_member_3(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Metadata)
    ];
encode_join_group_response_member_3(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_3(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_3(),
    Rest :: binary().

decode_join_group_response_member_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_response_4(join_group_response_4()) -> iodata().

encode_join_group_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_string(ProtocolName),
        ?encode_string(Leader),
        ?encode_string(MemberId),
        ?encode_array(Members, fun encode_join_group_response_member_4/1)
    ];
encode_join_group_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        generation_id => int32,
        protocol_name => string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_4}
    }).

-spec decode_join_group_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_4(),
    Rest :: binary().

decode_join_group_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(GenerationId, Bin2, Bin3),
    ?_decode_string(ProtocolName, Bin3, Bin4),
    ?_decode_string(Leader, Bin4, Bin5),
    ?_decode_string(MemberId, Bin5, Bin6),
    ?_decode_array(Members, Bin6, Bin7, ?_decode_element(decode_join_group_response_member_4)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin7
    }.

-spec encode_join_group_response_member_4(join_group_response_member_4()) -> iodata().

encode_join_group_response_member_4(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Metadata)
    ];
encode_join_group_response_member_4(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_4(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_4(),
    Rest :: binary().

decode_join_group_response_member_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_response_5(join_group_response_5()) -> iodata().

encode_join_group_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_string(ProtocolName),
        ?encode_string(Leader),
        ?encode_string(MemberId),
        ?encode_array(Members, fun encode_join_group_response_member_5/1)
    ];
encode_join_group_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        generation_id => int32,
        protocol_name => string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_5}
    }).

-spec decode_join_group_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_5(),
    Rest :: binary().

decode_join_group_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(GenerationId, Bin2, Bin3),
    ?_decode_string(ProtocolName, Bin3, Bin4),
    ?_decode_string(Leader, Bin4, Bin5),
    ?_decode_string(MemberId, Bin5, Bin6),
    ?_decode_array(Members, Bin6, Bin7, ?_decode_element(decode_join_group_response_member_5)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin7
    }.

-spec encode_join_group_response_member_5(join_group_response_member_5()) -> iodata().

encode_join_group_response_member_5(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(MemberId),
        ?encode_nullable_string(GroupInstanceId),
        ?encode_bytes(Metadata)
    ];
encode_join_group_response_member_5(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_5(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_5(),
    Rest :: binary().

decode_join_group_response_member_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_bytes(Metadata, Bin2, Bin3),
    {
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            metadata => Metadata
        },
        Bin3
    }.

-spec encode_join_group_response_6(join_group_response_6()) -> iodata().

encode_join_group_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_compact_string(ProtocolName),
        ?encode_compact_string(Leader),
        ?encode_compact_string(MemberId),
        ?encode_compact_array(Members, fun encode_join_group_response_member_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        generation_id => int32,
        protocol_name => string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_6}
    }).

-spec decode_join_group_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_6(),
    Rest :: binary().

decode_join_group_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(GenerationId, Bin2, Bin3),
    ?_decode_compact_string(ProtocolName, Bin3, Bin4),
    ?_decode_compact_string(Leader, Bin4, Bin5),
    ?_decode_compact_string(MemberId, Bin5, Bin6),
    ?_decode_compact_array(Members, Bin6, Bin7, ?_decode_element(decode_join_group_response_member_6)),
    ?decode_tagged_fields(
        fun decode_join_group_response_6_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin7
    ).

-spec decode_join_group_response_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_response_6().

decode_join_group_response_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_response_member_6(join_group_response_member_6()) -> iodata().

encode_join_group_response_member_6(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_bytes(Metadata)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_bytes(Metadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_response_member_6(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_6(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_6(),
    Rest :: binary().

decode_join_group_response_member_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_compact_bytes(Metadata, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_join_group_response_member_6_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            metadata => Metadata
        },
        Bin3
    ).

-spec decode_join_group_response_member_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_response_member_6().

decode_join_group_response_member_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_response_7(join_group_response_7()) -> iodata().

encode_join_group_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol name.
        protocol_type := ProtocolType,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_nullable_string(ProtocolType),
    ?is_nullable_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_compact_nullable_string(ProtocolType),
        ?encode_compact_nullable_string(ProtocolName),
        ?encode_compact_string(Leader),
        ?encode_compact_string(MemberId),
        ?encode_compact_array(Members, fun encode_join_group_response_member_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        generation_id => int32,
        protocol_type => nullable_string,
        protocol_name => nullable_string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_7}
    }).

-spec decode_join_group_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_7(),
    Rest :: binary().

decode_join_group_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(GenerationId, Bin2, Bin3),
    ?_decode_compact_nullable_string(ProtocolType, Bin3, Bin4),
    ?_decode_compact_nullable_string(ProtocolName, Bin4, Bin5),
    ?_decode_compact_string(Leader, Bin5, Bin6),
    ?_decode_compact_string(MemberId, Bin6, Bin7),
    ?_decode_compact_array(Members, Bin7, Bin8, ?_decode_element(decode_join_group_response_member_7)),
    ?decode_tagged_fields(
        fun decode_join_group_response_7_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_type => ProtocolType,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin8
    ).

-spec decode_join_group_response_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_response_7().

decode_join_group_response_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_response_member_7(join_group_response_member_7()) -> iodata().

encode_join_group_response_member_7(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_bytes(Metadata)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_bytes(Metadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_response_member_7(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_7(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_7(),
    Rest :: binary().

decode_join_group_response_member_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_compact_bytes(Metadata, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_join_group_response_member_7_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            metadata => Metadata
        },
        Bin3
    ).

-spec decode_join_group_response_member_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_response_member_7().

decode_join_group_response_member_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_response_8(join_group_response_8()) -> iodata().

encode_join_group_response_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol name.
        protocol_type := ProtocolType,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_nullable_string(ProtocolType),
    ?is_nullable_string(ProtocolName),
    ?is_string(Leader),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_compact_nullable_string(ProtocolType),
        ?encode_compact_nullable_string(ProtocolName),
        ?encode_compact_string(Leader),
        ?encode_compact_string(MemberId),
        ?encode_compact_array(Members, fun encode_join_group_response_member_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_response_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        generation_id => int32,
        protocol_type => nullable_string,
        protocol_name => nullable_string,
        leader => string,
        member_id => string,
        members => {array, join_group_response_member_8}
    }).

-spec decode_join_group_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_8(),
    Rest :: binary().

decode_join_group_response_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(GenerationId, Bin2, Bin3),
    ?_decode_compact_nullable_string(ProtocolType, Bin3, Bin4),
    ?_decode_compact_nullable_string(ProtocolName, Bin4, Bin5),
    ?_decode_compact_string(Leader, Bin5, Bin6),
    ?_decode_compact_string(MemberId, Bin6, Bin7),
    ?_decode_compact_array(Members, Bin7, Bin8, ?_decode_element(decode_join_group_response_member_8)),
    ?decode_tagged_fields(
        fun decode_join_group_response_8_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_type => ProtocolType,
            protocol_name => ProtocolName,
            leader => Leader,
            member_id => MemberId,
            members => Members
        },
        Bin8
    ).

-spec decode_join_group_response_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_response_8().

decode_join_group_response_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_response_member_8(join_group_response_member_8()) -> iodata().

encode_join_group_response_member_8(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_bytes(Metadata)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_bytes(Metadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_response_member_8(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_8(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_8(),
    Rest :: binary().

decode_join_group_response_member_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_compact_bytes(Metadata, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_join_group_response_member_8_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            metadata => Metadata
        },
        Bin3
    ).

-spec decode_join_group_response_member_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_response_member_8().

decode_join_group_response_member_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_response_9(join_group_response_9()) -> iodata().

encode_join_group_response_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The generation ID of the group.
        generation_id := GenerationId,
        % The group protocol name.
        protocol_type := ProtocolType,
        % The group protocol selected by the coordinator.
        protocol_name := ProtocolName,
        % The leader of the group.
        leader := Leader,
        % True if the leader must skip running the assignment.
        skip_assignment := SkipAssignment,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_int32(GenerationId),
    ?is_nullable_string(ProtocolType),
    ?is_nullable_string(ProtocolName),
    ?is_string(Leader),
    ?is_bool(SkipAssignment),
    ?is_string(MemberId),
    ?is_array(Members)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_int32(GenerationId),
        ?encode_compact_nullable_string(ProtocolType),
        ?encode_compact_nullable_string(ProtocolName),
        ?encode_compact_string(Leader),
        ?encode_bool(SkipAssignment),
        ?encode_compact_string(MemberId),
        ?encode_compact_array(Members, fun encode_join_group_response_member_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_response_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        generation_id => int32,
        protocol_type => nullable_string,
        protocol_name => nullable_string,
        leader => string,
        skip_assignment => bool,
        member_id => string,
        members => {array, join_group_response_member_9}
    }).

-spec decode_join_group_response_9(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_9(),
    Rest :: binary().

decode_join_group_response_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(GenerationId, Bin2, Bin3),
    ?_decode_compact_nullable_string(ProtocolType, Bin3, Bin4),
    ?_decode_compact_nullable_string(ProtocolName, Bin4, Bin5),
    ?_decode_compact_string(Leader, Bin5, Bin6),
    ?_decode_bool(SkipAssignment, Bin6, Bin7),
    ?_decode_compact_string(MemberId, Bin7, Bin8),
    ?_decode_compact_array(Members, Bin8, Bin9, ?_decode_element(decode_join_group_response_member_9)),
    ?decode_tagged_fields(
        fun decode_join_group_response_9_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            generation_id => GenerationId,
            protocol_type => ProtocolType,
            protocol_name => ProtocolName,
            leader => Leader,
            skip_assignment => SkipAssignment,
            member_id => MemberId,
            members => Members
        },
        Bin9
    ).

-spec decode_join_group_response_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_response_9().

decode_join_group_response_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_response_member_9(join_group_response_member_9()) -> iodata().

encode_join_group_response_member_9(
    _Args = #{
        % The group member ID.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The group member metadata.
        metadata := Metadata
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_bytes(Metadata)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_bytes(Metadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_response_member_9(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        metadata => bytes
    }).

-spec decode_join_group_response_member_9(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_response_member_9(),
    Rest :: binary().

decode_join_group_response_member_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_compact_bytes(Metadata, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_join_group_response_member_9_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            metadata => Metadata
        },
        Bin3
    ).

-spec decode_join_group_response_member_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_response_member_9().

decode_join_group_response_member_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type join_group_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_name := binary(),
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_0())
}.
-type join_group_response_member_0() :: #{
    member_id := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_name := binary(),
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_1())
}.
-type join_group_response_member_1() :: #{
    member_id := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_name := binary(),
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_2())
}.
-type join_group_response_member_2() :: #{
    member_id := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_name := binary(),
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_3())
}.
-type join_group_response_member_3() :: #{
    member_id := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_name := binary(),
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_4())
}.
-type join_group_response_member_4() :: #{
    member_id := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_name := binary(),
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_5())
}.
-type join_group_response_member_5() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    metadata := kafcod:bytes()
}.
-type join_group_response_6() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_name := binary(),
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_6())
}.
-type join_group_response_member_6() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    metadata := kafcod:bytes()
}.
-type join_group_response_7() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_type := binary() | null,
    protocol_name := binary() | null,
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_7())
}.
-type join_group_response_member_7() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    metadata := kafcod:bytes()
}.
-type join_group_response_8() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_type := binary() | null,
    protocol_name := binary() | null,
    leader := binary(),
    member_id := binary(),
    members := list(join_group_response_member_8())
}.
-type join_group_response_member_8() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    metadata := kafcod:bytes()
}.
-type join_group_response_9() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    generation_id := integer(),
    protocol_type := binary() | null,
    protocol_name := binary() | null,
    leader := binary(),
    skip_assignment := boolean(),
    member_id := binary(),
    members := list(join_group_response_member_9())
}.
-type join_group_response_member_9() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    metadata := kafcod:bytes()
}.
