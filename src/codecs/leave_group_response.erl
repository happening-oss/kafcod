-module(leave_group_response).
-export([
    encode_leave_group_response_0/1,
    decode_leave_group_response_0/1,
    encode_leave_group_response_1/1,
    decode_leave_group_response_1/1,
    encode_leave_group_response_2/1,
    decode_leave_group_response_2/1,
    encode_leave_group_response_3/1,
    decode_leave_group_response_3/1,
    encode_leave_group_response_4/1,
    decode_leave_group_response_4/1,
    encode_leave_group_response_5/1,
    decode_leave_group_response_5/1
]).
-export_type([
    leave_group_response_0/0,
    leave_group_response_1/0,
    leave_group_response_2/0,
    leave_group_response_3/0,
    member_response_3/0,
    leave_group_response_4/0,
    member_response_4/0,
    leave_group_response_5/0,
    member_response_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_leave_group_response_0(leave_group_response_0()) -> iodata().

encode_leave_group_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode)
    ];
encode_leave_group_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_leave_group_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_response_0(),
    Rest :: binary().

decode_leave_group_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    {
        Header#{
            error_code => ErrorCode
        },
        Bin1
    }.

-spec encode_leave_group_response_1(leave_group_response_1()) -> iodata().

encode_leave_group_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode)
    ];
encode_leave_group_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16
    }).

-spec decode_leave_group_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_response_1(),
    Rest :: binary().

decode_leave_group_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_leave_group_response_2(leave_group_response_2()) -> iodata().

encode_leave_group_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode)
    ];
encode_leave_group_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16
    }).

-spec decode_leave_group_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_response_2(),
    Rest :: binary().

decode_leave_group_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_leave_group_response_3(leave_group_response_3()) -> iodata().

encode_leave_group_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % List of leaving member responses.
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Members)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_array(Members, fun encode_member_response_3/1)
    ];
encode_leave_group_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        members => {array, member_response_3}
    }).

-spec decode_leave_group_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_response_3(),
    Rest :: binary().

decode_leave_group_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_array(Members, Bin2, Bin3, ?_decode_element(decode_member_response_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            members => Members
        },
        Bin3
    }.

-spec encode_member_response_3(member_response_3()) -> iodata().

encode_member_response_3(
    _Args = #{
        % The member ID to remove from the group.
        member_id := MemberId,
        % The group instance ID to remove from the group.
        group_instance_id := GroupInstanceId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(MemberId),
        ?encode_nullable_string(GroupInstanceId),
        ?encode_int16(ErrorCode)
    ];
encode_member_response_3(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        error_code => int16
    }).

-spec decode_member_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: member_response_3(),
    Rest :: binary().

decode_member_response_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    {
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            error_code => ErrorCode
        },
        Bin3
    }.

-spec encode_leave_group_response_4(leave_group_response_4()) -> iodata().

encode_leave_group_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % List of leaving member responses.
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Members)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Members, fun encode_member_response_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leave_group_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        members => {array, member_response_4}
    }).

-spec decode_leave_group_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_response_4(),
    Rest :: binary().

decode_leave_group_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Members, Bin2, Bin3, ?_decode_element(decode_member_response_4)),
    ?decode_tagged_fields(
        fun decode_leave_group_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            members => Members
        },
        Bin3
    ).

-spec decode_leave_group_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_leave_group_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_member_response_4(member_response_4()) -> iodata().

encode_member_response_4(
    _Args = #{
        % The member ID to remove from the group.
        member_id := MemberId,
        % The group instance ID to remove from the group.
        group_instance_id := GroupInstanceId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_member_response_4(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        error_code => int16
    }).

-spec decode_member_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: member_response_4(),
    Rest :: binary().

decode_member_response_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_member_response_4_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            error_code => ErrorCode
        },
        Bin3
    ).

-spec decode_member_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_member_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leave_group_response_5(leave_group_response_5()) -> iodata().

encode_leave_group_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % List of leaving member responses.
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Members)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Members, fun encode_member_response_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leave_group_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        members => {array, member_response_5}
    }).

-spec decode_leave_group_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_response_5(),
    Rest :: binary().

decode_leave_group_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Members, Bin2, Bin3, ?_decode_element(decode_member_response_5)),
    ?decode_tagged_fields(
        fun decode_leave_group_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            members => Members
        },
        Bin3
    ).

-spec decode_leave_group_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_leave_group_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_member_response_5(member_response_5()) -> iodata().

encode_member_response_5(
    _Args = #{
        % The member ID to remove from the group.
        member_id := MemberId,
        % The group instance ID to remove from the group.
        group_instance_id := GroupInstanceId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_member_response_5(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        error_code => int16
    }).

-spec decode_member_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: member_response_5(),
    Rest :: binary().

decode_member_response_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_member_response_5_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            error_code => ErrorCode
        },
        Bin3
    ).

-spec decode_member_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_member_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type leave_group_response_0() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type leave_group_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer()
}.
-type leave_group_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer()
}.
-type leave_group_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    members := list(member_response_3())
}.
-type member_response_3() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    error_code := integer()
}.
-type leave_group_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    members := list(member_response_4())
}.
-type member_response_4() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    error_code := integer()
}.
-type leave_group_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    members := list(member_response_5())
}.
-type member_response_5() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    error_code := integer()
}.
