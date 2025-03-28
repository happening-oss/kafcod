-module(list_groups_response).
-export([
    encode_list_groups_response_0/1,
    decode_list_groups_response_0/1,
    encode_list_groups_response_1/1,
    decode_list_groups_response_1/1,
    encode_list_groups_response_2/1,
    decode_list_groups_response_2/1,
    encode_list_groups_response_3/1,
    decode_list_groups_response_3/1,
    encode_list_groups_response_4/1,
    decode_list_groups_response_4/1,
    encode_list_groups_response_5/1,
    decode_list_groups_response_5/1
]).
-export_type([
    list_groups_response_0/0,
    listed_group_0/0,
    list_groups_response_1/0,
    listed_group_1/0,
    list_groups_response_2/0,
    listed_group_2/0,
    list_groups_response_3/0,
    listed_group_3/0,
    list_groups_response_4/0,
    listed_group_4/0,
    list_groups_response_5/0,
    listed_group_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_groups_response_0(list_groups_response_0()) -> iodata().

encode_list_groups_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each group in the response.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Groups)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(Groups, fun encode_listed_group_0/1)
    ];
encode_list_groups_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        groups => {array, listed_group_0}
    }).

-spec decode_list_groups_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_response_0(),
    Rest :: binary().

decode_list_groups_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(Groups, Bin1, Bin2, ?_decode_element(decode_listed_group_0)),
    {
        Header#{
            error_code => ErrorCode,
            groups => Groups
        },
        Bin2
    }.

-spec encode_listed_group_0(listed_group_0()) -> iodata().

encode_listed_group_0(
    _Args = #{
        % The group ID.
        group_id := GroupId,
        % The group protocol type.
        protocol_type := ProtocolType
    }
) when
    ?is_string(GroupId),
    ?is_string(ProtocolType)
->
    [
        ?encode_string(GroupId),
        ?encode_string(ProtocolType)
    ];
encode_listed_group_0(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        protocol_type => string
    }).

-spec decode_listed_group_0(binary()) -> {Decoded, Rest} when
    Decoded :: listed_group_0(),
    Rest :: binary().

decode_listed_group_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_string(ProtocolType, Bin1, Bin2),
    {
        #{
            group_id => GroupId,
            protocol_type => ProtocolType
        },
        Bin2
    }.

-spec encode_list_groups_response_1(list_groups_response_1()) -> iodata().

encode_list_groups_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each group in the response.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Groups)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_array(Groups, fun encode_listed_group_1/1)
    ];
encode_list_groups_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        groups => {array, listed_group_1}
    }).

-spec decode_list_groups_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_response_1(),
    Rest :: binary().

decode_list_groups_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_array(Groups, Bin2, Bin3, ?_decode_element(decode_listed_group_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            groups => Groups
        },
        Bin3
    }.

-spec encode_listed_group_1(listed_group_1()) -> iodata().

encode_listed_group_1(
    _Args = #{
        % The group ID.
        group_id := GroupId,
        % The group protocol type.
        protocol_type := ProtocolType
    }
) when
    ?is_string(GroupId),
    ?is_string(ProtocolType)
->
    [
        ?encode_string(GroupId),
        ?encode_string(ProtocolType)
    ];
encode_listed_group_1(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        protocol_type => string
    }).

-spec decode_listed_group_1(binary()) -> {Decoded, Rest} when
    Decoded :: listed_group_1(),
    Rest :: binary().

decode_listed_group_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_string(ProtocolType, Bin1, Bin2),
    {
        #{
            group_id => GroupId,
            protocol_type => ProtocolType
        },
        Bin2
    }.

-spec encode_list_groups_response_2(list_groups_response_2()) -> iodata().

encode_list_groups_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each group in the response.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Groups)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_array(Groups, fun encode_listed_group_2/1)
    ];
encode_list_groups_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        groups => {array, listed_group_2}
    }).

-spec decode_list_groups_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_response_2(),
    Rest :: binary().

decode_list_groups_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_array(Groups, Bin2, Bin3, ?_decode_element(decode_listed_group_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            groups => Groups
        },
        Bin3
    }.

-spec encode_listed_group_2(listed_group_2()) -> iodata().

encode_listed_group_2(
    _Args = #{
        % The group ID.
        group_id := GroupId,
        % The group protocol type.
        protocol_type := ProtocolType
    }
) when
    ?is_string(GroupId),
    ?is_string(ProtocolType)
->
    [
        ?encode_string(GroupId),
        ?encode_string(ProtocolType)
    ];
encode_listed_group_2(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        protocol_type => string
    }).

-spec decode_listed_group_2(binary()) -> {Decoded, Rest} when
    Decoded :: listed_group_2(),
    Rest :: binary().

decode_listed_group_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_string(ProtocolType, Bin1, Bin2),
    {
        #{
            group_id => GroupId,
            protocol_type => ProtocolType
        },
        Bin2
    }.

-spec encode_list_groups_response_3(list_groups_response_3()) -> iodata().

encode_list_groups_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each group in the response.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Groups)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Groups, fun encode_listed_group_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_groups_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        groups => {array, listed_group_3}
    }).

-spec decode_list_groups_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_response_3(),
    Rest :: binary().

decode_list_groups_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Groups, Bin2, Bin3, ?_decode_element(decode_listed_group_3)),
    ?decode_tagged_fields(
        fun decode_list_groups_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            groups => Groups
        },
        Bin3
    ).

-spec decode_list_groups_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_groups_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_listed_group_3(listed_group_3()) -> iodata().

encode_listed_group_3(
    _Args = #{
        % The group ID.
        group_id := GroupId,
        % The group protocol type.
        protocol_type := ProtocolType
    }
) when
    ?is_string(GroupId),
    ?is_string(ProtocolType)
->
    [
        ?encode_compact_string(GroupId),
        ?encode_compact_string(ProtocolType),
        ?EMPTY_TAG_BUFFER
    ];
encode_listed_group_3(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        protocol_type => string
    }).

-spec decode_listed_group_3(binary()) -> {Decoded, Rest} when
    Decoded :: listed_group_3(),
    Rest :: binary().

decode_listed_group_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_string(ProtocolType, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_listed_group_3_tagged_field/3,
        #{
            group_id => GroupId,
            protocol_type => ProtocolType
        },
        Bin2
    ).

-spec decode_listed_group_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_listed_group_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_groups_response_4(list_groups_response_4()) -> iodata().

encode_list_groups_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each group in the response.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Groups)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Groups, fun encode_listed_group_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_groups_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        groups => {array, listed_group_4}
    }).

-spec decode_list_groups_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_response_4(),
    Rest :: binary().

decode_list_groups_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Groups, Bin2, Bin3, ?_decode_element(decode_listed_group_4)),
    ?decode_tagged_fields(
        fun decode_list_groups_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            groups => Groups
        },
        Bin3
    ).

-spec decode_list_groups_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_groups_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_listed_group_4(listed_group_4()) -> iodata().

encode_listed_group_4(
    _Args = #{
        % The group ID.
        group_id := GroupId,
        % The group protocol type.
        protocol_type := ProtocolType,
        % The group state name.
        group_state := GroupState
    }
) when
    ?is_string(GroupId),
    ?is_string(ProtocolType),
    ?is_string(GroupState)
->
    [
        ?encode_compact_string(GroupId),
        ?encode_compact_string(ProtocolType),
        ?encode_compact_string(GroupState),
        ?EMPTY_TAG_BUFFER
    ];
encode_listed_group_4(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        protocol_type => string,
        group_state => string
    }).

-spec decode_listed_group_4(binary()) -> {Decoded, Rest} when
    Decoded :: listed_group_4(),
    Rest :: binary().

decode_listed_group_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_string(ProtocolType, Bin1, Bin2),
    ?_decode_compact_string(GroupState, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_listed_group_4_tagged_field/3,
        #{
            group_id => GroupId,
            protocol_type => ProtocolType,
            group_state => GroupState
        },
        Bin3
    ).

-spec decode_listed_group_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_listed_group_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_groups_response_5(list_groups_response_5()) -> iodata().

encode_list_groups_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % Each group in the response.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Groups)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Groups, fun encode_listed_group_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_groups_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        groups => {array, listed_group_5}
    }).

-spec decode_list_groups_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_response_5(),
    Rest :: binary().

decode_list_groups_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Groups, Bin2, Bin3, ?_decode_element(decode_listed_group_5)),
    ?decode_tagged_fields(
        fun decode_list_groups_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            groups => Groups
        },
        Bin3
    ).

-spec decode_list_groups_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_groups_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_listed_group_5(listed_group_5()) -> iodata().

encode_listed_group_5(
    _Args = #{
        % The group ID.
        group_id := GroupId,
        % The group protocol type.
        protocol_type := ProtocolType,
        % The group state name.
        group_state := GroupState,
        % The group type name.
        group_type := GroupType
    }
) when
    ?is_string(GroupId),
    ?is_string(ProtocolType),
    ?is_string(GroupState),
    ?is_string(GroupType)
->
    [
        ?encode_compact_string(GroupId),
        ?encode_compact_string(ProtocolType),
        ?encode_compact_string(GroupState),
        ?encode_compact_string(GroupType),
        ?EMPTY_TAG_BUFFER
    ];
encode_listed_group_5(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        protocol_type => string,
        group_state => string,
        group_type => string
    }).

-spec decode_listed_group_5(binary()) -> {Decoded, Rest} when
    Decoded :: listed_group_5(),
    Rest :: binary().

decode_listed_group_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_string(ProtocolType, Bin1, Bin2),
    ?_decode_compact_string(GroupState, Bin2, Bin3),
    ?_decode_compact_string(GroupType, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_listed_group_5_tagged_field/3,
        #{
            group_id => GroupId,
            protocol_type => ProtocolType,
            group_state => GroupState,
            group_type => GroupType
        },
        Bin4
    ).

-spec decode_listed_group_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_listed_group_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_groups_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    groups := list(listed_group_0())
}.
-type listed_group_0() :: #{
    group_id := binary(),
    protocol_type := binary()
}.
-type list_groups_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    groups := list(listed_group_1())
}.
-type listed_group_1() :: #{
    group_id := binary(),
    protocol_type := binary()
}.
-type list_groups_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    groups := list(listed_group_2())
}.
-type listed_group_2() :: #{
    group_id := binary(),
    protocol_type := binary()
}.
-type list_groups_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    groups := list(listed_group_3())
}.
-type listed_group_3() :: #{
    group_id := binary(),
    protocol_type := binary()
}.
-type list_groups_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    groups := list(listed_group_4())
}.
-type listed_group_4() :: #{
    group_id := binary(),
    protocol_type := binary(),
    group_state := binary()
}.
-type list_groups_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    groups := list(listed_group_5())
}.
-type listed_group_5() :: #{
    group_id := binary(),
    protocol_type := binary(),
    group_state := binary(),
    group_type := binary()
}.
