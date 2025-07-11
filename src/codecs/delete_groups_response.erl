-module(delete_groups_response).
-export([
    encode_delete_groups_response_0/1,
    decode_delete_groups_response_0/1,
    encode_delete_groups_response_1/1,
    decode_delete_groups_response_1/1,
    encode_delete_groups_response_2/1,
    decode_delete_groups_response_2/1
]).
-export_type([
    delete_groups_response_0/0,
    deletable_group_result_0/0,
    delete_groups_response_1/0,
    deletable_group_result_1/0,
    delete_groups_response_2/0,
    deletable_group_result_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_delete_groups_response_0(delete_groups_response_0()) -> iodata().

encode_delete_groups_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The deletion results
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Results, fun encode_deletable_group_result_0/1)
    ];
encode_delete_groups_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, deletable_group_result_0}
    }).

-spec decode_delete_groups_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_groups_response_0(),
    Rest :: binary().

decode_delete_groups_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_deletable_group_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_deletable_group_result_0(deletable_group_result_0()) -> iodata().

encode_deletable_group_result_0(
    _Args = #{
        % The group id
        group_id := GroupId,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_string(GroupId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(GroupId),
        ?encode_int16(ErrorCode)
    ];
encode_deletable_group_result_0(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        error_code => int16
    }).

-spec decode_deletable_group_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_group_result_0(),
    Rest :: binary().

decode_deletable_group_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            group_id => GroupId,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_delete_groups_response_1(delete_groups_response_1()) -> iodata().

encode_delete_groups_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The deletion results
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Results, fun encode_deletable_group_result_1/1)
    ];
encode_delete_groups_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, deletable_group_result_1}
    }).

-spec decode_delete_groups_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_groups_response_1(),
    Rest :: binary().

decode_delete_groups_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_deletable_group_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_deletable_group_result_1(deletable_group_result_1()) -> iodata().

encode_deletable_group_result_1(
    _Args = #{
        % The group id
        group_id := GroupId,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_string(GroupId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(GroupId),
        ?encode_int16(ErrorCode)
    ];
encode_deletable_group_result_1(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        error_code => int16
    }).

-spec decode_deletable_group_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_group_result_1(),
    Rest :: binary().

decode_deletable_group_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            group_id => GroupId,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_delete_groups_response_2(delete_groups_response_2()) -> iodata().

encode_delete_groups_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The deletion results
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Results)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Results, fun encode_deletable_group_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_groups_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, deletable_group_result_2}
    }).

-spec decode_delete_groups_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_groups_response_2(),
    Rest :: binary().

decode_delete_groups_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Results, Bin1, Bin2, ?_decode_element(decode_deletable_group_result_2)),
    ?decode_tagged_fields(
        fun decode_delete_groups_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    ).

-spec decode_delete_groups_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_groups_response_2().

decode_delete_groups_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_deletable_group_result_2(deletable_group_result_2()) -> iodata().

encode_deletable_group_result_2(
    _Args = #{
        % The group id
        group_id := GroupId,
        % The deletion error, or 0 if the deletion succeeded.
        error_code := ErrorCode
    }
) when
    ?is_string(GroupId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_compact_string(GroupId),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_deletable_group_result_2(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        error_code => int16
    }).

-spec decode_deletable_group_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: deletable_group_result_2(),
    Rest :: binary().

decode_deletable_group_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_deletable_group_result_2_tagged_field/3,
        #{
            group_id => GroupId,
            error_code => ErrorCode
        },
        Bin2
    ).

-spec decode_deletable_group_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: deletable_group_result_2().

decode_deletable_group_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type delete_groups_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(deletable_group_result_0())
}.
-type deletable_group_result_0() :: #{
    group_id := binary(),
    error_code := integer()
}.
-type delete_groups_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(deletable_group_result_1())
}.
-type deletable_group_result_1() :: #{
    group_id := binary(),
    error_code := integer()
}.
-type delete_groups_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(deletable_group_result_2())
}.
-type deletable_group_result_2() :: #{
    group_id := binary(),
    error_code := integer()
}.
