-module(delete_acls_response).
-export([
    encode_delete_acls_response_0/1,
    decode_delete_acls_response_0/1,
    encode_delete_acls_response_1/1,
    decode_delete_acls_response_1/1,
    encode_delete_acls_response_2/1,
    decode_delete_acls_response_2/1,
    encode_delete_acls_response_3/1,
    decode_delete_acls_response_3/1
]).
-export_type([
    delete_acls_response_0/0,
    delete_acls_matching_acl_0/0,
    delete_acls_filter_result_0/0,
    delete_acls_response_1/0,
    delete_acls_matching_acl_1/0,
    delete_acls_filter_result_1/0,
    delete_acls_response_2/0,
    delete_acls_matching_acl_2/0,
    delete_acls_filter_result_2/0,
    delete_acls_response_3/0,
    delete_acls_matching_acl_3/0,
    delete_acls_filter_result_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_delete_acls_response_0(delete_acls_response_0()) -> iodata().

encode_delete_acls_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each filter.
        filter_results := FilterResults
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(FilterResults)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(FilterResults, fun encode_delete_acls_filter_result_0/1)
    ];
encode_delete_acls_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        filter_results => {array, delete_acls_filter_result_0}
    }).

-spec decode_delete_acls_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_response_0(),
    Rest :: binary().

decode_delete_acls_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(FilterResults, Bin1, Bin2, ?_decode_element(decode_delete_acls_filter_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            filter_results => FilterResults
        },
        Bin2
    }.

-spec encode_delete_acls_matching_acl_0(delete_acls_matching_acl_0()) -> iodata().

encode_delete_acls_matching_acl_0(
    _Args = #{
        % The deletion error code, or 0 if the deletion succeeded.
        error_code := ErrorCode,
        % The deletion error message, or null if the deletion succeeded.
        error_message := ErrorMessage,
        % The ACL resource type.
        resource_type := ResourceType,
        % The ACL resource name.
        resource_name := ResourceName,
        % The ACL principal.
        principal := Principal,
        % The ACL host.
        host := Host,
        % The ACL operation.
        operation := Operation,
        % The ACL permission type.
        permission_type := PermissionType
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_string(Principal),
        ?encode_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_delete_acls_matching_acl_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_delete_acls_matching_acl_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_matching_acl_0(),
    Rest :: binary().

decode_delete_acls_matching_acl_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_string(ResourceName, Bin3, Bin4),
    ?_decode_string(Principal, Bin4, Bin5),
    ?_decode_string(Host, Bin5, Bin6),
    ?_decode_int8(Operation, Bin6, Bin7),
    ?_decode_int8(PermissionType, Bin7, Bin8),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin8
    }.

-spec encode_delete_acls_filter_result_0(delete_acls_filter_result_0()) -> iodata().

encode_delete_acls_filter_result_0(
    _Args = #{
        % The error code, or 0 if the filter succeeded.
        error_code := ErrorCode,
        % The error message, or null if the filter succeeded.
        error_message := ErrorMessage,
        % The ACLs which matched this filter.
        matching_acls := MatchingAcls
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(MatchingAcls)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_array(MatchingAcls, fun encode_delete_acls_matching_acl_0/1)
    ];
encode_delete_acls_filter_result_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        matching_acls => {array, delete_acls_matching_acl_0}
    }).

-spec decode_delete_acls_filter_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_filter_result_0(),
    Rest :: binary().

decode_delete_acls_filter_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_array(MatchingAcls, Bin2, Bin3, ?_decode_element(decode_delete_acls_matching_acl_0)),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            matching_acls => MatchingAcls
        },
        Bin3
    }.

-spec encode_delete_acls_response_1(delete_acls_response_1()) -> iodata().

encode_delete_acls_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each filter.
        filter_results := FilterResults
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(FilterResults)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(FilterResults, fun encode_delete_acls_filter_result_1/1)
    ];
encode_delete_acls_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        filter_results => {array, delete_acls_filter_result_1}
    }).

-spec decode_delete_acls_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_response_1(),
    Rest :: binary().

decode_delete_acls_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(FilterResults, Bin1, Bin2, ?_decode_element(decode_delete_acls_filter_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            filter_results => FilterResults
        },
        Bin2
    }.

-spec encode_delete_acls_matching_acl_1(delete_acls_matching_acl_1()) -> iodata().

encode_delete_acls_matching_acl_1(
    _Args = #{
        % The deletion error code, or 0 if the deletion succeeded.
        error_code := ErrorCode,
        % The deletion error message, or null if the deletion succeeded.
        error_message := ErrorMessage,
        % The ACL resource type.
        resource_type := ResourceType,
        % The ACL resource name.
        resource_name := ResourceName,
        % The ACL resource pattern type.
        pattern_type := PatternType,
        % The ACL principal.
        principal := Principal,
        % The ACL host.
        host := Host,
        % The ACL operation.
        operation := Operation,
        % The ACL permission type.
        permission_type := PermissionType
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(PatternType),
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_int8(PatternType),
        ?encode_string(Principal),
        ?encode_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_delete_acls_matching_acl_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        pattern_type => int8,
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_delete_acls_matching_acl_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_matching_acl_1(),
    Rest :: binary().

decode_delete_acls_matching_acl_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_string(ResourceName, Bin3, Bin4),
    ?_decode_int8(PatternType, Bin4, Bin5),
    ?_decode_string(Principal, Bin5, Bin6),
    ?_decode_string(Host, Bin6, Bin7),
    ?_decode_int8(Operation, Bin7, Bin8),
    ?_decode_int8(PermissionType, Bin8, Bin9),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            pattern_type => PatternType,
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin9
    }.

-spec encode_delete_acls_filter_result_1(delete_acls_filter_result_1()) -> iodata().

encode_delete_acls_filter_result_1(
    _Args = #{
        % The error code, or 0 if the filter succeeded.
        error_code := ErrorCode,
        % The error message, or null if the filter succeeded.
        error_message := ErrorMessage,
        % The ACLs which matched this filter.
        matching_acls := MatchingAcls
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(MatchingAcls)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_array(MatchingAcls, fun encode_delete_acls_matching_acl_1/1)
    ];
encode_delete_acls_filter_result_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        matching_acls => {array, delete_acls_matching_acl_1}
    }).

-spec decode_delete_acls_filter_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_filter_result_1(),
    Rest :: binary().

decode_delete_acls_filter_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_array(MatchingAcls, Bin2, Bin3, ?_decode_element(decode_delete_acls_matching_acl_1)),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            matching_acls => MatchingAcls
        },
        Bin3
    }.

-spec encode_delete_acls_response_2(delete_acls_response_2()) -> iodata().

encode_delete_acls_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each filter.
        filter_results := FilterResults
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(FilterResults)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(FilterResults, fun encode_delete_acls_filter_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        filter_results => {array, delete_acls_filter_result_2}
    }).

-spec decode_delete_acls_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_response_2(),
    Rest :: binary().

decode_delete_acls_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(FilterResults, Bin1, Bin2, ?_decode_element(decode_delete_acls_filter_result_2)),
    ?decode_tagged_fields(
        fun decode_delete_acls_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            filter_results => FilterResults
        },
        Bin2
    ).

-spec decode_delete_acls_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_acls_response_2().

decode_delete_acls_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_acls_matching_acl_2(delete_acls_matching_acl_2()) -> iodata().

encode_delete_acls_matching_acl_2(
    _Args = #{
        % The deletion error code, or 0 if the deletion succeeded.
        error_code := ErrorCode,
        % The deletion error message, or null if the deletion succeeded.
        error_message := ErrorMessage,
        % The ACL resource type.
        resource_type := ResourceType,
        % The ACL resource name.
        resource_name := ResourceName,
        % The ACL resource pattern type.
        pattern_type := PatternType,
        % The ACL principal.
        principal := Principal,
        % The ACL host.
        host := Host,
        % The ACL operation.
        operation := Operation,
        % The ACL permission type.
        permission_type := PermissionType
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(PatternType),
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_int8(PatternType),
        ?encode_compact_string(Principal),
        ?encode_compact_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_matching_acl_2(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        pattern_type => int8,
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_delete_acls_matching_acl_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_matching_acl_2(),
    Rest :: binary().

decode_delete_acls_matching_acl_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_compact_string(ResourceName, Bin3, Bin4),
    ?_decode_int8(PatternType, Bin4, Bin5),
    ?_decode_compact_string(Principal, Bin5, Bin6),
    ?_decode_compact_string(Host, Bin6, Bin7),
    ?_decode_int8(Operation, Bin7, Bin8),
    ?_decode_int8(PermissionType, Bin8, Bin9),
    ?decode_tagged_fields(
        fun decode_delete_acls_matching_acl_2_tagged_field/3,
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            pattern_type => PatternType,
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin9
    ).

-spec decode_delete_acls_matching_acl_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_acls_matching_acl_2().

decode_delete_acls_matching_acl_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_acls_filter_result_2(delete_acls_filter_result_2()) -> iodata().

encode_delete_acls_filter_result_2(
    _Args = #{
        % The error code, or 0 if the filter succeeded.
        error_code := ErrorCode,
        % The error message, or null if the filter succeeded.
        error_message := ErrorMessage,
        % The ACLs which matched this filter.
        matching_acls := MatchingAcls
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(MatchingAcls)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(MatchingAcls, fun encode_delete_acls_matching_acl_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_filter_result_2(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        matching_acls => {array, delete_acls_matching_acl_2}
    }).

-spec decode_delete_acls_filter_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_filter_result_2(),
    Rest :: binary().

decode_delete_acls_filter_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_compact_array(MatchingAcls, Bin2, Bin3, ?_decode_element(decode_delete_acls_matching_acl_2)),
    ?decode_tagged_fields(
        fun decode_delete_acls_filter_result_2_tagged_field/3,
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            matching_acls => MatchingAcls
        },
        Bin3
    ).

-spec decode_delete_acls_filter_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_acls_filter_result_2().

decode_delete_acls_filter_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_acls_response_3(delete_acls_response_3()) -> iodata().

encode_delete_acls_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each filter.
        filter_results := FilterResults
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(FilterResults)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(FilterResults, fun encode_delete_acls_filter_result_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        filter_results => {array, delete_acls_filter_result_3}
    }).

-spec decode_delete_acls_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_response_3(),
    Rest :: binary().

decode_delete_acls_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(FilterResults, Bin1, Bin2, ?_decode_element(decode_delete_acls_filter_result_3)),
    ?decode_tagged_fields(
        fun decode_delete_acls_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            filter_results => FilterResults
        },
        Bin2
    ).

-spec decode_delete_acls_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_acls_response_3().

decode_delete_acls_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_acls_matching_acl_3(delete_acls_matching_acl_3()) -> iodata().

encode_delete_acls_matching_acl_3(
    _Args = #{
        % The deletion error code, or 0 if the deletion succeeded.
        error_code := ErrorCode,
        % The deletion error message, or null if the deletion succeeded.
        error_message := ErrorMessage,
        % The ACL resource type.
        resource_type := ResourceType,
        % The ACL resource name.
        resource_name := ResourceName,
        % The ACL resource pattern type.
        pattern_type := PatternType,
        % The ACL principal.
        principal := Principal,
        % The ACL host.
        host := Host,
        % The ACL operation.
        operation := Operation,
        % The ACL permission type.
        permission_type := PermissionType
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(PatternType),
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_int8(PatternType),
        ?encode_compact_string(Principal),
        ?encode_compact_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_matching_acl_3(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        pattern_type => int8,
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_delete_acls_matching_acl_3(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_matching_acl_3(),
    Rest :: binary().

decode_delete_acls_matching_acl_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_compact_string(ResourceName, Bin3, Bin4),
    ?_decode_int8(PatternType, Bin4, Bin5),
    ?_decode_compact_string(Principal, Bin5, Bin6),
    ?_decode_compact_string(Host, Bin6, Bin7),
    ?_decode_int8(Operation, Bin7, Bin8),
    ?_decode_int8(PermissionType, Bin8, Bin9),
    ?decode_tagged_fields(
        fun decode_delete_acls_matching_acl_3_tagged_field/3,
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            pattern_type => PatternType,
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin9
    ).

-spec decode_delete_acls_matching_acl_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_acls_matching_acl_3().

decode_delete_acls_matching_acl_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_acls_filter_result_3(delete_acls_filter_result_3()) -> iodata().

encode_delete_acls_filter_result_3(
    _Args = #{
        % The error code, or 0 if the filter succeeded.
        error_code := ErrorCode,
        % The error message, or null if the filter succeeded.
        error_message := ErrorMessage,
        % The ACLs which matched this filter.
        matching_acls := MatchingAcls
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(MatchingAcls)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(MatchingAcls, fun encode_delete_acls_matching_acl_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_filter_result_3(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        matching_acls => {array, delete_acls_matching_acl_3}
    }).

-spec decode_delete_acls_filter_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_filter_result_3(),
    Rest :: binary().

decode_delete_acls_filter_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_compact_array(MatchingAcls, Bin2, Bin3, ?_decode_element(decode_delete_acls_matching_acl_3)),
    ?decode_tagged_fields(
        fun decode_delete_acls_filter_result_3_tagged_field/3,
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            matching_acls => MatchingAcls
        },
        Bin3
    ).

-spec decode_delete_acls_filter_result_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_acls_filter_result_3().

decode_delete_acls_filter_result_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type delete_acls_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    filter_results := list(delete_acls_filter_result_0())
}.
-type delete_acls_matching_acl_0() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type delete_acls_filter_result_0() :: #{
    error_code := integer(),
    error_message := binary() | null,
    matching_acls := list(delete_acls_matching_acl_0())
}.
-type delete_acls_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    filter_results := list(delete_acls_filter_result_1())
}.
-type delete_acls_matching_acl_1() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    pattern_type := integer(),
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type delete_acls_filter_result_1() :: #{
    error_code := integer(),
    error_message := binary() | null,
    matching_acls := list(delete_acls_matching_acl_1())
}.
-type delete_acls_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    filter_results := list(delete_acls_filter_result_2())
}.
-type delete_acls_matching_acl_2() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    pattern_type := integer(),
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type delete_acls_filter_result_2() :: #{
    error_code := integer(),
    error_message := binary() | null,
    matching_acls := list(delete_acls_matching_acl_2())
}.
-type delete_acls_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    filter_results := list(delete_acls_filter_result_3())
}.
-type delete_acls_matching_acl_3() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    pattern_type := integer(),
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type delete_acls_filter_result_3() :: #{
    error_code := integer(),
    error_message := binary() | null,
    matching_acls := list(delete_acls_matching_acl_3())
}.
