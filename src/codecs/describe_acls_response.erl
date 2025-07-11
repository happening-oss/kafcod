-module(describe_acls_response).
-export([
    encode_describe_acls_response_0/1,
    decode_describe_acls_response_0/1,
    encode_describe_acls_response_1/1,
    decode_describe_acls_response_1/1,
    encode_describe_acls_response_2/1,
    decode_describe_acls_response_2/1,
    encode_describe_acls_response_3/1,
    decode_describe_acls_response_3/1
]).
-export_type([
    describe_acls_response_0/0,
    acl_description_0/0,
    describe_acls_resource_0/0,
    describe_acls_response_1/0,
    acl_description_1/0,
    describe_acls_resource_1/0,
    describe_acls_response_2/0,
    acl_description_2/0,
    describe_acls_resource_2/0,
    describe_acls_response_3/0,
    acl_description_3/0,
    describe_acls_resource_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_acls_response_0(describe_acls_response_0()) -> iodata().

encode_describe_acls_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % Each Resource that is referenced in an ACL.
        resources := Resources
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(Resources)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_array(Resources, fun encode_describe_acls_resource_0/1)
    ];
encode_describe_acls_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        resources => {array, describe_acls_resource_0}
    }).

-spec decode_describe_acls_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_response_0(),
    Rest :: binary().

decode_describe_acls_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_array(Resources, Bin3, Bin4, ?_decode_element(decode_describe_acls_resource_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resources => Resources
        },
        Bin4
    }.

-spec encode_acl_description_0(acl_description_0()) -> iodata().

encode_acl_description_0(
    _Args = #{
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
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_string(Principal),
        ?encode_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_acl_description_0(Args) ->
    ?encoder_error(Args, #{
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_acl_description_0(binary()) -> {Decoded, Rest} when
    Decoded :: acl_description_0(),
    Rest :: binary().

decode_acl_description_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Principal, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int8(Operation, Bin2, Bin3),
    ?_decode_int8(PermissionType, Bin3, Bin4),
    {
        #{
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin4
    }.

-spec encode_describe_acls_resource_0(describe_acls_resource_0()) -> iodata().

encode_describe_acls_resource_0(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The ACLs.
        acls := Acls
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_array(Acls)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_array(Acls, fun encode_acl_description_0/1)
    ];
encode_describe_acls_resource_0(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        acls => {array, acl_description_0}
    }).

-spec decode_describe_acls_resource_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_resource_0(),
    Rest :: binary().

decode_describe_acls_resource_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_array(Acls, Bin2, Bin3, ?_decode_element(decode_acl_description_0)),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            acls => Acls
        },
        Bin3
    }.

-spec encode_describe_acls_response_1(describe_acls_response_1()) -> iodata().

encode_describe_acls_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % Each Resource that is referenced in an ACL.
        resources := Resources
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(Resources)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_array(Resources, fun encode_describe_acls_resource_1/1)
    ];
encode_describe_acls_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        resources => {array, describe_acls_resource_1}
    }).

-spec decode_describe_acls_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_response_1(),
    Rest :: binary().

decode_describe_acls_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_array(Resources, Bin3, Bin4, ?_decode_element(decode_describe_acls_resource_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resources => Resources
        },
        Bin4
    }.

-spec encode_acl_description_1(acl_description_1()) -> iodata().

encode_acl_description_1(
    _Args = #{
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
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_string(Principal),
        ?encode_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_acl_description_1(Args) ->
    ?encoder_error(Args, #{
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_acl_description_1(binary()) -> {Decoded, Rest} when
    Decoded :: acl_description_1(),
    Rest :: binary().

decode_acl_description_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Principal, Bin0, Bin1),
    ?_decode_string(Host, Bin1, Bin2),
    ?_decode_int8(Operation, Bin2, Bin3),
    ?_decode_int8(PermissionType, Bin3, Bin4),
    {
        #{
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin4
    }.

-spec encode_describe_acls_resource_1(describe_acls_resource_1()) -> iodata().

encode_describe_acls_resource_1(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The resource pattern type.
        pattern_type := PatternType,
        % The ACLs.
        acls := Acls
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(PatternType),
    ?is_array(Acls)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_int8(PatternType),
        ?encode_array(Acls, fun encode_acl_description_1/1)
    ];
encode_describe_acls_resource_1(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        pattern_type => int8,
        acls => {array, acl_description_1}
    }).

-spec decode_describe_acls_resource_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_resource_1(),
    Rest :: binary().

decode_describe_acls_resource_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_int8(PatternType, Bin2, Bin3),
    ?_decode_array(Acls, Bin3, Bin4, ?_decode_element(decode_acl_description_1)),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            pattern_type => PatternType,
            acls => Acls
        },
        Bin4
    }.

-spec encode_describe_acls_response_2(describe_acls_response_2()) -> iodata().

encode_describe_acls_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % Each Resource that is referenced in an ACL.
        resources := Resources
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(Resources)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(Resources, fun encode_describe_acls_resource_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_acls_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        resources => {array, describe_acls_resource_2}
    }).

-spec decode_describe_acls_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_response_2(),
    Rest :: binary().

decode_describe_acls_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(Resources, Bin3, Bin4, ?_decode_element(decode_describe_acls_resource_2)),
    ?decode_tagged_fields(
        fun decode_describe_acls_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resources => Resources
        },
        Bin4
    ).

-spec decode_describe_acls_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_acls_response_2().

decode_describe_acls_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_acl_description_2(acl_description_2()) -> iodata().

encode_acl_description_2(
    _Args = #{
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
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_compact_string(Principal),
        ?encode_compact_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_acl_description_2(Args) ->
    ?encoder_error(Args, #{
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_acl_description_2(binary()) -> {Decoded, Rest} when
    Decoded :: acl_description_2(),
    Rest :: binary().

decode_acl_description_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Principal, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int8(Operation, Bin2, Bin3),
    ?_decode_int8(PermissionType, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_acl_description_2_tagged_field/3,
        #{
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin4
    ).

-spec decode_acl_description_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: acl_description_2().

decode_acl_description_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_acls_resource_2(describe_acls_resource_2()) -> iodata().

encode_describe_acls_resource_2(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The resource pattern type.
        pattern_type := PatternType,
        % The ACLs.
        acls := Acls
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(PatternType),
    ?is_array(Acls)
->
    [
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_int8(PatternType),
        ?encode_compact_array(Acls, fun encode_acl_description_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_acls_resource_2(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        pattern_type => int8,
        acls => {array, acl_description_2}
    }).

-spec decode_describe_acls_resource_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_resource_2(),
    Rest :: binary().

decode_describe_acls_resource_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_compact_string(ResourceName, Bin1, Bin2),
    ?_decode_int8(PatternType, Bin2, Bin3),
    ?_decode_compact_array(Acls, Bin3, Bin4, ?_decode_element(decode_acl_description_2)),
    ?decode_tagged_fields(
        fun decode_describe_acls_resource_2_tagged_field/3,
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            pattern_type => PatternType,
            acls => Acls
        },
        Bin4
    ).

-spec decode_describe_acls_resource_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_acls_resource_2().

decode_describe_acls_resource_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_acls_response_3(describe_acls_response_3()) -> iodata().

encode_describe_acls_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % Each Resource that is referenced in an ACL.
        resources := Resources
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(Resources)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(Resources, fun encode_describe_acls_resource_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_acls_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        resources => {array, describe_acls_resource_3}
    }).

-spec decode_describe_acls_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_response_3(),
    Rest :: binary().

decode_describe_acls_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(Resources, Bin3, Bin4, ?_decode_element(decode_describe_acls_resource_3)),
    ?decode_tagged_fields(
        fun decode_describe_acls_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resources => Resources
        },
        Bin4
    ).

-spec decode_describe_acls_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_acls_response_3().

decode_describe_acls_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_acl_description_3(acl_description_3()) -> iodata().

encode_acl_description_3(
    _Args = #{
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
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_compact_string(Principal),
        ?encode_compact_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_acl_description_3(Args) ->
    ?encoder_error(Args, #{
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_acl_description_3(binary()) -> {Decoded, Rest} when
    Decoded :: acl_description_3(),
    Rest :: binary().

decode_acl_description_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Principal, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int8(Operation, Bin2, Bin3),
    ?_decode_int8(PermissionType, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_acl_description_3_tagged_field/3,
        #{
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin4
    ).

-spec decode_acl_description_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: acl_description_3().

decode_acl_description_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_acls_resource_3(describe_acls_resource_3()) -> iodata().

encode_describe_acls_resource_3(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The resource pattern type.
        pattern_type := PatternType,
        % The ACLs.
        acls := Acls
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(PatternType),
    ?is_array(Acls)
->
    [
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_int8(PatternType),
        ?encode_compact_array(Acls, fun encode_acl_description_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_acls_resource_3(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        pattern_type => int8,
        acls => {array, acl_description_3}
    }).

-spec decode_describe_acls_resource_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_resource_3(),
    Rest :: binary().

decode_describe_acls_resource_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_compact_string(ResourceName, Bin1, Bin2),
    ?_decode_int8(PatternType, Bin2, Bin3),
    ?_decode_compact_array(Acls, Bin3, Bin4, ?_decode_element(decode_acl_description_3)),
    ?decode_tagged_fields(
        fun decode_describe_acls_resource_3_tagged_field/3,
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            pattern_type => PatternType,
            acls => Acls
        },
        Bin4
    ).

-spec decode_describe_acls_resource_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_acls_resource_3().

decode_describe_acls_resource_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_acls_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    resources := list(describe_acls_resource_0())
}.
-type acl_description_0() :: #{
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type describe_acls_resource_0() :: #{
    resource_type := integer(),
    resource_name := binary(),
    acls := list(acl_description_0())
}.
-type describe_acls_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    resources := list(describe_acls_resource_1())
}.
-type acl_description_1() :: #{
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type describe_acls_resource_1() :: #{
    resource_type := integer(),
    resource_name := binary(),
    pattern_type := integer(),
    acls := list(acl_description_1())
}.
-type describe_acls_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    resources := list(describe_acls_resource_2())
}.
-type acl_description_2() :: #{
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type describe_acls_resource_2() :: #{
    resource_type := integer(),
    resource_name := binary(),
    pattern_type := integer(),
    acls := list(acl_description_2())
}.
-type describe_acls_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    resources := list(describe_acls_resource_3())
}.
-type acl_description_3() :: #{
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type describe_acls_resource_3() :: #{
    resource_type := integer(),
    resource_name := binary(),
    pattern_type := integer(),
    acls := list(acl_description_3())
}.
