-module(create_acls_request).
-export([
    encode_create_acls_request_0/1,
    decode_create_acls_request_0/1,
    encode_create_acls_request_1/1,
    decode_create_acls_request_1/1,
    encode_create_acls_request_2/1,
    decode_create_acls_request_2/1,
    encode_create_acls_request_3/1,
    decode_create_acls_request_3/1
]).
-export_type([
    create_acls_request_0/0,
    acl_creation_0/0,
    create_acls_request_1/0,
    acl_creation_1/0,
    create_acls_request_2/0,
    acl_creation_2/0,
    create_acls_request_3/0,
    acl_creation_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(CREATE_ACLS_REQUEST, 30).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_create_acls_request_0(create_acls_request_0()) -> iodata().

encode_create_acls_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ACLs that we want to create.
        creations := Creations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Creations)
->
    [
        ?encode_request_header_1(?CREATE_ACLS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Creations, fun encode_acl_creation_0/1)
    ];
encode_create_acls_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        creations => {array, acl_creation_0}
    }).

-spec decode_create_acls_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_acls_request_0(),
    Rest :: binary().

decode_create_acls_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Creations, Bin0, Bin1, ?_decode_element(decode_acl_creation_0)),
    {
        Header#{
            creations => Creations
        },
        Bin1
    }.

-spec encode_acl_creation_0(acl_creation_0()) -> iodata().

encode_acl_creation_0(
    _Args = #{
        % The type of the resource.
        resource_type := ResourceType,
        % The resource name for the ACL.
        resource_name := ResourceName,
        % The principal for the ACL.
        principal := Principal,
        % The host for the ACL.
        host := Host,
        % The operation type for the ACL (read, write, etc.).
        operation := Operation,
        % The permission type for the ACL (allow, deny, etc.).
        permission_type := PermissionType
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_string(Principal),
        ?encode_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_acl_creation_0(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_acl_creation_0(binary()) -> {Decoded, Rest} when
    Decoded :: acl_creation_0(),
    Rest :: binary().

decode_acl_creation_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_string(Principal, Bin2, Bin3),
    ?_decode_string(Host, Bin3, Bin4),
    ?_decode_int8(Operation, Bin4, Bin5),
    ?_decode_int8(PermissionType, Bin5, Bin6),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin6
    }.

-spec encode_create_acls_request_1(create_acls_request_1()) -> iodata().

encode_create_acls_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ACLs that we want to create.
        creations := Creations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Creations)
->
    [
        ?encode_request_header_1(?CREATE_ACLS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Creations, fun encode_acl_creation_1/1)
    ];
encode_create_acls_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        creations => {array, acl_creation_1}
    }).

-spec decode_create_acls_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_acls_request_1(),
    Rest :: binary().

decode_create_acls_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Creations, Bin0, Bin1, ?_decode_element(decode_acl_creation_1)),
    {
        Header#{
            creations => Creations
        },
        Bin1
    }.

-spec encode_acl_creation_1(acl_creation_1()) -> iodata().

encode_acl_creation_1(
    _Args = #{
        % The type of the resource.
        resource_type := ResourceType,
        % The resource name for the ACL.
        resource_name := ResourceName,
        % The pattern type for the ACL.
        resource_pattern_type := ResourcePatternType,
        % The principal for the ACL.
        principal := Principal,
        % The host for the ACL.
        host := Host,
        % The operation type for the ACL (read, write, etc.).
        operation := Operation,
        % The permission type for the ACL (allow, deny, etc.).
        permission_type := PermissionType
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(ResourcePatternType),
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_int8(ResourcePatternType),
        ?encode_string(Principal),
        ?encode_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_acl_creation_1(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        resource_pattern_type => int8,
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_acl_creation_1(binary()) -> {Decoded, Rest} when
    Decoded :: acl_creation_1(),
    Rest :: binary().

decode_acl_creation_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_int8(ResourcePatternType, Bin2, Bin3),
    ?_decode_string(Principal, Bin3, Bin4),
    ?_decode_string(Host, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            resource_pattern_type => ResourcePatternType,
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin7
    }.

-spec encode_create_acls_request_2(create_acls_request_2()) -> iodata().

encode_create_acls_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ACLs that we want to create.
        creations := Creations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Creations)
->
    [
        ?encode_request_header_2(?CREATE_ACLS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_array(Creations, fun encode_acl_creation_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_acls_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        creations => {array, acl_creation_2}
    }).

-spec decode_create_acls_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_acls_request_2(),
    Rest :: binary().

decode_create_acls_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Creations, Bin0, Bin1, ?_decode_element(decode_acl_creation_2)),
    ?decode_tagged_fields(
        fun decode_create_acls_request_2_tagged_field/3,
        Header#{
            creations => Creations
        },
        Bin1
    ).

-spec decode_create_acls_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_acls_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_acl_creation_2(acl_creation_2()) -> iodata().

encode_acl_creation_2(
    _Args = #{
        % The type of the resource.
        resource_type := ResourceType,
        % The resource name for the ACL.
        resource_name := ResourceName,
        % The pattern type for the ACL.
        resource_pattern_type := ResourcePatternType,
        % The principal for the ACL.
        principal := Principal,
        % The host for the ACL.
        host := Host,
        % The operation type for the ACL (read, write, etc.).
        operation := Operation,
        % The permission type for the ACL (allow, deny, etc.).
        permission_type := PermissionType
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(ResourcePatternType),
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_int8(ResourcePatternType),
        ?encode_compact_string(Principal),
        ?encode_compact_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_acl_creation_2(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        resource_pattern_type => int8,
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_acl_creation_2(binary()) -> {Decoded, Rest} when
    Decoded :: acl_creation_2(),
    Rest :: binary().

decode_acl_creation_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_compact_string(ResourceName, Bin1, Bin2),
    ?_decode_int8(ResourcePatternType, Bin2, Bin3),
    ?_decode_compact_string(Principal, Bin3, Bin4),
    ?_decode_compact_string(Host, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_acl_creation_2_tagged_field/3,
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            resource_pattern_type => ResourcePatternType,
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin7
    ).

-spec decode_acl_creation_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_acl_creation_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_acls_request_3(create_acls_request_3()) -> iodata().

encode_create_acls_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ACLs that we want to create.
        creations := Creations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Creations)
->
    [
        ?encode_request_header_2(?CREATE_ACLS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_array(Creations, fun encode_acl_creation_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_acls_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        creations => {array, acl_creation_3}
    }).

-spec decode_create_acls_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_acls_request_3(),
    Rest :: binary().

decode_create_acls_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Creations, Bin0, Bin1, ?_decode_element(decode_acl_creation_3)),
    ?decode_tagged_fields(
        fun decode_create_acls_request_3_tagged_field/3,
        Header#{
            creations => Creations
        },
        Bin1
    ).

-spec decode_create_acls_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_acls_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_acl_creation_3(acl_creation_3()) -> iodata().

encode_acl_creation_3(
    _Args = #{
        % The type of the resource.
        resource_type := ResourceType,
        % The resource name for the ACL.
        resource_name := ResourceName,
        % The pattern type for the ACL.
        resource_pattern_type := ResourcePatternType,
        % The principal for the ACL.
        principal := Principal,
        % The host for the ACL.
        host := Host,
        % The operation type for the ACL (read, write, etc.).
        operation := Operation,
        % The permission type for the ACL (allow, deny, etc.).
        permission_type := PermissionType
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_int8(ResourcePatternType),
    ?is_string(Principal),
    ?is_string(Host),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_int8(ResourcePatternType),
        ?encode_compact_string(Principal),
        ?encode_compact_string(Host),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_acl_creation_3(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        resource_pattern_type => int8,
        principal => string,
        host => string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_acl_creation_3(binary()) -> {Decoded, Rest} when
    Decoded :: acl_creation_3(),
    Rest :: binary().

decode_acl_creation_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_compact_string(ResourceName, Bin1, Bin2),
    ?_decode_int8(ResourcePatternType, Bin2, Bin3),
    ?_decode_compact_string(Principal, Bin3, Bin4),
    ?_decode_compact_string(Host, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_acl_creation_3_tagged_field/3,
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            resource_pattern_type => ResourcePatternType,
            principal => Principal,
            host => Host,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin7
    ).

-spec decode_acl_creation_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_acl_creation_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type create_acls_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    creations := list(acl_creation_0())
}.
-type acl_creation_0() :: #{
    resource_type := integer(),
    resource_name := binary(),
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type create_acls_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    creations := list(acl_creation_1())
}.
-type acl_creation_1() :: #{
    resource_type := integer(),
    resource_name := binary(),
    resource_pattern_type := integer(),
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type create_acls_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    creations := list(acl_creation_2())
}.
-type acl_creation_2() :: #{
    resource_type := integer(),
    resource_name := binary(),
    resource_pattern_type := integer(),
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
-type create_acls_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    creations := list(acl_creation_3())
}.
-type acl_creation_3() :: #{
    resource_type := integer(),
    resource_name := binary(),
    resource_pattern_type := integer(),
    principal := binary(),
    host := binary(),
    operation := integer(),
    permission_type := integer()
}.
