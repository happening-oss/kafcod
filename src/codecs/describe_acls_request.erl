-module(describe_acls_request).
-export([
    encode_describe_acls_request_0/1,
    decode_describe_acls_request_0/1,
    encode_describe_acls_request_1/1,
    decode_describe_acls_request_1/1,
    encode_describe_acls_request_2/1,
    decode_describe_acls_request_2/1,
    encode_describe_acls_request_3/1,
    decode_describe_acls_request_3/1
]).
-export_type([
    describe_acls_request_0/0,
    describe_acls_request_1/0,
    describe_acls_request_2/0,
    describe_acls_request_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_ACLS_REQUEST, 29).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_acls_request_0(describe_acls_request_0()) -> iodata().

encode_describe_acls_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resource type.
        resource_type_filter := ResourceTypeFilter,
        % The resource name, or null to match any resource name.
        resource_name_filter := ResourceNameFilter,
        % The principal to match, or null to match any principal.
        principal_filter := PrincipalFilter,
        % The host to match, or null to match any host.
        host_filter := HostFilter,
        % The operation to match.
        operation := Operation,
        % The permission type to match.
        permission_type := PermissionType
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int8(ResourceTypeFilter),
    ?is_nullable_string(ResourceNameFilter),
    ?is_nullable_string(PrincipalFilter),
    ?is_nullable_string(HostFilter),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_request_header_1(?DESCRIBE_ACLS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int8(ResourceTypeFilter),
        ?encode_nullable_string(ResourceNameFilter),
        ?encode_nullable_string(PrincipalFilter),
        ?encode_nullable_string(HostFilter),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_describe_acls_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resource_type_filter => int8,
        resource_name_filter => nullable_string,
        principal_filter => nullable_string,
        host_filter => nullable_string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_describe_acls_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_request_0(),
    Rest :: binary().

decode_describe_acls_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int8(ResourceTypeFilter, Bin0, Bin1),
    ?_decode_nullable_string(ResourceNameFilter, Bin1, Bin2),
    ?_decode_nullable_string(PrincipalFilter, Bin2, Bin3),
    ?_decode_nullable_string(HostFilter, Bin3, Bin4),
    ?_decode_int8(Operation, Bin4, Bin5),
    ?_decode_int8(PermissionType, Bin5, Bin6),
    {
        Header#{
            resource_type_filter => ResourceTypeFilter,
            resource_name_filter => ResourceNameFilter,
            principal_filter => PrincipalFilter,
            host_filter => HostFilter,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin6
    }.

-spec encode_describe_acls_request_1(describe_acls_request_1()) -> iodata().

encode_describe_acls_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resource type.
        resource_type_filter := ResourceTypeFilter,
        % The resource name, or null to match any resource name.
        resource_name_filter := ResourceNameFilter,
        % The resource pattern to match.
        pattern_type_filter := PatternTypeFilter,
        % The principal to match, or null to match any principal.
        principal_filter := PrincipalFilter,
        % The host to match, or null to match any host.
        host_filter := HostFilter,
        % The operation to match.
        operation := Operation,
        % The permission type to match.
        permission_type := PermissionType
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int8(ResourceTypeFilter),
    ?is_nullable_string(ResourceNameFilter),
    ?is_int8(PatternTypeFilter),
    ?is_nullable_string(PrincipalFilter),
    ?is_nullable_string(HostFilter),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_request_header_1(?DESCRIBE_ACLS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int8(ResourceTypeFilter),
        ?encode_nullable_string(ResourceNameFilter),
        ?encode_int8(PatternTypeFilter),
        ?encode_nullable_string(PrincipalFilter),
        ?encode_nullable_string(HostFilter),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_describe_acls_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resource_type_filter => int8,
        resource_name_filter => nullable_string,
        pattern_type_filter => int8,
        principal_filter => nullable_string,
        host_filter => nullable_string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_describe_acls_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_request_1(),
    Rest :: binary().

decode_describe_acls_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int8(ResourceTypeFilter, Bin0, Bin1),
    ?_decode_nullable_string(ResourceNameFilter, Bin1, Bin2),
    ?_decode_int8(PatternTypeFilter, Bin2, Bin3),
    ?_decode_nullable_string(PrincipalFilter, Bin3, Bin4),
    ?_decode_nullable_string(HostFilter, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    {
        Header#{
            resource_type_filter => ResourceTypeFilter,
            resource_name_filter => ResourceNameFilter,
            pattern_type_filter => PatternTypeFilter,
            principal_filter => PrincipalFilter,
            host_filter => HostFilter,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin7
    }.

-spec encode_describe_acls_request_2(describe_acls_request_2()) -> iodata().

encode_describe_acls_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resource type.
        resource_type_filter := ResourceTypeFilter,
        % The resource name, or null to match any resource name.
        resource_name_filter := ResourceNameFilter,
        % The resource pattern to match.
        pattern_type_filter := PatternTypeFilter,
        % The principal to match, or null to match any principal.
        principal_filter := PrincipalFilter,
        % The host to match, or null to match any host.
        host_filter := HostFilter,
        % The operation to match.
        operation := Operation,
        % The permission type to match.
        permission_type := PermissionType
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int8(ResourceTypeFilter),
    ?is_nullable_string(ResourceNameFilter),
    ?is_int8(PatternTypeFilter),
    ?is_nullable_string(PrincipalFilter),
    ?is_nullable_string(HostFilter),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_request_header_2(?DESCRIBE_ACLS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int8(ResourceTypeFilter),
        ?encode_compact_nullable_string(ResourceNameFilter),
        ?encode_int8(PatternTypeFilter),
        ?encode_compact_nullable_string(PrincipalFilter),
        ?encode_compact_nullable_string(HostFilter),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_acls_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resource_type_filter => int8,
        resource_name_filter => nullable_string,
        pattern_type_filter => int8,
        principal_filter => nullable_string,
        host_filter => nullable_string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_describe_acls_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_request_2(),
    Rest :: binary().

decode_describe_acls_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int8(ResourceTypeFilter, Bin0, Bin1),
    ?_decode_compact_nullable_string(ResourceNameFilter, Bin1, Bin2),
    ?_decode_int8(PatternTypeFilter, Bin2, Bin3),
    ?_decode_compact_nullable_string(PrincipalFilter, Bin3, Bin4),
    ?_decode_compact_nullable_string(HostFilter, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_describe_acls_request_2_tagged_field/3,
        Header#{
            resource_type_filter => ResourceTypeFilter,
            resource_name_filter => ResourceNameFilter,
            pattern_type_filter => PatternTypeFilter,
            principal_filter => PrincipalFilter,
            host_filter => HostFilter,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin7
    ).

-spec decode_describe_acls_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_acls_request_2().

decode_describe_acls_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_acls_request_3(describe_acls_request_3()) -> iodata().

encode_describe_acls_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resource type.
        resource_type_filter := ResourceTypeFilter,
        % The resource name, or null to match any resource name.
        resource_name_filter := ResourceNameFilter,
        % The resource pattern to match.
        pattern_type_filter := PatternTypeFilter,
        % The principal to match, or null to match any principal.
        principal_filter := PrincipalFilter,
        % The host to match, or null to match any host.
        host_filter := HostFilter,
        % The operation to match.
        operation := Operation,
        % The permission type to match.
        permission_type := PermissionType
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int8(ResourceTypeFilter),
    ?is_nullable_string(ResourceNameFilter),
    ?is_int8(PatternTypeFilter),
    ?is_nullable_string(PrincipalFilter),
    ?is_nullable_string(HostFilter),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_request_header_2(?DESCRIBE_ACLS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int8(ResourceTypeFilter),
        ?encode_compact_nullable_string(ResourceNameFilter),
        ?encode_int8(PatternTypeFilter),
        ?encode_compact_nullable_string(PrincipalFilter),
        ?encode_compact_nullable_string(HostFilter),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_acls_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resource_type_filter => int8,
        resource_name_filter => nullable_string,
        pattern_type_filter => int8,
        principal_filter => nullable_string,
        host_filter => nullable_string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_describe_acls_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_acls_request_3(),
    Rest :: binary().

decode_describe_acls_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int8(ResourceTypeFilter, Bin0, Bin1),
    ?_decode_compact_nullable_string(ResourceNameFilter, Bin1, Bin2),
    ?_decode_int8(PatternTypeFilter, Bin2, Bin3),
    ?_decode_compact_nullable_string(PrincipalFilter, Bin3, Bin4),
    ?_decode_compact_nullable_string(HostFilter, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_describe_acls_request_3_tagged_field/3,
        Header#{
            resource_type_filter => ResourceTypeFilter,
            resource_name_filter => ResourceNameFilter,
            pattern_type_filter => PatternTypeFilter,
            principal_filter => PrincipalFilter,
            host_filter => HostFilter,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin7
    ).

-spec decode_describe_acls_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_acls_request_3().

decode_describe_acls_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_acls_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resource_type_filter := integer(),
    resource_name_filter := binary() | null,
    principal_filter := binary() | null,
    host_filter := binary() | null,
    operation := integer(),
    permission_type := integer()
}.
-type describe_acls_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resource_type_filter := integer(),
    resource_name_filter := binary() | null,
    pattern_type_filter := integer(),
    principal_filter := binary() | null,
    host_filter := binary() | null,
    operation := integer(),
    permission_type := integer()
}.
-type describe_acls_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resource_type_filter := integer(),
    resource_name_filter := binary() | null,
    pattern_type_filter := integer(),
    principal_filter := binary() | null,
    host_filter := binary() | null,
    operation := integer(),
    permission_type := integer()
}.
-type describe_acls_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resource_type_filter := integer(),
    resource_name_filter := binary() | null,
    pattern_type_filter := integer(),
    principal_filter := binary() | null,
    host_filter := binary() | null,
    operation := integer(),
    permission_type := integer()
}.
