-module(delete_acls_request).
-export([
    encode_delete_acls_request_0/1,
    decode_delete_acls_request_0/1,
    encode_delete_acls_request_1/1,
    decode_delete_acls_request_1/1,
    encode_delete_acls_request_2/1,
    decode_delete_acls_request_2/1,
    encode_delete_acls_request_3/1,
    decode_delete_acls_request_3/1
]).
-export_type([
    delete_acls_request_0/0,
    delete_acls_filter_0/0,
    delete_acls_request_1/0,
    delete_acls_filter_1/0,
    delete_acls_request_2/0,
    delete_acls_filter_2/0,
    delete_acls_request_3/0,
    delete_acls_filter_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DELETE_ACLS_REQUEST, 31).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_delete_acls_request_0(delete_acls_request_0()) -> iodata().

encode_delete_acls_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The filters to use when deleting ACLs.
        filters := Filters
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Filters)
->
    [
        ?encode_request_header_1(?DELETE_ACLS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Filters, fun encode_delete_acls_filter_0/1)
    ];
encode_delete_acls_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        filters => {array, delete_acls_filter_0}
    }).

-spec decode_delete_acls_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_request_0(),
    Rest :: binary().

decode_delete_acls_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Filters, Bin0, Bin1, ?_decode_element(decode_delete_acls_filter_0)),
    {
        Header#{
            filters => Filters
        },
        Bin1
    }.

-spec encode_delete_acls_filter_0(delete_acls_filter_0()) -> iodata().

encode_delete_acls_filter_0(
    _Args = #{
        % The resource type.
        resource_type_filter := ResourceTypeFilter,
        % The resource name.
        resource_name_filter := ResourceNameFilter,
        % The principal filter, or null to accept all principals.
        principal_filter := PrincipalFilter,
        % The host filter, or null to accept all hosts.
        host_filter := HostFilter,
        % The ACL operation.
        operation := Operation,
        % The permission type.
        permission_type := PermissionType
    }
) when
    ?is_int8(ResourceTypeFilter),
    ?is_nullable_string(ResourceNameFilter),
    ?is_nullable_string(PrincipalFilter),
    ?is_nullable_string(HostFilter),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int8(ResourceTypeFilter),
        ?encode_nullable_string(ResourceNameFilter),
        ?encode_nullable_string(PrincipalFilter),
        ?encode_nullable_string(HostFilter),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_delete_acls_filter_0(Args) ->
    ?encoder_error(Args, #{
        resource_type_filter => int8,
        resource_name_filter => nullable_string,
        principal_filter => nullable_string,
        host_filter => nullable_string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_delete_acls_filter_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_filter_0(),
    Rest :: binary().

decode_delete_acls_filter_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceTypeFilter, Bin0, Bin1),
    ?_decode_nullable_string(ResourceNameFilter, Bin1, Bin2),
    ?_decode_nullable_string(PrincipalFilter, Bin2, Bin3),
    ?_decode_nullable_string(HostFilter, Bin3, Bin4),
    ?_decode_int8(Operation, Bin4, Bin5),
    ?_decode_int8(PermissionType, Bin5, Bin6),
    {
        #{
            resource_type_filter => ResourceTypeFilter,
            resource_name_filter => ResourceNameFilter,
            principal_filter => PrincipalFilter,
            host_filter => HostFilter,
            operation => Operation,
            permission_type => PermissionType
        },
        Bin6
    }.

-spec encode_delete_acls_request_1(delete_acls_request_1()) -> iodata().

encode_delete_acls_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The filters to use when deleting ACLs.
        filters := Filters
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Filters)
->
    [
        ?encode_request_header_1(?DELETE_ACLS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Filters, fun encode_delete_acls_filter_1/1)
    ];
encode_delete_acls_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        filters => {array, delete_acls_filter_1}
    }).

-spec decode_delete_acls_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_request_1(),
    Rest :: binary().

decode_delete_acls_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Filters, Bin0, Bin1, ?_decode_element(decode_delete_acls_filter_1)),
    {
        Header#{
            filters => Filters
        },
        Bin1
    }.

-spec encode_delete_acls_filter_1(delete_acls_filter_1()) -> iodata().

encode_delete_acls_filter_1(
    _Args = #{
        % The resource type.
        resource_type_filter := ResourceTypeFilter,
        % The resource name.
        resource_name_filter := ResourceNameFilter,
        % The pattern type.
        pattern_type_filter := PatternTypeFilter,
        % The principal filter, or null to accept all principals.
        principal_filter := PrincipalFilter,
        % The host filter, or null to accept all hosts.
        host_filter := HostFilter,
        % The ACL operation.
        operation := Operation,
        % The permission type.
        permission_type := PermissionType
    }
) when
    ?is_int8(ResourceTypeFilter),
    ?is_nullable_string(ResourceNameFilter),
    ?is_int8(PatternTypeFilter),
    ?is_nullable_string(PrincipalFilter),
    ?is_nullable_string(HostFilter),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int8(ResourceTypeFilter),
        ?encode_nullable_string(ResourceNameFilter),
        ?encode_int8(PatternTypeFilter),
        ?encode_nullable_string(PrincipalFilter),
        ?encode_nullable_string(HostFilter),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType)
    ];
encode_delete_acls_filter_1(Args) ->
    ?encoder_error(Args, #{
        resource_type_filter => int8,
        resource_name_filter => nullable_string,
        pattern_type_filter => int8,
        principal_filter => nullable_string,
        host_filter => nullable_string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_delete_acls_filter_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_filter_1(),
    Rest :: binary().

decode_delete_acls_filter_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceTypeFilter, Bin0, Bin1),
    ?_decode_nullable_string(ResourceNameFilter, Bin1, Bin2),
    ?_decode_int8(PatternTypeFilter, Bin2, Bin3),
    ?_decode_nullable_string(PrincipalFilter, Bin3, Bin4),
    ?_decode_nullable_string(HostFilter, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    {
        #{
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

-spec encode_delete_acls_request_2(delete_acls_request_2()) -> iodata().

encode_delete_acls_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The filters to use when deleting ACLs.
        filters := Filters
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Filters)
->
    [
        ?encode_request_header_2(?DELETE_ACLS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_array(Filters, fun encode_delete_acls_filter_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        filters => {array, delete_acls_filter_2}
    }).

-spec decode_delete_acls_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_request_2(),
    Rest :: binary().

decode_delete_acls_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Filters, Bin0, Bin1, ?_decode_element(decode_delete_acls_filter_2)),
    ?decode_tagged_fields(
        fun decode_delete_acls_request_2_tagged_field/3,
        Header#{
            filters => Filters
        },
        Bin1
    ).

-spec decode_delete_acls_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_acls_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_acls_filter_2(delete_acls_filter_2()) -> iodata().

encode_delete_acls_filter_2(
    _Args = #{
        % The resource type.
        resource_type_filter := ResourceTypeFilter,
        % The resource name.
        resource_name_filter := ResourceNameFilter,
        % The pattern type.
        pattern_type_filter := PatternTypeFilter,
        % The principal filter, or null to accept all principals.
        principal_filter := PrincipalFilter,
        % The host filter, or null to accept all hosts.
        host_filter := HostFilter,
        % The ACL operation.
        operation := Operation,
        % The permission type.
        permission_type := PermissionType
    }
) when
    ?is_int8(ResourceTypeFilter),
    ?is_nullable_string(ResourceNameFilter),
    ?is_int8(PatternTypeFilter),
    ?is_nullable_string(PrincipalFilter),
    ?is_nullable_string(HostFilter),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int8(ResourceTypeFilter),
        ?encode_compact_nullable_string(ResourceNameFilter),
        ?encode_int8(PatternTypeFilter),
        ?encode_compact_nullable_string(PrincipalFilter),
        ?encode_compact_nullable_string(HostFilter),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_filter_2(Args) ->
    ?encoder_error(Args, #{
        resource_type_filter => int8,
        resource_name_filter => nullable_string,
        pattern_type_filter => int8,
        principal_filter => nullable_string,
        host_filter => nullable_string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_delete_acls_filter_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_filter_2(),
    Rest :: binary().

decode_delete_acls_filter_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceTypeFilter, Bin0, Bin1),
    ?_decode_compact_nullable_string(ResourceNameFilter, Bin1, Bin2),
    ?_decode_int8(PatternTypeFilter, Bin2, Bin3),
    ?_decode_compact_nullable_string(PrincipalFilter, Bin3, Bin4),
    ?_decode_compact_nullable_string(HostFilter, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_delete_acls_filter_2_tagged_field/3,
        #{
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

-spec decode_delete_acls_filter_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_acls_filter_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_acls_request_3(delete_acls_request_3()) -> iodata().

encode_delete_acls_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The filters to use when deleting ACLs.
        filters := Filters
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Filters)
->
    [
        ?encode_request_header_2(?DELETE_ACLS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_array(Filters, fun encode_delete_acls_filter_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        filters => {array, delete_acls_filter_3}
    }).

-spec decode_delete_acls_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_request_3(),
    Rest :: binary().

decode_delete_acls_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Filters, Bin0, Bin1, ?_decode_element(decode_delete_acls_filter_3)),
    ?decode_tagged_fields(
        fun decode_delete_acls_request_3_tagged_field/3,
        Header#{
            filters => Filters
        },
        Bin1
    ).

-spec decode_delete_acls_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_acls_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_acls_filter_3(delete_acls_filter_3()) -> iodata().

encode_delete_acls_filter_3(
    _Args = #{
        % The resource type.
        resource_type_filter := ResourceTypeFilter,
        % The resource name.
        resource_name_filter := ResourceNameFilter,
        % The pattern type.
        pattern_type_filter := PatternTypeFilter,
        % The principal filter, or null to accept all principals.
        principal_filter := PrincipalFilter,
        % The host filter, or null to accept all hosts.
        host_filter := HostFilter,
        % The ACL operation.
        operation := Operation,
        % The permission type.
        permission_type := PermissionType
    }
) when
    ?is_int8(ResourceTypeFilter),
    ?is_nullable_string(ResourceNameFilter),
    ?is_int8(PatternTypeFilter),
    ?is_nullable_string(PrincipalFilter),
    ?is_nullable_string(HostFilter),
    ?is_int8(Operation),
    ?is_int8(PermissionType)
->
    [
        ?encode_int8(ResourceTypeFilter),
        ?encode_compact_nullable_string(ResourceNameFilter),
        ?encode_int8(PatternTypeFilter),
        ?encode_compact_nullable_string(PrincipalFilter),
        ?encode_compact_nullable_string(HostFilter),
        ?encode_int8(Operation),
        ?encode_int8(PermissionType),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_acls_filter_3(Args) ->
    ?encoder_error(Args, #{
        resource_type_filter => int8,
        resource_name_filter => nullable_string,
        pattern_type_filter => int8,
        principal_filter => nullable_string,
        host_filter => nullable_string,
        operation => int8,
        permission_type => int8
    }).

-spec decode_delete_acls_filter_3(binary()) -> {Decoded, Rest} when
    Decoded :: delete_acls_filter_3(),
    Rest :: binary().

decode_delete_acls_filter_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceTypeFilter, Bin0, Bin1),
    ?_decode_compact_nullable_string(ResourceNameFilter, Bin1, Bin2),
    ?_decode_int8(PatternTypeFilter, Bin2, Bin3),
    ?_decode_compact_nullable_string(PrincipalFilter, Bin3, Bin4),
    ?_decode_compact_nullable_string(HostFilter, Bin4, Bin5),
    ?_decode_int8(Operation, Bin5, Bin6),
    ?_decode_int8(PermissionType, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_delete_acls_filter_3_tagged_field/3,
        #{
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

-spec decode_delete_acls_filter_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_delete_acls_filter_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type delete_acls_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    filters := list(delete_acls_filter_0())
}.
-type delete_acls_filter_0() :: #{
    resource_type_filter := integer(),
    resource_name_filter := binary() | null,
    principal_filter := binary() | null,
    host_filter := binary() | null,
    operation := integer(),
    permission_type := integer()
}.
-type delete_acls_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    filters := list(delete_acls_filter_1())
}.
-type delete_acls_filter_1() :: #{
    resource_type_filter := integer(),
    resource_name_filter := binary() | null,
    pattern_type_filter := integer(),
    principal_filter := binary() | null,
    host_filter := binary() | null,
    operation := integer(),
    permission_type := integer()
}.
-type delete_acls_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    filters := list(delete_acls_filter_2())
}.
-type delete_acls_filter_2() :: #{
    resource_type_filter := integer(),
    resource_name_filter := binary() | null,
    pattern_type_filter := integer(),
    principal_filter := binary() | null,
    host_filter := binary() | null,
    operation := integer(),
    permission_type := integer()
}.
-type delete_acls_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    filters := list(delete_acls_filter_3())
}.
-type delete_acls_filter_3() :: #{
    resource_type_filter := integer(),
    resource_name_filter := binary() | null,
    pattern_type_filter := integer(),
    principal_filter := binary() | null,
    host_filter := binary() | null,
    operation := integer(),
    permission_type := integer()
}.
