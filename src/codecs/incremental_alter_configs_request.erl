-module(incremental_alter_configs_request).
-export([
    encode_incremental_alter_configs_request_0/1,
    decode_incremental_alter_configs_request_0/1,
    encode_incremental_alter_configs_request_1/1,
    decode_incremental_alter_configs_request_1/1
]).
-export_type([
    incremental_alter_configs_request_0/0,
    alterable_config_0/0,
    alter_configs_resource_0/0,
    incremental_alter_configs_request_1/0,
    alterable_config_1/0,
    alter_configs_resource_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(INCREMENTAL_ALTER_CONFIGS_REQUEST, 44).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_incremental_alter_configs_request_0(incremental_alter_configs_request_0()) -> iodata().

encode_incremental_alter_configs_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The incremental updates for each resource.
        resources := Resources,
        % True if we should validate the request, but not change the configurations.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Resources),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_1(?INCREMENTAL_ALTER_CONFIGS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Resources, fun encode_alter_configs_resource_0/1),
        ?encode_bool(ValidateOnly)
    ];
encode_incremental_alter_configs_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resources => {array, alter_configs_resource_0},
        validate_only => bool
    }).

-spec decode_incremental_alter_configs_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: incremental_alter_configs_request_0(),
    Rest :: binary().

decode_incremental_alter_configs_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Resources, Bin0, Bin1, ?_decode_element(decode_alter_configs_resource_0)),
    ?_decode_bool(ValidateOnly, Bin1, Bin2),
    {
        Header#{
            resources => Resources,
            validate_only => ValidateOnly
        },
        Bin2
    }.

-spec encode_alterable_config_0(alterable_config_0()) -> iodata().

encode_alterable_config_0(
    _Args = #{
        % The configuration key name.
        name := Name,
        % The type (Set, Delete, Append, Subtract) of operation.
        config_operation := ConfigOperation,
        % The value to set for the configuration key.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_int8(ConfigOperation),
    ?is_nullable_string(Value)
->
    [
        ?encode_string(Name),
        ?encode_int8(ConfigOperation),
        ?encode_nullable_string(Value)
    ];
encode_alterable_config_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        config_operation => int8,
        value => nullable_string
    }).

-spec decode_alterable_config_0(binary()) -> {Decoded, Rest} when
    Decoded :: alterable_config_0(),
    Rest :: binary().

decode_alterable_config_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int8(ConfigOperation, Bin1, Bin2),
    ?_decode_nullable_string(Value, Bin2, Bin3),
    {
        #{
            name => Name,
            config_operation => ConfigOperation,
            value => Value
        },
        Bin3
    }.

-spec encode_alter_configs_resource_0(alter_configs_resource_0()) -> iodata().

encode_alter_configs_resource_0(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The configurations.
        configs := Configs
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_array(Configs)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_array(Configs, fun encode_alterable_config_0/1)
    ];
encode_alter_configs_resource_0(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        configs => {array, alterable_config_0}
    }).

-spec decode_alter_configs_resource_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_configs_resource_0(),
    Rest :: binary().

decode_alter_configs_resource_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_array(Configs, Bin2, Bin3, ?_decode_element(decode_alterable_config_0)),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            configs => Configs
        },
        Bin3
    }.

-spec encode_incremental_alter_configs_request_1(incremental_alter_configs_request_1()) -> iodata().

encode_incremental_alter_configs_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The incremental updates for each resource.
        resources := Resources,
        % True if we should validate the request, but not change the configurations.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Resources),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_2(?INCREMENTAL_ALTER_CONFIGS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_compact_array(Resources, fun encode_alter_configs_resource_1/1),
        ?encode_bool(ValidateOnly),
        ?EMPTY_TAG_BUFFER
    ];
encode_incremental_alter_configs_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resources => {array, alter_configs_resource_1},
        validate_only => bool
    }).

-spec decode_incremental_alter_configs_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: incremental_alter_configs_request_1(),
    Rest :: binary().

decode_incremental_alter_configs_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Resources, Bin0, Bin1, ?_decode_element(decode_alter_configs_resource_1)),
    ?_decode_bool(ValidateOnly, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_incremental_alter_configs_request_1_tagged_field/3,
        Header#{
            resources => Resources,
            validate_only => ValidateOnly
        },
        Bin2
    ).

-spec decode_incremental_alter_configs_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_incremental_alter_configs_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alterable_config_1(alterable_config_1()) -> iodata().

encode_alterable_config_1(
    _Args = #{
        % The configuration key name.
        name := Name,
        % The type (Set, Delete, Append, Subtract) of operation.
        config_operation := ConfigOperation,
        % The value to set for the configuration key.
        value := Value
    }
) when
    ?is_string(Name),
    ?is_int8(ConfigOperation),
    ?is_nullable_string(Value)
->
    [
        ?encode_compact_string(Name),
        ?encode_int8(ConfigOperation),
        ?encode_compact_nullable_string(Value),
        ?EMPTY_TAG_BUFFER
    ];
encode_alterable_config_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        config_operation => int8,
        value => nullable_string
    }).

-spec decode_alterable_config_1(binary()) -> {Decoded, Rest} when
    Decoded :: alterable_config_1(),
    Rest :: binary().

decode_alterable_config_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int8(ConfigOperation, Bin1, Bin2),
    ?_decode_compact_nullable_string(Value, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_alterable_config_1_tagged_field/3,
        #{
            name => Name,
            config_operation => ConfigOperation,
            value => Value
        },
        Bin3
    ).

-spec decode_alterable_config_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alterable_config_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_configs_resource_1(alter_configs_resource_1()) -> iodata().

encode_alter_configs_resource_1(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The configurations.
        configs := Configs
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_array(Configs)
->
    [
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_compact_array(Configs, fun encode_alterable_config_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_configs_resource_1(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        configs => {array, alterable_config_1}
    }).

-spec decode_alter_configs_resource_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_configs_resource_1(),
    Rest :: binary().

decode_alter_configs_resource_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_compact_string(ResourceName, Bin1, Bin2),
    ?_decode_compact_array(Configs, Bin2, Bin3, ?_decode_element(decode_alterable_config_1)),
    ?decode_tagged_fields(
        fun decode_alter_configs_resource_1_tagged_field/3,
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            configs => Configs
        },
        Bin3
    ).

-spec decode_alter_configs_resource_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_configs_resource_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type incremental_alter_configs_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resources := list(alter_configs_resource_0()),
    validate_only := boolean()
}.
-type alterable_config_0() :: #{
    name := binary(),
    config_operation := integer(),
    value := binary() | null
}.
-type alter_configs_resource_0() :: #{
    resource_type := integer(),
    resource_name := binary(),
    configs := list(alterable_config_0())
}.
-type incremental_alter_configs_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resources := list(alter_configs_resource_1()),
    validate_only := boolean()
}.
-type alterable_config_1() :: #{
    name := binary(),
    config_operation := integer(),
    value := binary() | null
}.
-type alter_configs_resource_1() :: #{
    resource_type := integer(),
    resource_name := binary(),
    configs := list(alterable_config_1())
}.
