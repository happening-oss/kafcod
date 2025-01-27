-module(describe_configs_request).
-export([
    encode_describe_configs_request_0/1,
    decode_describe_configs_request_0/1,
    encode_describe_configs_request_1/1,
    decode_describe_configs_request_1/1,
    encode_describe_configs_request_2/1,
    decode_describe_configs_request_2/1,
    encode_describe_configs_request_3/1,
    decode_describe_configs_request_3/1,
    encode_describe_configs_request_4/1,
    decode_describe_configs_request_4/1
]).
-export_type([
    describe_configs_request_0/0,
    describe_configs_resource_0/0,
    describe_configs_request_1/0,
    describe_configs_resource_1/0,
    describe_configs_request_2/0,
    describe_configs_resource_2/0,
    describe_configs_request_3/0,
    describe_configs_resource_3/0,
    describe_configs_request_4/0,
    describe_configs_resource_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_CONFIGS_REQUEST, 32).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_configs_request_0(describe_configs_request_0()) -> iodata().

encode_describe_configs_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resources whose configurations we want to describe.
        resources := Resources
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Resources)
->
    [
        ?encode_request_header_1(?DESCRIBE_CONFIGS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Resources, fun encode_describe_configs_resource_0/1)
    ];
encode_describe_configs_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resources => {array, describe_configs_resource_0}
    }).

-spec decode_describe_configs_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_request_0(),
    Rest :: binary().

decode_describe_configs_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Resources, Bin0, Bin1, ?_decode_element(decode_describe_configs_resource_0)),
    {
        Header#{
            resources => Resources
        },
        Bin1
    }.

-spec encode_describe_configs_resource_0(describe_configs_resource_0()) -> iodata().

encode_describe_configs_resource_0(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The configuration keys to list, or null to list all configuration keys.
        configuration_keys := ConfigurationKeys
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_nullable_array(ConfigurationKeys)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_nullable_array(ConfigurationKeys, ?encode_string_)
    ];
encode_describe_configs_resource_0(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        configuration_keys => {nullable_array, string}
    }).

-spec decode_describe_configs_resource_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_0(),
    Rest :: binary().

decode_describe_configs_resource_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_nullable_array(ConfigurationKeys, Bin2, Bin3, ?decode_string_),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            configuration_keys => ConfigurationKeys
        },
        Bin3
    }.

-spec encode_describe_configs_request_1(describe_configs_request_1()) -> iodata().

encode_describe_configs_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resources whose configurations we want to describe.
        resources := Resources,
        % True if we should include all synonyms.
        include_synonyms := IncludeSynonyms
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Resources),
    ?is_bool(IncludeSynonyms)
->
    [
        ?encode_request_header_1(?DESCRIBE_CONFIGS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Resources, fun encode_describe_configs_resource_1/1),
        ?encode_bool(IncludeSynonyms)
    ];
encode_describe_configs_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resources => {array, describe_configs_resource_1},
        include_synonyms => bool
    }).

-spec decode_describe_configs_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_request_1(),
    Rest :: binary().

decode_describe_configs_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Resources, Bin0, Bin1, ?_decode_element(decode_describe_configs_resource_1)),
    ?_decode_bool(IncludeSynonyms, Bin1, Bin2),
    {
        Header#{
            resources => Resources,
            include_synonyms => IncludeSynonyms
        },
        Bin2
    }.

-spec encode_describe_configs_resource_1(describe_configs_resource_1()) -> iodata().

encode_describe_configs_resource_1(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The configuration keys to list, or null to list all configuration keys.
        configuration_keys := ConfigurationKeys
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_nullable_array(ConfigurationKeys)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_nullable_array(ConfigurationKeys, ?encode_string_)
    ];
encode_describe_configs_resource_1(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        configuration_keys => {nullable_array, string}
    }).

-spec decode_describe_configs_resource_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_1(),
    Rest :: binary().

decode_describe_configs_resource_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_nullable_array(ConfigurationKeys, Bin2, Bin3, ?decode_string_),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            configuration_keys => ConfigurationKeys
        },
        Bin3
    }.

-spec encode_describe_configs_request_2(describe_configs_request_2()) -> iodata().

encode_describe_configs_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resources whose configurations we want to describe.
        resources := Resources,
        % True if we should include all synonyms.
        include_synonyms := IncludeSynonyms
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Resources),
    ?is_bool(IncludeSynonyms)
->
    [
        ?encode_request_header_1(?DESCRIBE_CONFIGS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_array(Resources, fun encode_describe_configs_resource_2/1),
        ?encode_bool(IncludeSynonyms)
    ];
encode_describe_configs_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resources => {array, describe_configs_resource_2},
        include_synonyms => bool
    }).

-spec decode_describe_configs_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_request_2(),
    Rest :: binary().

decode_describe_configs_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Resources, Bin0, Bin1, ?_decode_element(decode_describe_configs_resource_2)),
    ?_decode_bool(IncludeSynonyms, Bin1, Bin2),
    {
        Header#{
            resources => Resources,
            include_synonyms => IncludeSynonyms
        },
        Bin2
    }.

-spec encode_describe_configs_resource_2(describe_configs_resource_2()) -> iodata().

encode_describe_configs_resource_2(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The configuration keys to list, or null to list all configuration keys.
        configuration_keys := ConfigurationKeys
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_nullable_array(ConfigurationKeys)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_nullable_array(ConfigurationKeys, ?encode_string_)
    ];
encode_describe_configs_resource_2(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        configuration_keys => {nullable_array, string}
    }).

-spec decode_describe_configs_resource_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_2(),
    Rest :: binary().

decode_describe_configs_resource_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_nullable_array(ConfigurationKeys, Bin2, Bin3, ?decode_string_),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            configuration_keys => ConfigurationKeys
        },
        Bin3
    }.

-spec encode_describe_configs_request_3(describe_configs_request_3()) -> iodata().

encode_describe_configs_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resources whose configurations we want to describe.
        resources := Resources,
        % True if we should include all synonyms.
        include_synonyms := IncludeSynonyms,
        % True if we should include configuration documentation.
        include_documentation := IncludeDocumentation
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Resources),
    ?is_bool(IncludeSynonyms),
    ?is_bool(IncludeDocumentation)
->
    [
        ?encode_request_header_1(?DESCRIBE_CONFIGS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_array(Resources, fun encode_describe_configs_resource_3/1),
        ?encode_bool(IncludeSynonyms),
        ?encode_bool(IncludeDocumentation)
    ];
encode_describe_configs_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resources => {array, describe_configs_resource_3},
        include_synonyms => bool,
        include_documentation => bool
    }).

-spec decode_describe_configs_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_request_3(),
    Rest :: binary().

decode_describe_configs_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Resources, Bin0, Bin1, ?_decode_element(decode_describe_configs_resource_3)),
    ?_decode_bool(IncludeSynonyms, Bin1, Bin2),
    ?_decode_bool(IncludeDocumentation, Bin2, Bin3),
    {
        Header#{
            resources => Resources,
            include_synonyms => IncludeSynonyms,
            include_documentation => IncludeDocumentation
        },
        Bin3
    }.

-spec encode_describe_configs_resource_3(describe_configs_resource_3()) -> iodata().

encode_describe_configs_resource_3(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The configuration keys to list, or null to list all configuration keys.
        configuration_keys := ConfigurationKeys
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_nullable_array(ConfigurationKeys)
->
    [
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_nullable_array(ConfigurationKeys, ?encode_string_)
    ];
encode_describe_configs_resource_3(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        configuration_keys => {nullable_array, string}
    }).

-spec decode_describe_configs_resource_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_3(),
    Rest :: binary().

decode_describe_configs_resource_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_string(ResourceName, Bin1, Bin2),
    ?_decode_nullable_array(ConfigurationKeys, Bin2, Bin3, ?decode_string_),
    {
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            configuration_keys => ConfigurationKeys
        },
        Bin3
    }.

-spec encode_describe_configs_request_4(describe_configs_request_4()) -> iodata().

encode_describe_configs_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The resources whose configurations we want to describe.
        resources := Resources,
        % True if we should include all synonyms.
        include_synonyms := IncludeSynonyms,
        % True if we should include configuration documentation.
        include_documentation := IncludeDocumentation
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Resources),
    ?is_bool(IncludeSynonyms),
    ?is_bool(IncludeDocumentation)
->
    [
        ?encode_request_header_2(?DESCRIBE_CONFIGS_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_array(Resources, fun encode_describe_configs_resource_4/1),
        ?encode_bool(IncludeSynonyms),
        ?encode_bool(IncludeDocumentation),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_configs_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        resources => {array, describe_configs_resource_4},
        include_synonyms => bool,
        include_documentation => bool
    }).

-spec decode_describe_configs_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_request_4(),
    Rest :: binary().

decode_describe_configs_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Resources, Bin0, Bin1, ?_decode_element(decode_describe_configs_resource_4)),
    ?_decode_bool(IncludeSynonyms, Bin1, Bin2),
    ?_decode_bool(IncludeDocumentation, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_describe_configs_request_4_tagged_field/3,
        Header#{
            resources => Resources,
            include_synonyms => IncludeSynonyms,
            include_documentation => IncludeDocumentation
        },
        Bin3
    ).

-spec decode_describe_configs_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_configs_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_configs_resource_4(describe_configs_resource_4()) -> iodata().

encode_describe_configs_resource_4(
    _Args = #{
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % The configuration keys to list, or null to list all configuration keys.
        configuration_keys := ConfigurationKeys
    }
) when
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_nullable_array(ConfigurationKeys)
->
    [
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_compact_nullable_array(ConfigurationKeys, ?encode_compact_string_),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_configs_resource_4(Args) ->
    ?encoder_error(Args, #{
        resource_type => int8,
        resource_name => string,
        configuration_keys => {nullable_array, string}
    }).

-spec decode_describe_configs_resource_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_4(),
    Rest :: binary().

decode_describe_configs_resource_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(ResourceType, Bin0, Bin1),
    ?_decode_compact_string(ResourceName, Bin1, Bin2),
    ?_decode_compact_nullable_array(ConfigurationKeys, Bin2, Bin3, ?decode_string_),
    ?decode_tagged_fields(
        fun decode_describe_configs_resource_4_tagged_field/3,
        #{
            resource_type => ResourceType,
            resource_name => ResourceName,
            configuration_keys => ConfigurationKeys
        },
        Bin3
    ).

-spec decode_describe_configs_resource_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_configs_resource_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_configs_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resources := list(describe_configs_resource_0())
}.
-type describe_configs_resource_0() :: #{
    resource_type := integer(),
    resource_name := binary(),
    configuration_keys := list(binary()) | null
}.
-type describe_configs_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resources := list(describe_configs_resource_1()),
    include_synonyms := boolean()
}.
-type describe_configs_resource_1() :: #{
    resource_type := integer(),
    resource_name := binary(),
    configuration_keys := list(binary()) | null
}.
-type describe_configs_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resources := list(describe_configs_resource_2()),
    include_synonyms := boolean()
}.
-type describe_configs_resource_2() :: #{
    resource_type := integer(),
    resource_name := binary(),
    configuration_keys := list(binary()) | null
}.
-type describe_configs_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resources := list(describe_configs_resource_3()),
    include_synonyms := boolean(),
    include_documentation := boolean()
}.
-type describe_configs_resource_3() :: #{
    resource_type := integer(),
    resource_name := binary(),
    configuration_keys := list(binary()) | null
}.
-type describe_configs_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    resources := list(describe_configs_resource_4()),
    include_synonyms := boolean(),
    include_documentation := boolean()
}.
-type describe_configs_resource_4() :: #{
    resource_type := integer(),
    resource_name := binary(),
    configuration_keys := list(binary()) | null
}.
