-module(describe_configs_response).
-export([
    encode_describe_configs_response_0/1,
    decode_describe_configs_response_0/1,
    encode_describe_configs_response_1/1,
    decode_describe_configs_response_1/1,
    encode_describe_configs_response_2/1,
    decode_describe_configs_response_2/1,
    encode_describe_configs_response_3/1,
    decode_describe_configs_response_3/1,
    encode_describe_configs_response_4/1,
    decode_describe_configs_response_4/1
]).
-export_type([
    describe_configs_response_0/0,
    describe_configs_resource_result_0/0,
    describe_configs_result_0/0,
    describe_configs_response_1/0,
    describe_configs_synonym_1/0,
    describe_configs_resource_result_1/0,
    describe_configs_result_1/0,
    describe_configs_response_2/0,
    describe_configs_synonym_2/0,
    describe_configs_resource_result_2/0,
    describe_configs_result_2/0,
    describe_configs_response_3/0,
    describe_configs_synonym_3/0,
    describe_configs_resource_result_3/0,
    describe_configs_result_3/0,
    describe_configs_response_4/0,
    describe_configs_synonym_4/0,
    describe_configs_resource_result_4/0,
    describe_configs_result_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_configs_response_0(describe_configs_response_0()) -> iodata().

encode_describe_configs_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each resource.
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
        ?encode_array(Results, fun encode_describe_configs_result_0/1)
    ];
encode_describe_configs_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, describe_configs_result_0}
    }).

-spec decode_describe_configs_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_response_0(),
    Rest :: binary().

decode_describe_configs_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_describe_configs_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_describe_configs_resource_result_0(describe_configs_resource_result_0()) -> iodata().

encode_describe_configs_resource_result_0(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value,
        % True if the configuration is read-only.
        read_only := ReadOnly,
        % True if the configuration is not set.
        is_default := IsDefault,
        % True if this configuration is sensitive.
        is_sensitive := IsSensitive
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_bool(ReadOnly),
    ?is_bool(IsDefault),
    ?is_bool(IsSensitive)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value),
        ?encode_bool(ReadOnly),
        ?encode_bool(IsDefault),
        ?encode_bool(IsSensitive)
    ];
encode_describe_configs_resource_result_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        read_only => bool,
        is_default => bool,
        is_sensitive => bool
    }).

-spec decode_describe_configs_resource_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_result_0(),
    Rest :: binary().

decode_describe_configs_resource_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    ?_decode_bool(ReadOnly, Bin2, Bin3),
    ?_decode_bool(IsDefault, Bin3, Bin4),
    ?_decode_bool(IsSensitive, Bin4, Bin5),
    {
        #{
            name => Name,
            value => Value,
            read_only => ReadOnly,
            is_default => IsDefault,
            is_sensitive => IsSensitive
        },
        Bin5
    }.

-spec encode_describe_configs_result_0(describe_configs_result_0()) -> iodata().

encode_describe_configs_result_0(
    _Args = #{
        % The error code, or 0 if we were able to successfully describe the configurations.
        error_code := ErrorCode,
        % The error message, or null if we were able to successfully describe the configurations.
        error_message := ErrorMessage,
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % Each listed configuration.
        configs := Configs
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_array(Configs)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_array(Configs, fun encode_describe_configs_resource_result_0/1)
    ];
encode_describe_configs_result_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        configs => {array, describe_configs_resource_result_0}
    }).

-spec decode_describe_configs_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_result_0(),
    Rest :: binary().

decode_describe_configs_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_string(ResourceName, Bin3, Bin4),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_describe_configs_resource_result_0)),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            configs => Configs
        },
        Bin5
    }.

-spec encode_describe_configs_response_1(describe_configs_response_1()) -> iodata().

encode_describe_configs_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each resource.
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
        ?encode_array(Results, fun encode_describe_configs_result_1/1)
    ];
encode_describe_configs_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, describe_configs_result_1}
    }).

-spec decode_describe_configs_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_response_1(),
    Rest :: binary().

decode_describe_configs_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_describe_configs_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_describe_configs_synonym_1(describe_configs_synonym_1()) -> iodata().

encode_describe_configs_synonym_1(
    _Args = #{
        % The synonym name.
        name := Name,
        % The synonym value.
        value := Value,
        % The synonym source.
        source := Source
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_int8(Source)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value),
        ?encode_int8(Source)
    ];
encode_describe_configs_synonym_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        source => int8
    }).

-spec decode_describe_configs_synonym_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_synonym_1(),
    Rest :: binary().

decode_describe_configs_synonym_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    ?_decode_int8(Source, Bin2, Bin3),
    {
        #{
            name => Name,
            value => Value,
            source => Source
        },
        Bin3
    }.

-spec encode_describe_configs_resource_result_1(describe_configs_resource_result_1()) -> iodata().

encode_describe_configs_resource_result_1(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value,
        % True if the configuration is read-only.
        read_only := ReadOnly,
        % The configuration source.
        config_source := ConfigSource,
        % True if this configuration is sensitive.
        is_sensitive := IsSensitive,
        % The synonyms for this configuration key.
        synonyms := Synonyms
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_bool(ReadOnly),
    ?is_int8(ConfigSource),
    ?is_bool(IsSensitive),
    ?is_array(Synonyms)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value),
        ?encode_bool(ReadOnly),
        ?encode_int8(ConfigSource),
        ?encode_bool(IsSensitive),
        ?encode_array(Synonyms, fun encode_describe_configs_synonym_1/1)
    ];
encode_describe_configs_resource_result_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        read_only => bool,
        config_source => int8,
        is_sensitive => bool,
        synonyms => {array, describe_configs_synonym_1}
    }).

-spec decode_describe_configs_resource_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_result_1(),
    Rest :: binary().

decode_describe_configs_resource_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    ?_decode_bool(ReadOnly, Bin2, Bin3),
    ?_decode_int8(ConfigSource, Bin3, Bin4),
    ?_decode_bool(IsSensitive, Bin4, Bin5),
    ?_decode_array(Synonyms, Bin5, Bin6, ?_decode_element(decode_describe_configs_synonym_1)),
    {
        #{
            name => Name,
            value => Value,
            read_only => ReadOnly,
            config_source => ConfigSource,
            is_sensitive => IsSensitive,
            synonyms => Synonyms
        },
        Bin6
    }.

-spec encode_describe_configs_result_1(describe_configs_result_1()) -> iodata().

encode_describe_configs_result_1(
    _Args = #{
        % The error code, or 0 if we were able to successfully describe the configurations.
        error_code := ErrorCode,
        % The error message, or null if we were able to successfully describe the configurations.
        error_message := ErrorMessage,
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % Each listed configuration.
        configs := Configs
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_array(Configs)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_array(Configs, fun encode_describe_configs_resource_result_1/1)
    ];
encode_describe_configs_result_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        configs => {array, describe_configs_resource_result_1}
    }).

-spec decode_describe_configs_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_result_1(),
    Rest :: binary().

decode_describe_configs_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_string(ResourceName, Bin3, Bin4),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_describe_configs_resource_result_1)),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            configs => Configs
        },
        Bin5
    }.

-spec encode_describe_configs_response_2(describe_configs_response_2()) -> iodata().

encode_describe_configs_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each resource.
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
        ?encode_array(Results, fun encode_describe_configs_result_2/1)
    ];
encode_describe_configs_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, describe_configs_result_2}
    }).

-spec decode_describe_configs_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_response_2(),
    Rest :: binary().

decode_describe_configs_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_describe_configs_result_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_describe_configs_synonym_2(describe_configs_synonym_2()) -> iodata().

encode_describe_configs_synonym_2(
    _Args = #{
        % The synonym name.
        name := Name,
        % The synonym value.
        value := Value,
        % The synonym source.
        source := Source
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_int8(Source)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value),
        ?encode_int8(Source)
    ];
encode_describe_configs_synonym_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        source => int8
    }).

-spec decode_describe_configs_synonym_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_synonym_2(),
    Rest :: binary().

decode_describe_configs_synonym_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    ?_decode_int8(Source, Bin2, Bin3),
    {
        #{
            name => Name,
            value => Value,
            source => Source
        },
        Bin3
    }.

-spec encode_describe_configs_resource_result_2(describe_configs_resource_result_2()) -> iodata().

encode_describe_configs_resource_result_2(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value,
        % True if the configuration is read-only.
        read_only := ReadOnly,
        % The configuration source.
        config_source := ConfigSource,
        % True if this configuration is sensitive.
        is_sensitive := IsSensitive,
        % The synonyms for this configuration key.
        synonyms := Synonyms
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_bool(ReadOnly),
    ?is_int8(ConfigSource),
    ?is_bool(IsSensitive),
    ?is_array(Synonyms)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value),
        ?encode_bool(ReadOnly),
        ?encode_int8(ConfigSource),
        ?encode_bool(IsSensitive),
        ?encode_array(Synonyms, fun encode_describe_configs_synonym_2/1)
    ];
encode_describe_configs_resource_result_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        read_only => bool,
        config_source => int8,
        is_sensitive => bool,
        synonyms => {array, describe_configs_synonym_2}
    }).

-spec decode_describe_configs_resource_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_result_2(),
    Rest :: binary().

decode_describe_configs_resource_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    ?_decode_bool(ReadOnly, Bin2, Bin3),
    ?_decode_int8(ConfigSource, Bin3, Bin4),
    ?_decode_bool(IsSensitive, Bin4, Bin5),
    ?_decode_array(Synonyms, Bin5, Bin6, ?_decode_element(decode_describe_configs_synonym_2)),
    {
        #{
            name => Name,
            value => Value,
            read_only => ReadOnly,
            config_source => ConfigSource,
            is_sensitive => IsSensitive,
            synonyms => Synonyms
        },
        Bin6
    }.

-spec encode_describe_configs_result_2(describe_configs_result_2()) -> iodata().

encode_describe_configs_result_2(
    _Args = #{
        % The error code, or 0 if we were able to successfully describe the configurations.
        error_code := ErrorCode,
        % The error message, or null if we were able to successfully describe the configurations.
        error_message := ErrorMessage,
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % Each listed configuration.
        configs := Configs
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_array(Configs)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_array(Configs, fun encode_describe_configs_resource_result_2/1)
    ];
encode_describe_configs_result_2(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        configs => {array, describe_configs_resource_result_2}
    }).

-spec decode_describe_configs_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_result_2(),
    Rest :: binary().

decode_describe_configs_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_string(ResourceName, Bin3, Bin4),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_describe_configs_resource_result_2)),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            configs => Configs
        },
        Bin5
    }.

-spec encode_describe_configs_response_3(describe_configs_response_3()) -> iodata().

encode_describe_configs_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each resource.
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
        ?encode_array(Results, fun encode_describe_configs_result_3/1)
    ];
encode_describe_configs_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, describe_configs_result_3}
    }).

-spec decode_describe_configs_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_response_3(),
    Rest :: binary().

decode_describe_configs_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Results, Bin1, Bin2, ?_decode_element(decode_describe_configs_result_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    }.

-spec encode_describe_configs_synonym_3(describe_configs_synonym_3()) -> iodata().

encode_describe_configs_synonym_3(
    _Args = #{
        % The synonym name.
        name := Name,
        % The synonym value.
        value := Value,
        % The synonym source.
        source := Source
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_int8(Source)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value),
        ?encode_int8(Source)
    ];
encode_describe_configs_synonym_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        source => int8
    }).

-spec decode_describe_configs_synonym_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_synonym_3(),
    Rest :: binary().

decode_describe_configs_synonym_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    ?_decode_int8(Source, Bin2, Bin3),
    {
        #{
            name => Name,
            value => Value,
            source => Source
        },
        Bin3
    }.

-spec encode_describe_configs_resource_result_3(describe_configs_resource_result_3()) -> iodata().

encode_describe_configs_resource_result_3(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value,
        % True if the configuration is read-only.
        read_only := ReadOnly,
        % The configuration source.
        config_source := ConfigSource,
        % True if this configuration is sensitive.
        is_sensitive := IsSensitive,
        % The synonyms for this configuration key.
        synonyms := Synonyms,
        % The configuration data type. Type can be one of the following values - BOOLEAN, STRING, INT, SHORT, LONG, DOUBLE, LIST, CLASS, PASSWORD
        config_type := ConfigType,
        % The configuration documentation.
        documentation := Documentation
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_bool(ReadOnly),
    ?is_int8(ConfigSource),
    ?is_bool(IsSensitive),
    ?is_array(Synonyms),
    ?is_int8(ConfigType),
    ?is_nullable_string(Documentation)
->
    [
        ?encode_string(Name),
        ?encode_nullable_string(Value),
        ?encode_bool(ReadOnly),
        ?encode_int8(ConfigSource),
        ?encode_bool(IsSensitive),
        ?encode_array(Synonyms, fun encode_describe_configs_synonym_3/1),
        ?encode_int8(ConfigType),
        ?encode_nullable_string(Documentation)
    ];
encode_describe_configs_resource_result_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        read_only => bool,
        config_source => int8,
        is_sensitive => bool,
        synonyms => {array, describe_configs_synonym_3},
        config_type => int8,
        documentation => nullable_string
    }).

-spec decode_describe_configs_resource_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_result_3(),
    Rest :: binary().

decode_describe_configs_resource_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_nullable_string(Value, Bin1, Bin2),
    ?_decode_bool(ReadOnly, Bin2, Bin3),
    ?_decode_int8(ConfigSource, Bin3, Bin4),
    ?_decode_bool(IsSensitive, Bin4, Bin5),
    ?_decode_array(Synonyms, Bin5, Bin6, ?_decode_element(decode_describe_configs_synonym_3)),
    ?_decode_int8(ConfigType, Bin6, Bin7),
    ?_decode_nullable_string(Documentation, Bin7, Bin8),
    {
        #{
            name => Name,
            value => Value,
            read_only => ReadOnly,
            config_source => ConfigSource,
            is_sensitive => IsSensitive,
            synonyms => Synonyms,
            config_type => ConfigType,
            documentation => Documentation
        },
        Bin8
    }.

-spec encode_describe_configs_result_3(describe_configs_result_3()) -> iodata().

encode_describe_configs_result_3(
    _Args = #{
        % The error code, or 0 if we were able to successfully describe the configurations.
        error_code := ErrorCode,
        % The error message, or null if we were able to successfully describe the configurations.
        error_message := ErrorMessage,
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % Each listed configuration.
        configs := Configs
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_array(Configs)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName),
        ?encode_array(Configs, fun encode_describe_configs_resource_result_3/1)
    ];
encode_describe_configs_result_3(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        configs => {array, describe_configs_resource_result_3}
    }).

-spec decode_describe_configs_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_result_3(),
    Rest :: binary().

decode_describe_configs_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_string(ResourceName, Bin3, Bin4),
    ?_decode_array(Configs, Bin4, Bin5, ?_decode_element(decode_describe_configs_resource_result_3)),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            configs => Configs
        },
        Bin5
    }.

-spec encode_describe_configs_response_4(describe_configs_response_4()) -> iodata().

encode_describe_configs_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for each resource.
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
        ?encode_compact_array(Results, fun encode_describe_configs_result_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_configs_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, describe_configs_result_4}
    }).

-spec decode_describe_configs_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_response_4(),
    Rest :: binary().

decode_describe_configs_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Results, Bin1, Bin2, ?_decode_element(decode_describe_configs_result_4)),
    ?decode_tagged_fields(
        fun decode_describe_configs_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    ).

-spec decode_describe_configs_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_configs_response_4().

decode_describe_configs_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_configs_synonym_4(describe_configs_synonym_4()) -> iodata().

encode_describe_configs_synonym_4(
    _Args = #{
        % The synonym name.
        name := Name,
        % The synonym value.
        value := Value,
        % The synonym source.
        source := Source
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_int8(Source)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_nullable_string(Value),
        ?encode_int8(Source),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_configs_synonym_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        source => int8
    }).

-spec decode_describe_configs_synonym_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_synonym_4(),
    Rest :: binary().

decode_describe_configs_synonym_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_nullable_string(Value, Bin1, Bin2),
    ?_decode_int8(Source, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_describe_configs_synonym_4_tagged_field/3,
        #{
            name => Name,
            value => Value,
            source => Source
        },
        Bin3
    ).

-spec decode_describe_configs_synonym_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_configs_synonym_4().

decode_describe_configs_synonym_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_configs_resource_result_4(describe_configs_resource_result_4()) -> iodata().

encode_describe_configs_resource_result_4(
    _Args = #{
        % The configuration name.
        name := Name,
        % The configuration value.
        value := Value,
        % True if the configuration is read-only.
        read_only := ReadOnly,
        % The configuration source.
        config_source := ConfigSource,
        % True if this configuration is sensitive.
        is_sensitive := IsSensitive,
        % The synonyms for this configuration key.
        synonyms := Synonyms,
        % The configuration data type. Type can be one of the following values - BOOLEAN, STRING, INT, SHORT, LONG, DOUBLE, LIST, CLASS, PASSWORD
        config_type := ConfigType,
        % The configuration documentation.
        documentation := Documentation
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_bool(ReadOnly),
    ?is_int8(ConfigSource),
    ?is_bool(IsSensitive),
    ?is_array(Synonyms),
    ?is_int8(ConfigType),
    ?is_nullable_string(Documentation)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_nullable_string(Value),
        ?encode_bool(ReadOnly),
        ?encode_int8(ConfigSource),
        ?encode_bool(IsSensitive),
        ?encode_compact_array(Synonyms, fun encode_describe_configs_synonym_4/1),
        ?encode_int8(ConfigType),
        ?encode_compact_nullable_string(Documentation),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_configs_resource_result_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        read_only => bool,
        config_source => int8,
        is_sensitive => bool,
        synonyms => {array, describe_configs_synonym_4},
        config_type => int8,
        documentation => nullable_string
    }).

-spec decode_describe_configs_resource_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_resource_result_4(),
    Rest :: binary().

decode_describe_configs_resource_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_nullable_string(Value, Bin1, Bin2),
    ?_decode_bool(ReadOnly, Bin2, Bin3),
    ?_decode_int8(ConfigSource, Bin3, Bin4),
    ?_decode_bool(IsSensitive, Bin4, Bin5),
    ?_decode_compact_array(Synonyms, Bin5, Bin6, ?_decode_element(decode_describe_configs_synonym_4)),
    ?_decode_int8(ConfigType, Bin6, Bin7),
    ?_decode_compact_nullable_string(Documentation, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_describe_configs_resource_result_4_tagged_field/3,
        #{
            name => Name,
            value => Value,
            read_only => ReadOnly,
            config_source => ConfigSource,
            is_sensitive => IsSensitive,
            synonyms => Synonyms,
            config_type => ConfigType,
            documentation => Documentation
        },
        Bin8
    ).

-spec decode_describe_configs_resource_result_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_configs_resource_result_4().

decode_describe_configs_resource_result_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_configs_result_4(describe_configs_result_4()) -> iodata().

encode_describe_configs_result_4(
    _Args = #{
        % The error code, or 0 if we were able to successfully describe the configurations.
        error_code := ErrorCode,
        % The error message, or null if we were able to successfully describe the configurations.
        error_message := ErrorMessage,
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName,
        % Each listed configuration.
        configs := Configs
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName),
    ?is_array(Configs)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?encode_compact_array(Configs, fun encode_describe_configs_resource_result_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_configs_result_4(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string,
        configs => {array, describe_configs_resource_result_4}
    }).

-spec decode_describe_configs_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_configs_result_4(),
    Rest :: binary().

decode_describe_configs_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_compact_string(ResourceName, Bin3, Bin4),
    ?_decode_compact_array(Configs, Bin4, Bin5, ?_decode_element(decode_describe_configs_resource_result_4)),
    ?decode_tagged_fields(
        fun decode_describe_configs_result_4_tagged_field/3,
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName,
            configs => Configs
        },
        Bin5
    ).

-spec decode_describe_configs_result_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_configs_result_4().

decode_describe_configs_result_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_configs_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(describe_configs_result_0())
}.
-type describe_configs_resource_result_0() :: #{
    name := binary(),
    value := binary() | null,
    read_only := boolean(),
    is_default := boolean(),
    is_sensitive := boolean()
}.
-type describe_configs_result_0() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    configs := list(describe_configs_resource_result_0())
}.
-type describe_configs_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(describe_configs_result_1())
}.
-type describe_configs_synonym_1() :: #{
    name := binary(),
    value := binary() | null,
    source := integer()
}.
-type describe_configs_resource_result_1() :: #{
    name := binary(),
    value := binary() | null,
    read_only := boolean(),
    config_source := integer(),
    is_sensitive := boolean(),
    synonyms := list(describe_configs_synonym_1())
}.
-type describe_configs_result_1() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    configs := list(describe_configs_resource_result_1())
}.
-type describe_configs_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(describe_configs_result_2())
}.
-type describe_configs_synonym_2() :: #{
    name := binary(),
    value := binary() | null,
    source := integer()
}.
-type describe_configs_resource_result_2() :: #{
    name := binary(),
    value := binary() | null,
    read_only := boolean(),
    config_source := integer(),
    is_sensitive := boolean(),
    synonyms := list(describe_configs_synonym_2())
}.
-type describe_configs_result_2() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    configs := list(describe_configs_resource_result_2())
}.
-type describe_configs_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(describe_configs_result_3())
}.
-type describe_configs_synonym_3() :: #{
    name := binary(),
    value := binary() | null,
    source := integer()
}.
-type describe_configs_resource_result_3() :: #{
    name := binary(),
    value := binary() | null,
    read_only := boolean(),
    config_source := integer(),
    is_sensitive := boolean(),
    synonyms := list(describe_configs_synonym_3()),
    config_type := integer(),
    documentation := binary() | null
}.
-type describe_configs_result_3() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    configs := list(describe_configs_resource_result_3())
}.
-type describe_configs_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(describe_configs_result_4())
}.
-type describe_configs_synonym_4() :: #{
    name := binary(),
    value := binary() | null,
    source := integer()
}.
-type describe_configs_resource_result_4() :: #{
    name := binary(),
    value := binary() | null,
    read_only := boolean(),
    config_source := integer(),
    is_sensitive := boolean(),
    synonyms := list(describe_configs_synonym_4()),
    config_type := integer(),
    documentation := binary() | null
}.
-type describe_configs_result_4() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary(),
    configs := list(describe_configs_resource_result_4())
}.
