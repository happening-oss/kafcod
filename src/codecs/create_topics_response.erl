-module(create_topics_response).
-export([
    encode_create_topics_response_0/1,
    decode_create_topics_response_0/1,
    encode_create_topics_response_1/1,
    decode_create_topics_response_1/1,
    encode_create_topics_response_2/1,
    decode_create_topics_response_2/1,
    encode_create_topics_response_3/1,
    decode_create_topics_response_3/1,
    encode_create_topics_response_4/1,
    decode_create_topics_response_4/1,
    encode_create_topics_response_5/1,
    decode_create_topics_response_5/1,
    encode_create_topics_response_6/1,
    decode_create_topics_response_6/1,
    encode_create_topics_response_7/1,
    decode_create_topics_response_7/1
]).
-export_type([
    create_topics_response_0/0,
    creatable_topic_result_0/0,
    create_topics_response_1/0,
    creatable_topic_result_1/0,
    create_topics_response_2/0,
    creatable_topic_result_2/0,
    create_topics_response_3/0,
    creatable_topic_result_3/0,
    create_topics_response_4/0,
    creatable_topic_result_4/0,
    create_topics_response_5/0,
    creatable_topic_configs_5/0,
    creatable_topic_result_5/0,
    create_topics_response_6/0,
    creatable_topic_configs_6/0,
    creatable_topic_result_6/0,
    create_topics_response_7/0,
    creatable_topic_configs_7/0,
    creatable_topic_result_7/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_create_topics_response_0(create_topics_response_0()) -> iodata().

encode_create_topics_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Results for each topic we tried to create.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_creatable_topic_result_0/1)
    ];
encode_create_topics_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, creatable_topic_result_0}
    }).

-spec decode_create_topics_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_response_0(),
    Rest :: binary().

decode_create_topics_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_result_0)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_creatable_topic_result_0(creatable_topic_result_0()) -> iodata().

encode_creatable_topic_result_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode)
    ];
encode_creatable_topic_result_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16
    }).

-spec decode_creatable_topic_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_result_0(),
    Rest :: binary().

decode_creatable_topic_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    {
        #{
            name => Name,
            error_code => ErrorCode
        },
        Bin2
    }.

-spec encode_create_topics_response_1(create_topics_response_1()) -> iodata().

encode_create_topics_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Results for each topic we tried to create.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Topics, fun encode_creatable_topic_result_1/1)
    ];
encode_create_topics_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        topics => {array, creatable_topic_result_1}
    }).

-spec decode_create_topics_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_response_1(),
    Rest :: binary().

decode_create_topics_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_creatable_topic_result_1)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_creatable_topic_result_1(creatable_topic_result_1()) -> iodata().

encode_creatable_topic_result_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_creatable_topic_result_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_creatable_topic_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_result_1(),
    Rest :: binary().

decode_creatable_topic_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    {
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    }.

-spec encode_create_topics_response_2(create_topics_response_2()) -> iodata().

encode_create_topics_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Results for each topic we tried to create.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_creatable_topic_result_2/1)
    ];
encode_create_topics_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, creatable_topic_result_2}
    }).

-spec decode_create_topics_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_response_2(),
    Rest :: binary().

decode_create_topics_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_creatable_topic_result_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_creatable_topic_result_2(creatable_topic_result_2()) -> iodata().

encode_creatable_topic_result_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_creatable_topic_result_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_creatable_topic_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_result_2(),
    Rest :: binary().

decode_creatable_topic_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    {
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    }.

-spec encode_create_topics_response_3(create_topics_response_3()) -> iodata().

encode_create_topics_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Results for each topic we tried to create.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_creatable_topic_result_3/1)
    ];
encode_create_topics_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, creatable_topic_result_3}
    }).

-spec decode_create_topics_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_response_3(),
    Rest :: binary().

decode_create_topics_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_creatable_topic_result_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_creatable_topic_result_3(creatable_topic_result_3()) -> iodata().

encode_creatable_topic_result_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_creatable_topic_result_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_creatable_topic_result_3(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_result_3(),
    Rest :: binary().

decode_creatable_topic_result_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    {
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    }.

-spec encode_create_topics_response_4(create_topics_response_4()) -> iodata().

encode_create_topics_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Results for each topic we tried to create.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Topics, fun encode_creatable_topic_result_4/1)
    ];
encode_create_topics_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, creatable_topic_result_4}
    }).

-spec decode_create_topics_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_response_4(),
    Rest :: binary().

decode_create_topics_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_creatable_topic_result_4)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    }.

-spec encode_creatable_topic_result_4(creatable_topic_result_4()) -> iodata().

encode_creatable_topic_result_4(
    _Args = #{
        % The topic name.
        name := Name,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_creatable_topic_result_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_creatable_topic_result_4(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_result_4(),
    Rest :: binary().

decode_creatable_topic_result_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    {
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    }.

-spec encode_create_topics_response_5(create_topics_response_5()) -> iodata().

encode_create_topics_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Results for each topic we tried to create.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_creatable_topic_result_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_topics_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, creatable_topic_result_5}
    }).

-spec decode_create_topics_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_response_5(),
    Rest :: binary().

decode_create_topics_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_creatable_topic_result_5)),
    ?decode_tagged_fields(
        fun decode_create_topics_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_create_topics_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: create_topics_response_5().

decode_create_topics_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_configs_5(creatable_topic_configs_5()) -> iodata().

encode_creatable_topic_configs_5(
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
        is_sensitive := IsSensitive
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_bool(ReadOnly),
    ?is_int8(ConfigSource),
    ?is_bool(IsSensitive)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_nullable_string(Value),
        ?encode_bool(ReadOnly),
        ?encode_int8(ConfigSource),
        ?encode_bool(IsSensitive),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_topic_configs_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        read_only => bool,
        config_source => int8,
        is_sensitive => bool
    }).

-spec decode_creatable_topic_configs_5(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_configs_5(),
    Rest :: binary().

decode_creatable_topic_configs_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_nullable_string(Value, Bin1, Bin2),
    ?_decode_bool(ReadOnly, Bin2, Bin3),
    ?_decode_int8(ConfigSource, Bin3, Bin4),
    ?_decode_bool(IsSensitive, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_creatable_topic_configs_5_tagged_field/3,
        #{
            name => Name,
            value => Value,
            read_only => ReadOnly,
            config_source => ConfigSource,
            is_sensitive => IsSensitive
        },
        Bin5
    ).

-spec decode_creatable_topic_configs_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_configs_5().

decode_creatable_topic_configs_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_result_5(creatable_topic_result_5()) -> iodata().

encode_creatable_topic_result_5(
    Args = #{
        % The topic name.
        name := Name,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % Number of partitions of the topic.
        num_partitions := NumPartitions,
        % Replication factor of the topic.
        replication_factor := ReplicationFactor,
        % Configuration of the topic.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_nullable_array(Configs)
->
    [
        ?encode_compact_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_compact_nullable_array(Configs, fun encode_creatable_topic_configs_5/1),
        ?encode_tagged_fields(
            fun encode_creatable_topic_result_5_tagged_field/2,
            Args
        )
    ];
encode_creatable_topic_result_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string,
        num_partitions => int32,
        replication_factor => int16,
        configs => {nullable_array, creatable_topic_configs_5}
    }).

-spec encode_creatable_topic_result_5_tagged_field(
    Key :: atom(), Value :: integer()
) -> {non_neg_integer(), iodata()} | ignore.

encode_creatable_topic_result_5_tagged_field(_Key = topic_config_error_code, TopicConfigErrorCode) ->
    {0, ?encode_int16(TopicConfigErrorCode)};
encode_creatable_topic_result_5_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_creatable_topic_result_5(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_result_5(),
    Rest :: binary().

decode_creatable_topic_result_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_int32(NumPartitions, Bin3, Bin4),
    ?_decode_int16(ReplicationFactor, Bin4, Bin5),
    ?_decode_compact_nullable_array(Configs, Bin5, Bin6, ?_decode_element(decode_creatable_topic_configs_5)),
    ?decode_tagged_fields(
        fun decode_creatable_topic_result_5_tagged_field/3,
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            configs => Configs
        },
        Bin6
    ).

-spec decode_creatable_topic_result_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_result_5().

%% TopicConfigErrorCode
%% Optional topic config error returned if configs are not returned in the response.
decode_creatable_topic_result_5_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_int16(TopicConfigErrorCode, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{topic_config_error_code => TopicConfigErrorCode};
decode_creatable_topic_result_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_topics_response_6(create_topics_response_6()) -> iodata().

encode_create_topics_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Results for each topic we tried to create.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_creatable_topic_result_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_topics_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, creatable_topic_result_6}
    }).

-spec decode_create_topics_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_response_6(),
    Rest :: binary().

decode_create_topics_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_creatable_topic_result_6)),
    ?decode_tagged_fields(
        fun decode_create_topics_response_6_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_create_topics_response_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: create_topics_response_6().

decode_create_topics_response_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_configs_6(creatable_topic_configs_6()) -> iodata().

encode_creatable_topic_configs_6(
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
        is_sensitive := IsSensitive
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_bool(ReadOnly),
    ?is_int8(ConfigSource),
    ?is_bool(IsSensitive)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_nullable_string(Value),
        ?encode_bool(ReadOnly),
        ?encode_int8(ConfigSource),
        ?encode_bool(IsSensitive),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_topic_configs_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        read_only => bool,
        config_source => int8,
        is_sensitive => bool
    }).

-spec decode_creatable_topic_configs_6(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_configs_6(),
    Rest :: binary().

decode_creatable_topic_configs_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_nullable_string(Value, Bin1, Bin2),
    ?_decode_bool(ReadOnly, Bin2, Bin3),
    ?_decode_int8(ConfigSource, Bin3, Bin4),
    ?_decode_bool(IsSensitive, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_creatable_topic_configs_6_tagged_field/3,
        #{
            name => Name,
            value => Value,
            read_only => ReadOnly,
            config_source => ConfigSource,
            is_sensitive => IsSensitive
        },
        Bin5
    ).

-spec decode_creatable_topic_configs_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_configs_6().

decode_creatable_topic_configs_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_result_6(creatable_topic_result_6()) -> iodata().

encode_creatable_topic_result_6(
    Args = #{
        % The topic name.
        name := Name,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % Number of partitions of the topic.
        num_partitions := NumPartitions,
        % Replication factor of the topic.
        replication_factor := ReplicationFactor,
        % Configuration of the topic.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_nullable_array(Configs)
->
    [
        ?encode_compact_string(Name),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_compact_nullable_array(Configs, fun encode_creatable_topic_configs_6/1),
        ?encode_tagged_fields(
            fun encode_creatable_topic_result_6_tagged_field/2,
            Args
        )
    ];
encode_creatable_topic_result_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        error_code => int16,
        error_message => nullable_string,
        num_partitions => int32,
        replication_factor => int16,
        configs => {nullable_array, creatable_topic_configs_6}
    }).

-spec encode_creatable_topic_result_6_tagged_field(
    Key :: atom(), Value :: integer()
) -> {non_neg_integer(), iodata()} | ignore.

encode_creatable_topic_result_6_tagged_field(_Key = topic_config_error_code, TopicConfigErrorCode) ->
    {0, ?encode_int16(TopicConfigErrorCode)};
encode_creatable_topic_result_6_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_creatable_topic_result_6(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_result_6(),
    Rest :: binary().

decode_creatable_topic_result_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_int32(NumPartitions, Bin3, Bin4),
    ?_decode_int16(ReplicationFactor, Bin4, Bin5),
    ?_decode_compact_nullable_array(Configs, Bin5, Bin6, ?_decode_element(decode_creatable_topic_configs_6)),
    ?decode_tagged_fields(
        fun decode_creatable_topic_result_6_tagged_field/3,
        #{
            name => Name,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            configs => Configs
        },
        Bin6
    ).

-spec decode_creatable_topic_result_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_result_6().

%% TopicConfigErrorCode
%% Optional topic config error returned if configs are not returned in the response.
decode_creatable_topic_result_6_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_int16(TopicConfigErrorCode, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{topic_config_error_code => TopicConfigErrorCode};
decode_creatable_topic_result_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_topics_response_7(create_topics_response_7()) -> iodata().

encode_create_topics_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Results for each topic we tried to create.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Topics, fun encode_creatable_topic_result_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_topics_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        topics => {array, creatable_topic_result_7}
    }).

-spec decode_create_topics_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: create_topics_response_7(),
    Rest :: binary().

decode_create_topics_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_creatable_topic_result_7)),
    ?decode_tagged_fields(
        fun decode_create_topics_response_7_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_create_topics_response_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: create_topics_response_7().

decode_create_topics_response_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_configs_7(creatable_topic_configs_7()) -> iodata().

encode_creatable_topic_configs_7(
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
        is_sensitive := IsSensitive
    }
) when
    ?is_string(Name),
    ?is_nullable_string(Value),
    ?is_bool(ReadOnly),
    ?is_int8(ConfigSource),
    ?is_bool(IsSensitive)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_nullable_string(Value),
        ?encode_bool(ReadOnly),
        ?encode_int8(ConfigSource),
        ?encode_bool(IsSensitive),
        ?EMPTY_TAG_BUFFER
    ];
encode_creatable_topic_configs_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        value => nullable_string,
        read_only => bool,
        config_source => int8,
        is_sensitive => bool
    }).

-spec decode_creatable_topic_configs_7(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_configs_7(),
    Rest :: binary().

decode_creatable_topic_configs_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_nullable_string(Value, Bin1, Bin2),
    ?_decode_bool(ReadOnly, Bin2, Bin3),
    ?_decode_int8(ConfigSource, Bin3, Bin4),
    ?_decode_bool(IsSensitive, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_creatable_topic_configs_7_tagged_field/3,
        #{
            name => Name,
            value => Value,
            read_only => ReadOnly,
            config_source => ConfigSource,
            is_sensitive => IsSensitive
        },
        Bin5
    ).

-spec decode_creatable_topic_configs_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_configs_7().

decode_creatable_topic_configs_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_topic_result_7(creatable_topic_result_7()) -> iodata().

encode_creatable_topic_result_7(
    Args = #{
        % The topic name.
        name := Name,
        % The unique topic ID
        topic_id := TopicId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % Number of partitions of the topic.
        num_partitions := NumPartitions,
        % Replication factor of the topic.
        replication_factor := ReplicationFactor,
        % Configuration of the topic.
        configs := Configs
    }
) when
    ?is_string(Name),
    ?is_uuid(TopicId),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int32(NumPartitions),
    ?is_int16(ReplicationFactor),
    ?is_nullable_array(Configs)
->
    [
        ?encode_compact_string(Name),
        ?encode_uuid(TopicId),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int32(NumPartitions),
        ?encode_int16(ReplicationFactor),
        ?encode_compact_nullable_array(Configs, fun encode_creatable_topic_configs_7/1),
        ?encode_tagged_fields(
            fun encode_creatable_topic_result_7_tagged_field/2,
            Args
        )
    ];
encode_creatable_topic_result_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        topic_id => uuid,
        error_code => int16,
        error_message => nullable_string,
        num_partitions => int32,
        replication_factor => int16,
        configs => {nullable_array, creatable_topic_configs_7}
    }).

-spec encode_creatable_topic_result_7_tagged_field(
    Key :: atom(), Value :: integer()
) -> {non_neg_integer(), iodata()} | ignore.

encode_creatable_topic_result_7_tagged_field(_Key = topic_config_error_code, TopicConfigErrorCode) ->
    {0, ?encode_int16(TopicConfigErrorCode)};
encode_creatable_topic_result_7_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_creatable_topic_result_7(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_topic_result_7(),
    Rest :: binary().

decode_creatable_topic_result_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_uuid(TopicId, Bin1, Bin2),
    ?_decode_int16(ErrorCode, Bin2, Bin3),
    ?_decode_compact_nullable_string(ErrorMessage, Bin3, Bin4),
    ?_decode_int32(NumPartitions, Bin4, Bin5),
    ?_decode_int16(ReplicationFactor, Bin5, Bin6),
    ?_decode_compact_nullable_array(Configs, Bin6, Bin7, ?_decode_element(decode_creatable_topic_configs_7)),
    ?decode_tagged_fields(
        fun decode_creatable_topic_result_7_tagged_field/3,
        #{
            name => Name,
            topic_id => TopicId,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            num_partitions => NumPartitions,
            replication_factor => ReplicationFactor,
            configs => Configs
        },
        Bin7
    ).

-spec decode_creatable_topic_result_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: creatable_topic_result_7().

%% TopicConfigErrorCode
%% Optional topic config error returned if configs are not returned in the response.
decode_creatable_topic_result_7_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_int16(TopicConfigErrorCode, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{topic_config_error_code => TopicConfigErrorCode};
decode_creatable_topic_result_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type create_topics_response_0() :: #{
    correlation_id => integer(),
    topics := list(creatable_topic_result_0())
}.
-type creatable_topic_result_0() :: #{
    name := binary(),
    error_code := integer()
}.
-type create_topics_response_1() :: #{
    correlation_id => integer(),
    topics := list(creatable_topic_result_1())
}.
-type creatable_topic_result_1() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type create_topics_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(creatable_topic_result_2())
}.
-type creatable_topic_result_2() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type create_topics_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(creatable_topic_result_3())
}.
-type creatable_topic_result_3() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type create_topics_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(creatable_topic_result_4())
}.
-type creatable_topic_result_4() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type create_topics_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(creatable_topic_result_5())
}.
-type creatable_topic_configs_5() :: #{
    name := binary(),
    value := binary() | null,
    read_only := boolean(),
    config_source := integer(),
    is_sensitive := boolean()
}.
-type creatable_topic_result_5() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null,
    topic_config_error_code => integer(),
    num_partitions := integer(),
    replication_factor := integer(),
    configs := list(creatable_topic_configs_5()) | null
}.
-type create_topics_response_6() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(creatable_topic_result_6())
}.
-type creatable_topic_configs_6() :: #{
    name := binary(),
    value := binary() | null,
    read_only := boolean(),
    config_source := integer(),
    is_sensitive := boolean()
}.
-type creatable_topic_result_6() :: #{
    name := binary(),
    error_code := integer(),
    error_message := binary() | null,
    topic_config_error_code => integer(),
    num_partitions := integer(),
    replication_factor := integer(),
    configs := list(creatable_topic_configs_6()) | null
}.
-type create_topics_response_7() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    topics := list(creatable_topic_result_7())
}.
-type creatable_topic_configs_7() :: #{
    name := binary(),
    value := binary() | null,
    read_only := boolean(),
    config_source := integer(),
    is_sensitive := boolean()
}.
-type creatable_topic_result_7() :: #{
    name := binary(),
    topic_id := kafcod:uuid(),
    error_code := integer(),
    error_message := binary() | null,
    topic_config_error_code => integer(),
    num_partitions := integer(),
    replication_factor := integer(),
    configs := list(creatable_topic_configs_7()) | null
}.
