-module(incremental_alter_configs_response).
-export([
    encode_incremental_alter_configs_response_0/1,
    decode_incremental_alter_configs_response_0/1,
    encode_incremental_alter_configs_response_1/1,
    decode_incremental_alter_configs_response_1/1
]).
-export_type([
    incremental_alter_configs_response_0/0,
    alter_configs_resource_response_0/0,
    incremental_alter_configs_response_1/0,
    alter_configs_resource_response_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_incremental_alter_configs_response_0(incremental_alter_configs_response_0()) -> iodata().

encode_incremental_alter_configs_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each resource.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Responses, fun encode_alter_configs_resource_response_0/1)
    ];
encode_incremental_alter_configs_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, alter_configs_resource_response_0}
    }).

-spec decode_incremental_alter_configs_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: incremental_alter_configs_response_0(),
    Rest :: binary().

decode_incremental_alter_configs_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Responses, Bin1, Bin2, ?_decode_element(decode_alter_configs_resource_response_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    }.

-spec encode_alter_configs_resource_response_0(alter_configs_resource_response_0()) -> iodata().

encode_alter_configs_resource_response_0(
    _Args = #{
        % The resource error code.
        error_code := ErrorCode,
        % The resource error message, or null if there was no error.
        error_message := ErrorMessage,
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_string(ResourceName)
    ];
encode_alter_configs_resource_response_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string
    }).

-spec decode_alter_configs_resource_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_configs_resource_response_0(),
    Rest :: binary().

decode_alter_configs_resource_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_string(ResourceName, Bin3, Bin4),
    {
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName
        },
        Bin4
    }.

-spec encode_incremental_alter_configs_response_1(incremental_alter_configs_response_1()) -> iodata().

encode_incremental_alter_configs_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The responses for each resource.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Responses, fun encode_alter_configs_resource_response_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_incremental_alter_configs_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        responses => {array, alter_configs_resource_response_1}
    }).

-spec decode_incremental_alter_configs_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: incremental_alter_configs_response_1(),
    Rest :: binary().

decode_incremental_alter_configs_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Responses, Bin1, Bin2, ?_decode_element(decode_alter_configs_resource_response_1)),
    ?decode_tagged_fields(
        fun decode_incremental_alter_configs_response_1_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            responses => Responses
        },
        Bin2
    ).

-spec decode_incremental_alter_configs_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_incremental_alter_configs_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_configs_resource_response_1(alter_configs_resource_response_1()) -> iodata().

encode_alter_configs_resource_response_1(
    _Args = #{
        % The resource error code.
        error_code := ErrorCode,
        % The resource error message, or null if there was no error.
        error_message := ErrorMessage,
        % The resource type.
        resource_type := ResourceType,
        % The resource name.
        resource_name := ResourceName
    }
) when
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(ResourceType),
    ?is_string(ResourceName)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int8(ResourceType),
        ?encode_compact_string(ResourceName),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_configs_resource_response_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        error_message => nullable_string,
        resource_type => int8,
        resource_name => string
    }).

-spec decode_alter_configs_resource_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_configs_resource_response_1(),
    Rest :: binary().

decode_alter_configs_resource_response_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_nullable_string(ErrorMessage, Bin1, Bin2),
    ?_decode_int8(ResourceType, Bin2, Bin3),
    ?_decode_compact_string(ResourceName, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_alter_configs_resource_response_1_tagged_field/3,
        #{
            error_code => ErrorCode,
            error_message => ErrorMessage,
            resource_type => ResourceType,
            resource_name => ResourceName
        },
        Bin4
    ).

-spec decode_alter_configs_resource_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_configs_resource_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type incremental_alter_configs_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(alter_configs_resource_response_0())
}.
-type alter_configs_resource_response_0() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary()
}.
-type incremental_alter_configs_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    responses := list(alter_configs_resource_response_1())
}.
-type alter_configs_resource_response_1() :: #{
    error_code := integer(),
    error_message := binary() | null,
    resource_type := integer(),
    resource_name := binary()
}.
