-module(describe_client_quotas_response).
-export([
    encode_describe_client_quotas_response_0/1,
    decode_describe_client_quotas_response_0/1,
    encode_describe_client_quotas_response_1/1,
    decode_describe_client_quotas_response_1/1
]).
-export_type([
    describe_client_quotas_response_0/0,
    entity_data_0/0,
    value_data_0/0,
    entry_data_0/0,
    describe_client_quotas_response_1/0,
    entity_data_1/0,
    value_data_1/0,
    entry_data_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_client_quotas_response_0(describe_client_quotas_response_0()) -> iodata().

encode_describe_client_quotas_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or `0` if the quota description succeeded.
        error_code := ErrorCode,
        % The error message, or `null` if the quota description succeeded.
        error_message := ErrorMessage,
        % A result entry.
        entries := Entries
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_nullable_array(Entries)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_nullable_array(Entries, fun encode_entry_data_0/1)
    ];
encode_describe_client_quotas_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        entries => {nullable_array, entry_data_0}
    }).

-spec decode_describe_client_quotas_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_client_quotas_response_0(),
    Rest :: binary().

decode_describe_client_quotas_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_nullable_array(Entries, Bin3, Bin4, ?_decode_element(decode_entry_data_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            entries => Entries
        },
        Bin4
    }.

-spec encode_entity_data_0(entity_data_0()) -> iodata().

encode_entity_data_0(
    _Args = #{
        % The entity type.
        entity_type := EntityType,
        % The entity name, or null if the default.
        entity_name := EntityName
    }
) when
    ?is_string(EntityType),
    ?is_nullable_string(EntityName)
->
    [
        ?encode_string(EntityType),
        ?encode_nullable_string(EntityName)
    ];
encode_entity_data_0(Args) ->
    ?encoder_error(Args, #{
        entity_type => string,
        entity_name => nullable_string
    }).

-spec decode_entity_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: entity_data_0(),
    Rest :: binary().

decode_entity_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(EntityType, Bin0, Bin1),
    ?_decode_nullable_string(EntityName, Bin1, Bin2),
    {
        #{
            entity_type => EntityType,
            entity_name => EntityName
        },
        Bin2
    }.

-spec encode_value_data_0(value_data_0()) -> iodata().

encode_value_data_0(
    _Args = #{
        % The quota configuration key.
        key := Key,
        % The quota configuration value.
        value := Value
    }
) when
    ?is_string(Key),
    ?is_float64(Value)
->
    [
        ?encode_string(Key),
        ?encode_float64(Value)
    ];
encode_value_data_0(Args) ->
    ?encoder_error(Args, #{
        key => string,
        value => float64
    }).

-spec decode_value_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: value_data_0(),
    Rest :: binary().

decode_value_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Key, Bin0, Bin1),
    ?_decode_float64(Value, Bin1, Bin2),
    {
        #{
            key => Key,
            value => Value
        },
        Bin2
    }.

-spec encode_entry_data_0(entry_data_0()) -> iodata().

encode_entry_data_0(
    _Args = #{
        % The quota entity description.
        entity := Entity,
        % The quota values for the entity.
        values := Values
    }
) when
    ?is_array(Entity),
    ?is_array(Values)
->
    [
        ?encode_array(Entity, fun encode_entity_data_0/1),
        ?encode_array(Values, fun encode_value_data_0/1)
    ];
encode_entry_data_0(Args) ->
    ?encoder_error(Args, #{
        entity => {array, entity_data_0},
        values => {array, value_data_0}
    }).

-spec decode_entry_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: entry_data_0(),
    Rest :: binary().

decode_entry_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_array(Entity, Bin0, Bin1, ?_decode_element(decode_entity_data_0)),
    ?_decode_array(Values, Bin1, Bin2, ?_decode_element(decode_value_data_0)),
    {
        #{
            entity => Entity,
            values => Values
        },
        Bin2
    }.

-spec encode_describe_client_quotas_response_1(describe_client_quotas_response_1()) -> iodata().

encode_describe_client_quotas_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or `0` if the quota description succeeded.
        error_code := ErrorCode,
        % The error message, or `null` if the quota description succeeded.
        error_message := ErrorMessage,
        % A result entry.
        entries := Entries
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_nullable_array(Entries)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_nullable_array(Entries, fun encode_entry_data_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_client_quotas_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        entries => {nullable_array, entry_data_1}
    }).

-spec decode_describe_client_quotas_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_client_quotas_response_1(),
    Rest :: binary().

decode_describe_client_quotas_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_nullable_array(Entries, Bin3, Bin4, ?_decode_element(decode_entry_data_1)),
    ?decode_tagged_fields(
        fun decode_describe_client_quotas_response_1_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            entries => Entries
        },
        Bin4
    ).

-spec decode_describe_client_quotas_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_client_quotas_response_1().

decode_describe_client_quotas_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_entity_data_1(entity_data_1()) -> iodata().

encode_entity_data_1(
    _Args = #{
        % The entity type.
        entity_type := EntityType,
        % The entity name, or null if the default.
        entity_name := EntityName
    }
) when
    ?is_string(EntityType),
    ?is_nullable_string(EntityName)
->
    [
        ?encode_compact_string(EntityType),
        ?encode_compact_nullable_string(EntityName),
        ?EMPTY_TAG_BUFFER
    ];
encode_entity_data_1(Args) ->
    ?encoder_error(Args, #{
        entity_type => string,
        entity_name => nullable_string
    }).

-spec decode_entity_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: entity_data_1(),
    Rest :: binary().

decode_entity_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(EntityType, Bin0, Bin1),
    ?_decode_compact_nullable_string(EntityName, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_entity_data_1_tagged_field/3,
        #{
            entity_type => EntityType,
            entity_name => EntityName
        },
        Bin2
    ).

-spec decode_entity_data_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: entity_data_1().

decode_entity_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_value_data_1(value_data_1()) -> iodata().

encode_value_data_1(
    _Args = #{
        % The quota configuration key.
        key := Key,
        % The quota configuration value.
        value := Value
    }
) when
    ?is_string(Key),
    ?is_float64(Value)
->
    [
        ?encode_compact_string(Key),
        ?encode_float64(Value),
        ?EMPTY_TAG_BUFFER
    ];
encode_value_data_1(Args) ->
    ?encoder_error(Args, #{
        key => string,
        value => float64
    }).

-spec decode_value_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: value_data_1(),
    Rest :: binary().

decode_value_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Key, Bin0, Bin1),
    ?_decode_float64(Value, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_value_data_1_tagged_field/3,
        #{
            key => Key,
            value => Value
        },
        Bin2
    ).

-spec decode_value_data_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: value_data_1().

decode_value_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_entry_data_1(entry_data_1()) -> iodata().

encode_entry_data_1(
    _Args = #{
        % The quota entity description.
        entity := Entity,
        % The quota values for the entity.
        values := Values
    }
) when
    ?is_array(Entity),
    ?is_array(Values)
->
    [
        ?encode_compact_array(Entity, fun encode_entity_data_1/1),
        ?encode_compact_array(Values, fun encode_value_data_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_entry_data_1(Args) ->
    ?encoder_error(Args, #{
        entity => {array, entity_data_1},
        values => {array, value_data_1}
    }).

-spec decode_entry_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: entry_data_1(),
    Rest :: binary().

decode_entry_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_array(Entity, Bin0, Bin1, ?_decode_element(decode_entity_data_1)),
    ?_decode_compact_array(Values, Bin1, Bin2, ?_decode_element(decode_value_data_1)),
    ?decode_tagged_fields(
        fun decode_entry_data_1_tagged_field/3,
        #{
            entity => Entity,
            values => Values
        },
        Bin2
    ).

-spec decode_entry_data_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: entry_data_1().

decode_entry_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_client_quotas_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    entries := list(entry_data_0()) | null
}.
-type entity_data_0() :: #{
    entity_type := binary(),
    entity_name := binary() | null
}.
-type value_data_0() :: #{
    key := binary(),
    value := number()
}.
-type entry_data_0() :: #{
    entity := list(entity_data_0()),
    values := list(value_data_0())
}.
-type describe_client_quotas_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    entries := list(entry_data_1()) | null
}.
-type entity_data_1() :: #{
    entity_type := binary(),
    entity_name := binary() | null
}.
-type value_data_1() :: #{
    key := binary(),
    value := number()
}.
-type entry_data_1() :: #{
    entity := list(entity_data_1()),
    values := list(value_data_1())
}.
