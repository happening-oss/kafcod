-module(alter_client_quotas_request).
-export([
    encode_alter_client_quotas_request_0/1,
    decode_alter_client_quotas_request_0/1,
    encode_alter_client_quotas_request_1/1,
    decode_alter_client_quotas_request_1/1
]).
-export_type([
    alter_client_quotas_request_0/0,
    entity_data_0/0,
    op_data_0/0,
    entry_data_0/0,
    alter_client_quotas_request_1/0,
    entity_data_1/0,
    op_data_1/0,
    entry_data_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ALTER_CLIENT_QUOTAS_REQUEST, 49).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_alter_client_quotas_request_0(alter_client_quotas_request_0()) -> iodata().

encode_alter_client_quotas_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The quota configuration entries to alter.
        entries := Entries,
        % Whether the alteration should be validated, but not performed.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Entries),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_1(?ALTER_CLIENT_QUOTAS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Entries, fun encode_entry_data_0/1),
        ?encode_bool(ValidateOnly)
    ];
encode_alter_client_quotas_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        entries => {array, entry_data_0},
        validate_only => bool
    }).

-spec decode_alter_client_quotas_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_client_quotas_request_0(),
    Rest :: binary().

decode_alter_client_quotas_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Entries, Bin0, Bin1, ?_decode_element(decode_entry_data_0)),
    ?_decode_bool(ValidateOnly, Bin1, Bin2),
    {
        Header#{
            entries => Entries,
            validate_only => ValidateOnly
        },
        Bin2
    }.

-spec encode_entity_data_0(entity_data_0()) -> iodata().

encode_entity_data_0(
    _Args = #{
        % The entity type.
        entity_type := EntityType,
        % The name of the entity, or null if the default.
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

-spec encode_op_data_0(op_data_0()) -> iodata().

encode_op_data_0(
    _Args = #{
        % The quota configuration key.
        key := Key,
        % The value to set, otherwise ignored if the value is to be removed.
        value := Value,
        % Whether the quota configuration value should be removed, otherwise set.
        remove := Remove
    }
) when
    ?is_string(Key),
    ?is_float64(Value),
    ?is_bool(Remove)
->
    [
        ?encode_string(Key),
        ?encode_float64(Value),
        ?encode_bool(Remove)
    ];
encode_op_data_0(Args) ->
    ?encoder_error(Args, #{
        key => string,
        value => float64,
        remove => bool
    }).

-spec decode_op_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: op_data_0(),
    Rest :: binary().

decode_op_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Key, Bin0, Bin1),
    ?_decode_float64(Value, Bin1, Bin2),
    ?_decode_bool(Remove, Bin2, Bin3),
    {
        #{
            key => Key,
            value => Value,
            remove => Remove
        },
        Bin3
    }.

-spec encode_entry_data_0(entry_data_0()) -> iodata().

encode_entry_data_0(
    _Args = #{
        % The quota entity to alter.
        entity := Entity,
        % An individual quota configuration entry to alter.
        ops := Ops
    }
) when
    ?is_array(Entity),
    ?is_array(Ops)
->
    [
        ?encode_array(Entity, fun encode_entity_data_0/1),
        ?encode_array(Ops, fun encode_op_data_0/1)
    ];
encode_entry_data_0(Args) ->
    ?encoder_error(Args, #{
        entity => {array, entity_data_0},
        ops => {array, op_data_0}
    }).

-spec decode_entry_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: entry_data_0(),
    Rest :: binary().

decode_entry_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_array(Entity, Bin0, Bin1, ?_decode_element(decode_entity_data_0)),
    ?_decode_array(Ops, Bin1, Bin2, ?_decode_element(decode_op_data_0)),
    {
        #{
            entity => Entity,
            ops => Ops
        },
        Bin2
    }.

-spec encode_alter_client_quotas_request_1(alter_client_quotas_request_1()) -> iodata().

encode_alter_client_quotas_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The quota configuration entries to alter.
        entries := Entries,
        % Whether the alteration should be validated, but not performed.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Entries),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_2(?ALTER_CLIENT_QUOTAS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_compact_array(Entries, fun encode_entry_data_1/1),
        ?encode_bool(ValidateOnly),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_client_quotas_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        entries => {array, entry_data_1},
        validate_only => bool
    }).

-spec decode_alter_client_quotas_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_client_quotas_request_1(),
    Rest :: binary().

decode_alter_client_quotas_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Entries, Bin0, Bin1, ?_decode_element(decode_entry_data_1)),
    ?_decode_bool(ValidateOnly, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_alter_client_quotas_request_1_tagged_field/3,
        Header#{
            entries => Entries,
            validate_only => ValidateOnly
        },
        Bin2
    ).

-spec decode_alter_client_quotas_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_client_quotas_request_1().

decode_alter_client_quotas_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_entity_data_1(entity_data_1()) -> iodata().

encode_entity_data_1(
    _Args = #{
        % The entity type.
        entity_type := EntityType,
        % The name of the entity, or null if the default.
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

-spec encode_op_data_1(op_data_1()) -> iodata().

encode_op_data_1(
    _Args = #{
        % The quota configuration key.
        key := Key,
        % The value to set, otherwise ignored if the value is to be removed.
        value := Value,
        % Whether the quota configuration value should be removed, otherwise set.
        remove := Remove
    }
) when
    ?is_string(Key),
    ?is_float64(Value),
    ?is_bool(Remove)
->
    [
        ?encode_compact_string(Key),
        ?encode_float64(Value),
        ?encode_bool(Remove),
        ?EMPTY_TAG_BUFFER
    ];
encode_op_data_1(Args) ->
    ?encoder_error(Args, #{
        key => string,
        value => float64,
        remove => bool
    }).

-spec decode_op_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: op_data_1(),
    Rest :: binary().

decode_op_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Key, Bin0, Bin1),
    ?_decode_float64(Value, Bin1, Bin2),
    ?_decode_bool(Remove, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_op_data_1_tagged_field/3,
        #{
            key => Key,
            value => Value,
            remove => Remove
        },
        Bin3
    ).

-spec decode_op_data_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: op_data_1().

decode_op_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_entry_data_1(entry_data_1()) -> iodata().

encode_entry_data_1(
    _Args = #{
        % The quota entity to alter.
        entity := Entity,
        % An individual quota configuration entry to alter.
        ops := Ops
    }
) when
    ?is_array(Entity),
    ?is_array(Ops)
->
    [
        ?encode_compact_array(Entity, fun encode_entity_data_1/1),
        ?encode_compact_array(Ops, fun encode_op_data_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_entry_data_1(Args) ->
    ?encoder_error(Args, #{
        entity => {array, entity_data_1},
        ops => {array, op_data_1}
    }).

-spec decode_entry_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: entry_data_1(),
    Rest :: binary().

decode_entry_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_array(Entity, Bin0, Bin1, ?_decode_element(decode_entity_data_1)),
    ?_decode_compact_array(Ops, Bin1, Bin2, ?_decode_element(decode_op_data_1)),
    ?decode_tagged_fields(
        fun decode_entry_data_1_tagged_field/3,
        #{
            entity => Entity,
            ops => Ops
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

-type alter_client_quotas_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    entries := list(entry_data_0()),
    validate_only := boolean()
}.
-type entity_data_0() :: #{
    entity_type := binary(),
    entity_name := binary() | null
}.
-type op_data_0() :: #{
    key := binary(),
    value := number(),
    remove := boolean()
}.
-type entry_data_0() :: #{
    entity := list(entity_data_0()),
    ops := list(op_data_0())
}.
-type alter_client_quotas_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    entries := list(entry_data_1()),
    validate_only := boolean()
}.
-type entity_data_1() :: #{
    entity_type := binary(),
    entity_name := binary() | null
}.
-type op_data_1() :: #{
    key := binary(),
    value := number(),
    remove := boolean()
}.
-type entry_data_1() :: #{
    entity := list(entity_data_1()),
    ops := list(op_data_1())
}.
