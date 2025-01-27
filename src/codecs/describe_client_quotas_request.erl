-module(describe_client_quotas_request).
-export([
    encode_describe_client_quotas_request_0/1,
    decode_describe_client_quotas_request_0/1,
    encode_describe_client_quotas_request_1/1,
    decode_describe_client_quotas_request_1/1
]).
-export_type([
    describe_client_quotas_request_0/0,
    component_data_0/0,
    describe_client_quotas_request_1/0,
    component_data_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_CLIENT_QUOTAS_REQUEST, 48).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_client_quotas_request_0(describe_client_quotas_request_0()) -> iodata().

encode_describe_client_quotas_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Filter components to apply to quota entities.
        components := Components,
        % Whether the match is strict, i.e. should exclude entities with unspecified entity types.
        strict := Strict
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Components),
    ?is_bool(Strict)
->
    [
        ?encode_request_header_1(?DESCRIBE_CLIENT_QUOTAS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Components, fun encode_component_data_0/1),
        ?encode_bool(Strict)
    ];
encode_describe_client_quotas_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        components => {array, component_data_0},
        strict => bool
    }).

-spec decode_describe_client_quotas_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_client_quotas_request_0(),
    Rest :: binary().

decode_describe_client_quotas_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Components, Bin0, Bin1, ?_decode_element(decode_component_data_0)),
    ?_decode_bool(Strict, Bin1, Bin2),
    {
        Header#{
            components => Components,
            strict => Strict
        },
        Bin2
    }.

-spec encode_component_data_0(component_data_0()) -> iodata().

encode_component_data_0(
    _Args = #{
        % The entity type that the filter component applies to.
        entity_type := EntityType,
        % How to match the entity {0 = exact name, 1 = default name, 2 = any specified name}.
        match_type := MatchType,
        % The string to match against, or null if unused for the match type.
        match := Match
    }
) when
    ?is_string(EntityType),
    ?is_int8(MatchType),
    ?is_nullable_string(Match)
->
    [
        ?encode_string(EntityType),
        ?encode_int8(MatchType),
        ?encode_nullable_string(Match)
    ];
encode_component_data_0(Args) ->
    ?encoder_error(Args, #{
        entity_type => string,
        match_type => int8,
        match => nullable_string
    }).

-spec decode_component_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: component_data_0(),
    Rest :: binary().

decode_component_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(EntityType, Bin0, Bin1),
    ?_decode_int8(MatchType, Bin1, Bin2),
    ?_decode_nullable_string(Match, Bin2, Bin3),
    {
        #{
            entity_type => EntityType,
            match_type => MatchType,
            match => Match
        },
        Bin3
    }.

-spec encode_describe_client_quotas_request_1(describe_client_quotas_request_1()) -> iodata().

encode_describe_client_quotas_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Filter components to apply to quota entities.
        components := Components,
        % Whether the match is strict, i.e. should exclude entities with unspecified entity types.
        strict := Strict
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Components),
    ?is_bool(Strict)
->
    [
        ?encode_request_header_2(?DESCRIBE_CLIENT_QUOTAS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_compact_array(Components, fun encode_component_data_1/1),
        ?encode_bool(Strict),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_client_quotas_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        components => {array, component_data_1},
        strict => bool
    }).

-spec decode_describe_client_quotas_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_client_quotas_request_1(),
    Rest :: binary().

decode_describe_client_quotas_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Components, Bin0, Bin1, ?_decode_element(decode_component_data_1)),
    ?_decode_bool(Strict, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_describe_client_quotas_request_1_tagged_field/3,
        Header#{
            components => Components,
            strict => Strict
        },
        Bin2
    ).

-spec decode_describe_client_quotas_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_client_quotas_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_component_data_1(component_data_1()) -> iodata().

encode_component_data_1(
    _Args = #{
        % The entity type that the filter component applies to.
        entity_type := EntityType,
        % How to match the entity {0 = exact name, 1 = default name, 2 = any specified name}.
        match_type := MatchType,
        % The string to match against, or null if unused for the match type.
        match := Match
    }
) when
    ?is_string(EntityType),
    ?is_int8(MatchType),
    ?is_nullable_string(Match)
->
    [
        ?encode_compact_string(EntityType),
        ?encode_int8(MatchType),
        ?encode_compact_nullable_string(Match),
        ?EMPTY_TAG_BUFFER
    ];
encode_component_data_1(Args) ->
    ?encoder_error(Args, #{
        entity_type => string,
        match_type => int8,
        match => nullable_string
    }).

-spec decode_component_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: component_data_1(),
    Rest :: binary().

decode_component_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(EntityType, Bin0, Bin1),
    ?_decode_int8(MatchType, Bin1, Bin2),
    ?_decode_compact_nullable_string(Match, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_component_data_1_tagged_field/3,
        #{
            entity_type => EntityType,
            match_type => MatchType,
            match => Match
        },
        Bin3
    ).

-spec decode_component_data_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_component_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_client_quotas_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    components := list(component_data_0()),
    strict := boolean()
}.
-type component_data_0() :: #{
    entity_type := binary(),
    match_type := integer(),
    match := binary() | null
}.
-type describe_client_quotas_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    components := list(component_data_1()),
    strict := boolean()
}.
-type component_data_1() :: #{
    entity_type := binary(),
    match_type := integer(),
    match := binary() | null
}.
