-module(update_features_request).
-export([
    encode_update_features_request_0/1,
    decode_update_features_request_0/1,
    encode_update_features_request_1/1,
    decode_update_features_request_1/1
]).
-export_type([
    update_features_request_0/0,
    feature_update_key_0/0,
    update_features_request_1/0,
    feature_update_key_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(UPDATE_FEATURES_REQUEST, 57).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_update_features_request_0(update_features_request_0()) -> iodata().

encode_update_features_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % The list of updates to finalized features.
        feature_updates := FeatureUpdates
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(TimeoutMs),
    ?is_array(FeatureUpdates)
->
    [
        ?encode_request_header_2(?UPDATE_FEATURES_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(TimeoutMs),
        ?encode_compact_array(FeatureUpdates, fun encode_feature_update_key_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_features_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        timeout_ms => int32,
        feature_updates => {array, feature_update_key_0}
    }).

-spec decode_update_features_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: update_features_request_0(),
    Rest :: binary().

decode_update_features_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(TimeoutMs, Bin0, Bin1),
    ?_decode_compact_array(FeatureUpdates, Bin1, Bin2, ?_decode_element(decode_feature_update_key_0)),
    ?decode_tagged_fields(
        fun decode_update_features_request_0_tagged_field/3,
        Header#{
            timeout_ms => TimeoutMs,
            feature_updates => FeatureUpdates
        },
        Bin2
    ).

-spec decode_update_features_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_features_request_0().

decode_update_features_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_feature_update_key_0(feature_update_key_0()) -> iodata().

encode_feature_update_key_0(
    _Args = #{
        % The name of the finalized feature to be updated.
        feature := Feature,
        % The new maximum version level for the finalized feature. A value >= 1 is valid. A value < 1, is special, and can be used to request the deletion of the finalized feature.
        max_version_level := MaxVersionLevel,
        % DEPRECATED in version 1 (see DowngradeType). When set to true, the finalized feature version level is allowed to be downgraded/deleted. The downgrade request will fail if the new maximum version level is a value that's not lower than the existing maximum finalized version level.
        allow_downgrade := AllowDowngrade
    }
) when
    ?is_string(Feature),
    ?is_int16(MaxVersionLevel),
    ?is_bool(AllowDowngrade)
->
    [
        ?encode_compact_string(Feature),
        ?encode_int16(MaxVersionLevel),
        ?encode_bool(AllowDowngrade),
        ?EMPTY_TAG_BUFFER
    ];
encode_feature_update_key_0(Args) ->
    ?encoder_error(Args, #{
        feature => string,
        max_version_level => int16,
        allow_downgrade => bool
    }).

-spec decode_feature_update_key_0(binary()) -> {Decoded, Rest} when
    Decoded :: feature_update_key_0(),
    Rest :: binary().

decode_feature_update_key_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Feature, Bin0, Bin1),
    ?_decode_int16(MaxVersionLevel, Bin1, Bin2),
    ?_decode_bool(AllowDowngrade, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_feature_update_key_0_tagged_field/3,
        #{
            feature => Feature,
            max_version_level => MaxVersionLevel,
            allow_downgrade => AllowDowngrade
        },
        Bin3
    ).

-spec decode_feature_update_key_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: feature_update_key_0().

decode_feature_update_key_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_features_request_1(update_features_request_1()) -> iodata().

encode_update_features_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % How long to wait in milliseconds before timing out the request.
        timeout_ms := TimeoutMs,
        % The list of updates to finalized features.
        feature_updates := FeatureUpdates,
        % True if we should validate the request, but not perform the upgrade or downgrade.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(TimeoutMs),
    ?is_array(FeatureUpdates),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_2(?UPDATE_FEATURES_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(TimeoutMs),
        ?encode_compact_array(FeatureUpdates, fun encode_feature_update_key_1/1),
        ?encode_bool(ValidateOnly),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_features_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        timeout_ms => int32,
        feature_updates => {array, feature_update_key_1},
        validate_only => bool
    }).

-spec decode_update_features_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: update_features_request_1(),
    Rest :: binary().

decode_update_features_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(TimeoutMs, Bin0, Bin1),
    ?_decode_compact_array(FeatureUpdates, Bin1, Bin2, ?_decode_element(decode_feature_update_key_1)),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_update_features_request_1_tagged_field/3,
        Header#{
            timeout_ms => TimeoutMs,
            feature_updates => FeatureUpdates,
            validate_only => ValidateOnly
        },
        Bin3
    ).

-spec decode_update_features_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: update_features_request_1().

decode_update_features_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_feature_update_key_1(feature_update_key_1()) -> iodata().

encode_feature_update_key_1(
    _Args = #{
        % The name of the finalized feature to be updated.
        feature := Feature,
        % The new maximum version level for the finalized feature. A value >= 1 is valid. A value < 1, is special, and can be used to request the deletion of the finalized feature.
        max_version_level := MaxVersionLevel,
        % Determine which type of upgrade will be performed: 1 will perform an upgrade only (default), 2 is safe downgrades only (lossless), 3 is unsafe downgrades (lossy).
        upgrade_type := UpgradeType
    }
) when
    ?is_string(Feature),
    ?is_int16(MaxVersionLevel),
    ?is_int8(UpgradeType)
->
    [
        ?encode_compact_string(Feature),
        ?encode_int16(MaxVersionLevel),
        ?encode_int8(UpgradeType),
        ?EMPTY_TAG_BUFFER
    ];
encode_feature_update_key_1(Args) ->
    ?encoder_error(Args, #{
        feature => string,
        max_version_level => int16,
        upgrade_type => int8
    }).

-spec decode_feature_update_key_1(binary()) -> {Decoded, Rest} when
    Decoded :: feature_update_key_1(),
    Rest :: binary().

decode_feature_update_key_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Feature, Bin0, Bin1),
    ?_decode_int16(MaxVersionLevel, Bin1, Bin2),
    ?_decode_int8(UpgradeType, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_feature_update_key_1_tagged_field/3,
        #{
            feature => Feature,
            max_version_level => MaxVersionLevel,
            upgrade_type => UpgradeType
        },
        Bin3
    ).

-spec decode_feature_update_key_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: feature_update_key_1().

decode_feature_update_key_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type update_features_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    timeout_ms := integer(),
    feature_updates := list(feature_update_key_0())
}.
-type feature_update_key_0() :: #{
    feature := binary(),
    max_version_level := integer(),
    allow_downgrade := boolean()
}.
-type update_features_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    timeout_ms := integer(),
    feature_updates := list(feature_update_key_1()),
    validate_only := boolean()
}.
-type feature_update_key_1() :: #{
    feature := binary(),
    max_version_level := integer(),
    upgrade_type := integer()
}.
