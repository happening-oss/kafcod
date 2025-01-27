-module(api_versions_response).
-export([
    encode_api_versions_response_0/1,
    decode_api_versions_response_0/1,
    encode_api_versions_response_1/1,
    decode_api_versions_response_1/1,
    encode_api_versions_response_2/1,
    decode_api_versions_response_2/1,
    encode_api_versions_response_3/1,
    decode_api_versions_response_3/1
]).
-export_type([
    api_versions_response_0/0,
    api_version_0/0,
    api_versions_response_1/0,
    api_version_1/0,
    api_versions_response_2/0,
    api_version_2/0,
    api_versions_response_3/0,
    api_version_3/0,
    supported_feature_key_3/0,
    finalized_feature_key_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_api_versions_response_0(api_versions_response_0()) -> iodata().

encode_api_versions_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code.
        error_code := ErrorCode,
        % The APIs supported by the broker.
        api_keys := ApiKeys
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(ApiKeys)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(ApiKeys, fun encode_api_version_0/1)
    ];
encode_api_versions_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        api_keys => {array, api_version_0}
    }).

-spec decode_api_versions_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: api_versions_response_0(),
    Rest :: binary().

decode_api_versions_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(ApiKeys, Bin1, Bin2, ?_decode_element(decode_api_version_0)),
    {
        Header#{
            error_code => ErrorCode,
            api_keys => ApiKeys
        },
        Bin2
    }.

-spec encode_api_version_0(api_version_0()) -> iodata().

encode_api_version_0(
    _Args = #{
        % The API index.
        api_key := ApiKey,
        % The minimum supported version, inclusive.
        min_version := MinVersion,
        % The maximum supported version, inclusive.
        max_version := MaxVersion
    }
) when
    ?is_int16(ApiKey),
    ?is_int16(MinVersion),
    ?is_int16(MaxVersion)
->
    [
        ?encode_int16(ApiKey),
        ?encode_int16(MinVersion),
        ?encode_int16(MaxVersion)
    ];
encode_api_version_0(Args) ->
    ?encoder_error(Args, #{
        api_key => int16,
        min_version => int16,
        max_version => int16
    }).

-spec decode_api_version_0(binary()) -> {Decoded, Rest} when
    Decoded :: api_version_0(),
    Rest :: binary().

decode_api_version_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ApiKey, Bin0, Bin1),
    ?_decode_int16(MinVersion, Bin1, Bin2),
    ?_decode_int16(MaxVersion, Bin2, Bin3),
    {
        #{
            api_key => ApiKey,
            min_version => MinVersion,
            max_version => MaxVersion
        },
        Bin3
    }.

-spec encode_api_versions_response_1(api_versions_response_1()) -> iodata().

encode_api_versions_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code.
        error_code := ErrorCode,
        % The APIs supported by the broker.
        api_keys := ApiKeys,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(ApiKeys),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(ApiKeys, fun encode_api_version_1/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_api_versions_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        api_keys => {array, api_version_1},
        throttle_time_ms => int32
    }).

-spec decode_api_versions_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: api_versions_response_1(),
    Rest :: binary().

decode_api_versions_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(ApiKeys, Bin1, Bin2, ?_decode_element(decode_api_version_1)),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    {
        Header#{
            error_code => ErrorCode,
            api_keys => ApiKeys,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    }.

-spec encode_api_version_1(api_version_1()) -> iodata().

encode_api_version_1(
    _Args = #{
        % The API index.
        api_key := ApiKey,
        % The minimum supported version, inclusive.
        min_version := MinVersion,
        % The maximum supported version, inclusive.
        max_version := MaxVersion
    }
) when
    ?is_int16(ApiKey),
    ?is_int16(MinVersion),
    ?is_int16(MaxVersion)
->
    [
        ?encode_int16(ApiKey),
        ?encode_int16(MinVersion),
        ?encode_int16(MaxVersion)
    ];
encode_api_version_1(Args) ->
    ?encoder_error(Args, #{
        api_key => int16,
        min_version => int16,
        max_version => int16
    }).

-spec decode_api_version_1(binary()) -> {Decoded, Rest} when
    Decoded :: api_version_1(),
    Rest :: binary().

decode_api_version_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ApiKey, Bin0, Bin1),
    ?_decode_int16(MinVersion, Bin1, Bin2),
    ?_decode_int16(MaxVersion, Bin2, Bin3),
    {
        #{
            api_key => ApiKey,
            min_version => MinVersion,
            max_version => MaxVersion
        },
        Bin3
    }.

-spec encode_api_versions_response_2(api_versions_response_2()) -> iodata().

encode_api_versions_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code.
        error_code := ErrorCode,
        % The APIs supported by the broker.
        api_keys := ApiKeys,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(ApiKeys),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(ApiKeys, fun encode_api_version_2/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_api_versions_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        api_keys => {array, api_version_2},
        throttle_time_ms => int32
    }).

-spec decode_api_versions_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: api_versions_response_2(),
    Rest :: binary().

decode_api_versions_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(ApiKeys, Bin1, Bin2, ?_decode_element(decode_api_version_2)),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    {
        Header#{
            error_code => ErrorCode,
            api_keys => ApiKeys,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    }.

-spec encode_api_version_2(api_version_2()) -> iodata().

encode_api_version_2(
    _Args = #{
        % The API index.
        api_key := ApiKey,
        % The minimum supported version, inclusive.
        min_version := MinVersion,
        % The maximum supported version, inclusive.
        max_version := MaxVersion
    }
) when
    ?is_int16(ApiKey),
    ?is_int16(MinVersion),
    ?is_int16(MaxVersion)
->
    [
        ?encode_int16(ApiKey),
        ?encode_int16(MinVersion),
        ?encode_int16(MaxVersion)
    ];
encode_api_version_2(Args) ->
    ?encoder_error(Args, #{
        api_key => int16,
        min_version => int16,
        max_version => int16
    }).

-spec decode_api_version_2(binary()) -> {Decoded, Rest} when
    Decoded :: api_version_2(),
    Rest :: binary().

decode_api_version_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ApiKey, Bin0, Bin1),
    ?_decode_int16(MinVersion, Bin1, Bin2),
    ?_decode_int16(MaxVersion, Bin2, Bin3),
    {
        #{
            api_key => ApiKey,
            min_version => MinVersion,
            max_version => MaxVersion
        },
        Bin3
    }.

-spec encode_api_versions_response_3(api_versions_response_3()) -> iodata().

encode_api_versions_response_3(
    Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error code.
        error_code := ErrorCode,
        % The APIs supported by the broker.
        api_keys := ApiKeys,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(ApiKeys),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(ApiKeys, fun encode_api_version_3/1),
        ?encode_int32(ThrottleTimeMs),
        ?encode_tagged_fields(
            fun encode_api_versions_response_3_tagged_field/2,
            Args
        )
    ];
encode_api_versions_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        api_keys => {array, api_version_3},
        throttle_time_ms => int32
    }).

-spec encode_api_versions_response_3_tagged_field(Key :: atom(), Value :: term()) -> iodata() | ignore.

encode_api_versions_response_3_tagged_field(_Key = supported_features, SupportedFeatures) ->
    {0, ?encode_compact_array(SupportedFeatures, fun encode_supported_feature_key_3/1)};
encode_api_versions_response_3_tagged_field(_Key = finalized_features_epoch, FinalizedFeaturesEpoch) ->
    {1, ?encode_int64(FinalizedFeaturesEpoch)};
encode_api_versions_response_3_tagged_field(_Key = finalized_features, FinalizedFeatures) ->
    {2, ?encode_compact_array(FinalizedFeatures, fun encode_finalized_feature_key_3/1)};
encode_api_versions_response_3_tagged_field(_Key = zk_migration_ready, ZkMigrationReady) ->
    {3, ?encode_bool(ZkMigrationReady)};
encode_api_versions_response_3_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_api_versions_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: api_versions_response_3(),
    Rest :: binary().

decode_api_versions_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(ApiKeys, Bin1, Bin2, ?_decode_element(decode_api_version_3)),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_api_versions_response_3_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            api_keys => ApiKeys,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    ).

-spec decode_api_versions_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

%% SupportedFeatures
%% Features supported by the broker.
decode_api_versions_response_3_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_compact_array(SupportedFeatures, Bin0, Bin1, ?_decode_element(decode_supported_feature_key_3)),
    <<>> = Bin1,
    Acc#{supported_features => SupportedFeatures};
%% FinalizedFeaturesEpoch
%% The monotonically increasing epoch for the finalized features information. Valid values are >= 0. A value of -1 is special and represents unknown epoch.
decode_api_versions_response_3_tagged_field(_Tag = 1, Bin0, Acc) ->
    ?_decode_int64(FinalizedFeaturesEpoch, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{finalized_features_epoch => FinalizedFeaturesEpoch};
%% FinalizedFeatures
%% List of cluster-wide finalized features. The information is valid only if FinalizedFeaturesEpoch >= 0.
decode_api_versions_response_3_tagged_field(_Tag = 2, Bin0, Acc) ->
    ?_decode_compact_array(FinalizedFeatures, Bin0, Bin1, ?_decode_element(decode_finalized_feature_key_3)),
    <<>> = Bin1,
    Acc#{finalized_features => FinalizedFeatures};
%% ZkMigrationReady
%% Set by a KRaft controller if the required configurations for ZK migration are present
decode_api_versions_response_3_tagged_field(_Tag = 3, Bin0, Acc) ->
    ?_decode_bool(ZkMigrationReady, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{zk_migration_ready => ZkMigrationReady};
decode_api_versions_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_api_version_3(api_version_3()) -> iodata().

encode_api_version_3(
    _Args = #{
        % The API index.
        api_key := ApiKey,
        % The minimum supported version, inclusive.
        min_version := MinVersion,
        % The maximum supported version, inclusive.
        max_version := MaxVersion
    }
) when
    ?is_int16(ApiKey),
    ?is_int16(MinVersion),
    ?is_int16(MaxVersion)
->
    [
        ?encode_int16(ApiKey),
        ?encode_int16(MinVersion),
        ?encode_int16(MaxVersion),
        ?EMPTY_TAG_BUFFER
    ];
encode_api_version_3(Args) ->
    ?encoder_error(Args, #{
        api_key => int16,
        min_version => int16,
        max_version => int16
    }).

-spec decode_api_version_3(binary()) -> {Decoded, Rest} when
    Decoded :: api_version_3(),
    Rest :: binary().

decode_api_version_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ApiKey, Bin0, Bin1),
    ?_decode_int16(MinVersion, Bin1, Bin2),
    ?_decode_int16(MaxVersion, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_api_version_3_tagged_field/3,
        #{
            api_key => ApiKey,
            min_version => MinVersion,
            max_version => MaxVersion
        },
        Bin3
    ).

-spec decode_api_version_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_api_version_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_supported_feature_key_3(supported_feature_key_3()) -> iodata().

encode_supported_feature_key_3(
    _Args = #{
        % The name of the feature.
        name := Name,
        % The minimum supported version for the feature.
        min_version := MinVersion,
        % The maximum supported version for the feature.
        max_version := MaxVersion
    }
) when
    ?is_string(Name),
    ?is_int16(MinVersion),
    ?is_int16(MaxVersion)
->
    [
        ?encode_compact_string(Name),
        ?encode_int16(MinVersion),
        ?encode_int16(MaxVersion),
        ?EMPTY_TAG_BUFFER
    ];
encode_supported_feature_key_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        min_version => int16,
        max_version => int16
    }).

-spec decode_supported_feature_key_3(binary()) -> {Decoded, Rest} when
    Decoded :: supported_feature_key_3(),
    Rest :: binary().

decode_supported_feature_key_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(MinVersion, Bin1, Bin2),
    ?_decode_int16(MaxVersion, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_supported_feature_key_3_tagged_field/3,
        #{
            name => Name,
            min_version => MinVersion,
            max_version => MaxVersion
        },
        Bin3
    ).

-spec decode_supported_feature_key_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_supported_feature_key_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_finalized_feature_key_3(finalized_feature_key_3()) -> iodata().

encode_finalized_feature_key_3(
    _Args = #{
        % The name of the feature.
        name := Name,
        % The cluster-wide finalized max version level for the feature.
        max_version_level := MaxVersionLevel,
        % The cluster-wide finalized min version level for the feature.
        min_version_level := MinVersionLevel
    }
) when
    ?is_string(Name),
    ?is_int16(MaxVersionLevel),
    ?is_int16(MinVersionLevel)
->
    [
        ?encode_compact_string(Name),
        ?encode_int16(MaxVersionLevel),
        ?encode_int16(MinVersionLevel),
        ?EMPTY_TAG_BUFFER
    ];
encode_finalized_feature_key_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        max_version_level => int16,
        min_version_level => int16
    }).

-spec decode_finalized_feature_key_3(binary()) -> {Decoded, Rest} when
    Decoded :: finalized_feature_key_3(),
    Rest :: binary().

decode_finalized_feature_key_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(MaxVersionLevel, Bin1, Bin2),
    ?_decode_int16(MinVersionLevel, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_finalized_feature_key_3_tagged_field/3,
        #{
            name => Name,
            max_version_level => MaxVersionLevel,
            min_version_level => MinVersionLevel
        },
        Bin3
    ).

-spec decode_finalized_feature_key_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_finalized_feature_key_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type api_versions_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    api_keys := list(api_version_0())
}.
-type api_version_0() :: #{
    api_key := integer(),
    min_version := integer(),
    max_version := integer()
}.
-type api_versions_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    api_keys := list(api_version_1()),
    throttle_time_ms := integer()
}.
-type api_version_1() :: #{
    api_key := integer(),
    min_version := integer(),
    max_version := integer()
}.
-type api_versions_response_2() :: #{
    correlation_id => integer(),
    error_code := integer(),
    api_keys := list(api_version_2()),
    throttle_time_ms := integer()
}.
-type api_version_2() :: #{
    api_key := integer(),
    min_version := integer(),
    max_version := integer()
}.
-type api_versions_response_3() :: #{
    correlation_id => integer(),
    error_code := integer(),
    api_keys := list(api_version_3()),
    throttle_time_ms := integer(),
    supported_features := list(supported_feature_key_3()),
    finalized_features_epoch := integer(),
    finalized_features := list(finalized_feature_key_3()),
    zk_migration_ready := boolean()
}.
-type api_version_3() :: #{
    api_key := integer(),
    min_version := integer(),
    max_version := integer()
}.
-type supported_feature_key_3() :: #{
    name := binary(),
    min_version := integer(),
    max_version := integer()
}.
-type finalized_feature_key_3() :: #{
    name := binary(),
    max_version_level := integer(),
    min_version_level := integer()
}.
