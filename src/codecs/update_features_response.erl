-module(update_features_response).
-export([
    encode_update_features_response_0/1,
    decode_update_features_response_0/1,
    encode_update_features_response_1/1,
    decode_update_features_response_1/1
]).
-export_type([
    update_features_response_0/0,
    updatable_feature_result_0/0,
    update_features_response_1/0,
    updatable_feature_result_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_update_features_response_0(update_features_response_0()) -> iodata().

encode_update_features_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top-level error code, or `0` if there was no top-level error.
        error_code := ErrorCode,
        % The top-level error message, or `null` if there was no top-level error.
        error_message := ErrorMessage,
        % Results for each feature update.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(Results)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(Results, fun encode_updatable_feature_result_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_features_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        results => {array, updatable_feature_result_0}
    }).

-spec decode_update_features_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: update_features_response_0(),
    Rest :: binary().

decode_update_features_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(Results, Bin3, Bin4, ?_decode_element(decode_updatable_feature_result_0)),
    ?decode_tagged_fields(
        fun decode_update_features_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            results => Results
        },
        Bin4
    ).

-spec decode_update_features_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_update_features_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_updatable_feature_result_0(updatable_feature_result_0()) -> iodata().

encode_updatable_feature_result_0(
    _Args = #{
        % The name of the finalized feature.
        feature := Feature,
        % The feature update error code or `0` if the feature update succeeded.
        error_code := ErrorCode,
        % The feature update error, or `null` if the feature update succeeded.
        error_message := ErrorMessage
    }
) when
    ?is_string(Feature),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_compact_string(Feature),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_updatable_feature_result_0(Args) ->
    ?encoder_error(Args, #{
        feature => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_updatable_feature_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: updatable_feature_result_0(),
    Rest :: binary().

decode_updatable_feature_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Feature, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_updatable_feature_result_0_tagged_field/3,
        #{
            feature => Feature,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    ).

-spec decode_updatable_feature_result_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_updatable_feature_result_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_features_response_1(update_features_response_1()) -> iodata().

encode_update_features_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top-level error code, or `0` if there was no top-level error.
        error_code := ErrorCode,
        % The top-level error message, or `null` if there was no top-level error.
        error_message := ErrorMessage,
        % Results for each feature update.
        results := Results
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(Results)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(Results, fun encode_updatable_feature_result_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_features_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        results => {array, updatable_feature_result_1}
    }).

-spec decode_update_features_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: update_features_response_1(),
    Rest :: binary().

decode_update_features_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(Results, Bin3, Bin4, ?_decode_element(decode_updatable_feature_result_1)),
    ?decode_tagged_fields(
        fun decode_update_features_response_1_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            results => Results
        },
        Bin4
    ).

-spec decode_update_features_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_update_features_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_updatable_feature_result_1(updatable_feature_result_1()) -> iodata().

encode_updatable_feature_result_1(
    _Args = #{
        % The name of the finalized feature.
        feature := Feature,
        % The feature update error code or `0` if the feature update succeeded.
        error_code := ErrorCode,
        % The feature update error, or `null` if the feature update succeeded.
        error_message := ErrorMessage
    }
) when
    ?is_string(Feature),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_compact_string(Feature),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_updatable_feature_result_1(Args) ->
    ?encoder_error(Args, #{
        feature => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_updatable_feature_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: updatable_feature_result_1(),
    Rest :: binary().

decode_updatable_feature_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Feature, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_updatable_feature_result_1_tagged_field/3,
        #{
            feature => Feature,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    ).

-spec decode_updatable_feature_result_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_updatable_feature_result_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type update_features_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    results := list(updatable_feature_result_0())
}.
-type updatable_feature_result_0() :: #{
    feature := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
-type update_features_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    results := list(updatable_feature_result_1())
}.
-type updatable_feature_result_1() :: #{
    feature := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
