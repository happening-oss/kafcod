-module(alter_user_scram_credentials_response).
-export([
    encode_alter_user_scram_credentials_response_0/1,
    decode_alter_user_scram_credentials_response_0/1
]).
-export_type([
    alter_user_scram_credentials_response_0/0,
    alter_user_scram_credentials_result_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_alter_user_scram_credentials_response_0(alter_user_scram_credentials_response_0()) -> iodata().

encode_alter_user_scram_credentials_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The results for deletions and alterations, one per affected user.
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
        ?encode_compact_array(Results, fun encode_alter_user_scram_credentials_result_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_user_scram_credentials_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        results => {array, alter_user_scram_credentials_result_0}
    }).

-spec decode_alter_user_scram_credentials_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_user_scram_credentials_response_0(),
    Rest :: binary().

decode_alter_user_scram_credentials_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Results, Bin1, Bin2, ?_decode_element(decode_alter_user_scram_credentials_result_0)),
    ?decode_tagged_fields(
        fun decode_alter_user_scram_credentials_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            results => Results
        },
        Bin2
    ).

-spec decode_alter_user_scram_credentials_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_user_scram_credentials_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_user_scram_credentials_result_0(alter_user_scram_credentials_result_0()) -> iodata().

encode_alter_user_scram_credentials_result_0(
    _Args = #{
        % The user name.
        user := User,
        % The error code.
        error_code := ErrorCode,
        % The error message, if any.
        error_message := ErrorMessage
    }
) when
    ?is_string(User),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_compact_string(User),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_user_scram_credentials_result_0(Args) ->
    ?encoder_error(Args, #{
        user => string,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_alter_user_scram_credentials_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_user_scram_credentials_result_0(),
    Rest :: binary().

decode_alter_user_scram_credentials_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(User, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_alter_user_scram_credentials_result_0_tagged_field/3,
        #{
            user => User,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    ).

-spec decode_alter_user_scram_credentials_result_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_user_scram_credentials_result_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type alter_user_scram_credentials_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    results := list(alter_user_scram_credentials_result_0())
}.
-type alter_user_scram_credentials_result_0() :: #{
    user := binary(),
    error_code := integer(),
    error_message := binary() | null
}.
