-module(describe_user_scram_credentials_response).
-export([
    encode_describe_user_scram_credentials_response_0/1,
    decode_describe_user_scram_credentials_response_0/1
]).
-export_type([
    describe_user_scram_credentials_response_0/0,
    credential_info_0/0,
    describe_user_scram_credentials_result_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_user_scram_credentials_response_0(describe_user_scram_credentials_response_0()) -> iodata().

encode_describe_user_scram_credentials_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The message-level error code, 0 except for user authorization or infrastructure issues.
        error_code := ErrorCode,
        % The message-level error message, if any.
        error_message := ErrorMessage,
        % The results for descriptions, one per user.
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
        ?encode_compact_array(Results, fun encode_describe_user_scram_credentials_result_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_user_scram_credentials_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        results => {array, describe_user_scram_credentials_result_0}
    }).

-spec decode_describe_user_scram_credentials_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_user_scram_credentials_response_0(),
    Rest :: binary().

decode_describe_user_scram_credentials_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(Results, Bin3, Bin4, ?_decode_element(decode_describe_user_scram_credentials_result_0)),
    ?decode_tagged_fields(
        fun decode_describe_user_scram_credentials_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            results => Results
        },
        Bin4
    ).

-spec decode_describe_user_scram_credentials_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_user_scram_credentials_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_credential_info_0(credential_info_0()) -> iodata().

encode_credential_info_0(
    _Args = #{
        % The SCRAM mechanism.
        mechanism := Mechanism,
        % The number of iterations used in the SCRAM credential.
        iterations := Iterations
    }
) when
    ?is_int8(Mechanism),
    ?is_int32(Iterations)
->
    [
        ?encode_int8(Mechanism),
        ?encode_int32(Iterations),
        ?EMPTY_TAG_BUFFER
    ];
encode_credential_info_0(Args) ->
    ?encoder_error(Args, #{
        mechanism => int8,
        iterations => int32
    }).

-spec decode_credential_info_0(binary()) -> {Decoded, Rest} when
    Decoded :: credential_info_0(),
    Rest :: binary().

decode_credential_info_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int8(Mechanism, Bin0, Bin1),
    ?_decode_int32(Iterations, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_credential_info_0_tagged_field/3,
        #{
            mechanism => Mechanism,
            iterations => Iterations
        },
        Bin2
    ).

-spec decode_credential_info_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_credential_info_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_user_scram_credentials_result_0(describe_user_scram_credentials_result_0()) -> iodata().

encode_describe_user_scram_credentials_result_0(
    _Args = #{
        % The user name.
        user := User,
        % The user-level error code.
        error_code := ErrorCode,
        % The user-level error message, if any.
        error_message := ErrorMessage,
        % The mechanism and related information associated with the user's SCRAM credentials.
        credential_infos := CredentialInfos
    }
) when
    ?is_string(User),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(CredentialInfos)
->
    [
        ?encode_compact_string(User),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(CredentialInfos, fun encode_credential_info_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_user_scram_credentials_result_0(Args) ->
    ?encoder_error(Args, #{
        user => string,
        error_code => int16,
        error_message => nullable_string,
        credential_infos => {array, credential_info_0}
    }).

-spec decode_describe_user_scram_credentials_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_user_scram_credentials_result_0(),
    Rest :: binary().

decode_describe_user_scram_credentials_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(User, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(CredentialInfos, Bin3, Bin4, ?_decode_element(decode_credential_info_0)),
    ?decode_tagged_fields(
        fun decode_describe_user_scram_credentials_result_0_tagged_field/3,
        #{
            user => User,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            credential_infos => CredentialInfos
        },
        Bin4
    ).

-spec decode_describe_user_scram_credentials_result_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_user_scram_credentials_result_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_user_scram_credentials_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    results := list(describe_user_scram_credentials_result_0())
}.
-type credential_info_0() :: #{
    mechanism := integer(),
    iterations := integer()
}.
-type describe_user_scram_credentials_result_0() :: #{
    user := binary(),
    error_code := integer(),
    error_message := binary() | null,
    credential_infos := list(credential_info_0())
}.
