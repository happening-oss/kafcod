-module(create_delegation_token_response).
-export([
    encode_create_delegation_token_response_0/1,
    decode_create_delegation_token_response_0/1,
    encode_create_delegation_token_response_1/1,
    decode_create_delegation_token_response_1/1,
    encode_create_delegation_token_response_2/1,
    decode_create_delegation_token_response_2/1,
    encode_create_delegation_token_response_3/1,
    decode_create_delegation_token_response_3/1
]).
-export_type([
    create_delegation_token_response_0/0,
    create_delegation_token_response_1/0,
    create_delegation_token_response_2/0,
    create_delegation_token_response_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_create_delegation_token_response_0(create_delegation_token_response_0()) -> iodata().

encode_create_delegation_token_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error, or zero if there was no error.
        error_code := ErrorCode,
        % The principal type of the token owner.
        principal_type := PrincipalType,
        % The name of the token owner.
        principal_name := PrincipalName,
        % When this token was generated.
        issue_timestamp_ms := IssueTimestampMs,
        % When this token expires.
        expiry_timestamp_ms := ExpiryTimestampMs,
        % The maximum lifetime of this token.
        max_timestamp_ms := MaxTimestampMs,
        % The token UUID.
        token_id := TokenId,
        % HMAC of the delegation token.
        hmac := Hmac,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_string(PrincipalType),
    ?is_string(PrincipalName),
    ?is_int64(IssueTimestampMs),
    ?is_int64(ExpiryTimestampMs),
    ?is_int64(MaxTimestampMs),
    ?is_string(TokenId),
    ?is_bytes(Hmac),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_string(PrincipalType),
        ?encode_string(PrincipalName),
        ?encode_int64(IssueTimestampMs),
        ?encode_int64(ExpiryTimestampMs),
        ?encode_int64(MaxTimestampMs),
        ?encode_string(TokenId),
        ?encode_bytes(Hmac),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_create_delegation_token_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        principal_type => string,
        principal_name => string,
        issue_timestamp_ms => int64,
        expiry_timestamp_ms => int64,
        max_timestamp_ms => int64,
        token_id => string,
        hmac => bytes,
        throttle_time_ms => int32
    }).

-spec decode_create_delegation_token_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_delegation_token_response_0(),
    Rest :: binary().

decode_create_delegation_token_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(PrincipalType, Bin1, Bin2),
    ?_decode_string(PrincipalName, Bin2, Bin3),
    ?_decode_int64(IssueTimestampMs, Bin3, Bin4),
    ?_decode_int64(ExpiryTimestampMs, Bin4, Bin5),
    ?_decode_int64(MaxTimestampMs, Bin5, Bin6),
    ?_decode_string(TokenId, Bin6, Bin7),
    ?_decode_bytes(Hmac, Bin7, Bin8),
    ?_decode_int32(ThrottleTimeMs, Bin8, Bin9),
    {
        Header#{
            error_code => ErrorCode,
            principal_type => PrincipalType,
            principal_name => PrincipalName,
            issue_timestamp_ms => IssueTimestampMs,
            expiry_timestamp_ms => ExpiryTimestampMs,
            max_timestamp_ms => MaxTimestampMs,
            token_id => TokenId,
            hmac => Hmac,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin9
    }.

-spec encode_create_delegation_token_response_1(create_delegation_token_response_1()) -> iodata().

encode_create_delegation_token_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error, or zero if there was no error.
        error_code := ErrorCode,
        % The principal type of the token owner.
        principal_type := PrincipalType,
        % The name of the token owner.
        principal_name := PrincipalName,
        % When this token was generated.
        issue_timestamp_ms := IssueTimestampMs,
        % When this token expires.
        expiry_timestamp_ms := ExpiryTimestampMs,
        % The maximum lifetime of this token.
        max_timestamp_ms := MaxTimestampMs,
        % The token UUID.
        token_id := TokenId,
        % HMAC of the delegation token.
        hmac := Hmac,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_string(PrincipalType),
    ?is_string(PrincipalName),
    ?is_int64(IssueTimestampMs),
    ?is_int64(ExpiryTimestampMs),
    ?is_int64(MaxTimestampMs),
    ?is_string(TokenId),
    ?is_bytes(Hmac),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_string(PrincipalType),
        ?encode_string(PrincipalName),
        ?encode_int64(IssueTimestampMs),
        ?encode_int64(ExpiryTimestampMs),
        ?encode_int64(MaxTimestampMs),
        ?encode_string(TokenId),
        ?encode_bytes(Hmac),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_create_delegation_token_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        principal_type => string,
        principal_name => string,
        issue_timestamp_ms => int64,
        expiry_timestamp_ms => int64,
        max_timestamp_ms => int64,
        token_id => string,
        hmac => bytes,
        throttle_time_ms => int32
    }).

-spec decode_create_delegation_token_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_delegation_token_response_1(),
    Rest :: binary().

decode_create_delegation_token_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(PrincipalType, Bin1, Bin2),
    ?_decode_string(PrincipalName, Bin2, Bin3),
    ?_decode_int64(IssueTimestampMs, Bin3, Bin4),
    ?_decode_int64(ExpiryTimestampMs, Bin4, Bin5),
    ?_decode_int64(MaxTimestampMs, Bin5, Bin6),
    ?_decode_string(TokenId, Bin6, Bin7),
    ?_decode_bytes(Hmac, Bin7, Bin8),
    ?_decode_int32(ThrottleTimeMs, Bin8, Bin9),
    {
        Header#{
            error_code => ErrorCode,
            principal_type => PrincipalType,
            principal_name => PrincipalName,
            issue_timestamp_ms => IssueTimestampMs,
            expiry_timestamp_ms => ExpiryTimestampMs,
            max_timestamp_ms => MaxTimestampMs,
            token_id => TokenId,
            hmac => Hmac,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin9
    }.

-spec encode_create_delegation_token_response_2(create_delegation_token_response_2()) -> iodata().

encode_create_delegation_token_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error, or zero if there was no error.
        error_code := ErrorCode,
        % The principal type of the token owner.
        principal_type := PrincipalType,
        % The name of the token owner.
        principal_name := PrincipalName,
        % When this token was generated.
        issue_timestamp_ms := IssueTimestampMs,
        % When this token expires.
        expiry_timestamp_ms := ExpiryTimestampMs,
        % The maximum lifetime of this token.
        max_timestamp_ms := MaxTimestampMs,
        % The token UUID.
        token_id := TokenId,
        % HMAC of the delegation token.
        hmac := Hmac,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_string(PrincipalType),
    ?is_string(PrincipalName),
    ?is_int64(IssueTimestampMs),
    ?is_int64(ExpiryTimestampMs),
    ?is_int64(MaxTimestampMs),
    ?is_string(TokenId),
    ?is_bytes(Hmac),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_string(PrincipalType),
        ?encode_compact_string(PrincipalName),
        ?encode_int64(IssueTimestampMs),
        ?encode_int64(ExpiryTimestampMs),
        ?encode_int64(MaxTimestampMs),
        ?encode_compact_string(TokenId),
        ?encode_compact_bytes(Hmac),
        ?encode_int32(ThrottleTimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_delegation_token_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        principal_type => string,
        principal_name => string,
        issue_timestamp_ms => int64,
        expiry_timestamp_ms => int64,
        max_timestamp_ms => int64,
        token_id => string,
        hmac => bytes,
        throttle_time_ms => int32
    }).

-spec decode_create_delegation_token_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_delegation_token_response_2(),
    Rest :: binary().

decode_create_delegation_token_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(PrincipalType, Bin1, Bin2),
    ?_decode_compact_string(PrincipalName, Bin2, Bin3),
    ?_decode_int64(IssueTimestampMs, Bin3, Bin4),
    ?_decode_int64(ExpiryTimestampMs, Bin4, Bin5),
    ?_decode_int64(MaxTimestampMs, Bin5, Bin6),
    ?_decode_compact_string(TokenId, Bin6, Bin7),
    ?_decode_compact_bytes(Hmac, Bin7, Bin8),
    ?_decode_int32(ThrottleTimeMs, Bin8, Bin9),
    ?decode_tagged_fields(
        fun decode_create_delegation_token_response_2_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            principal_type => PrincipalType,
            principal_name => PrincipalName,
            issue_timestamp_ms => IssueTimestampMs,
            expiry_timestamp_ms => ExpiryTimestampMs,
            max_timestamp_ms => MaxTimestampMs,
            token_id => TokenId,
            hmac => Hmac,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin9
    ).

-spec decode_create_delegation_token_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_delegation_token_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_delegation_token_response_3(create_delegation_token_response_3()) -> iodata().

encode_create_delegation_token_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top-level error, or zero if there was no error.
        error_code := ErrorCode,
        % The principal type of the token owner.
        principal_type := PrincipalType,
        % The name of the token owner.
        principal_name := PrincipalName,
        % The principal type of the requester of the token.
        token_requester_principal_type := TokenRequesterPrincipalType,
        % The principal type of the requester of the token.
        token_requester_principal_name := TokenRequesterPrincipalName,
        % When this token was generated.
        issue_timestamp_ms := IssueTimestampMs,
        % When this token expires.
        expiry_timestamp_ms := ExpiryTimestampMs,
        % The maximum lifetime of this token.
        max_timestamp_ms := MaxTimestampMs,
        % The token UUID.
        token_id := TokenId,
        % HMAC of the delegation token.
        hmac := Hmac,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_string(PrincipalType),
    ?is_string(PrincipalName),
    ?is_string(TokenRequesterPrincipalType),
    ?is_string(TokenRequesterPrincipalName),
    ?is_int64(IssueTimestampMs),
    ?is_int64(ExpiryTimestampMs),
    ?is_int64(MaxTimestampMs),
    ?is_string(TokenId),
    ?is_bytes(Hmac),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_string(PrincipalType),
        ?encode_compact_string(PrincipalName),
        ?encode_compact_string(TokenRequesterPrincipalType),
        ?encode_compact_string(TokenRequesterPrincipalName),
        ?encode_int64(IssueTimestampMs),
        ?encode_int64(ExpiryTimestampMs),
        ?encode_int64(MaxTimestampMs),
        ?encode_compact_string(TokenId),
        ?encode_compact_bytes(Hmac),
        ?encode_int32(ThrottleTimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_delegation_token_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        principal_type => string,
        principal_name => string,
        token_requester_principal_type => string,
        token_requester_principal_name => string,
        issue_timestamp_ms => int64,
        expiry_timestamp_ms => int64,
        max_timestamp_ms => int64,
        token_id => string,
        hmac => bytes,
        throttle_time_ms => int32
    }).

-spec decode_create_delegation_token_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_delegation_token_response_3(),
    Rest :: binary().

decode_create_delegation_token_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(PrincipalType, Bin1, Bin2),
    ?_decode_compact_string(PrincipalName, Bin2, Bin3),
    ?_decode_compact_string(TokenRequesterPrincipalType, Bin3, Bin4),
    ?_decode_compact_string(TokenRequesterPrincipalName, Bin4, Bin5),
    ?_decode_int64(IssueTimestampMs, Bin5, Bin6),
    ?_decode_int64(ExpiryTimestampMs, Bin6, Bin7),
    ?_decode_int64(MaxTimestampMs, Bin7, Bin8),
    ?_decode_compact_string(TokenId, Bin8, Bin9),
    ?_decode_compact_bytes(Hmac, Bin9, Bin10),
    ?_decode_int32(ThrottleTimeMs, Bin10, Bin11),
    ?decode_tagged_fields(
        fun decode_create_delegation_token_response_3_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            principal_type => PrincipalType,
            principal_name => PrincipalName,
            token_requester_principal_type => TokenRequesterPrincipalType,
            token_requester_principal_name => TokenRequesterPrincipalName,
            issue_timestamp_ms => IssueTimestampMs,
            expiry_timestamp_ms => ExpiryTimestampMs,
            max_timestamp_ms => MaxTimestampMs,
            token_id => TokenId,
            hmac => Hmac,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin11
    ).

-spec decode_create_delegation_token_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_delegation_token_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type create_delegation_token_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    principal_type := binary(),
    principal_name := binary(),
    issue_timestamp_ms := integer(),
    expiry_timestamp_ms := integer(),
    max_timestamp_ms := integer(),
    token_id := binary(),
    hmac := kafcod:bytes(),
    throttle_time_ms := integer()
}.
-type create_delegation_token_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    principal_type := binary(),
    principal_name := binary(),
    issue_timestamp_ms := integer(),
    expiry_timestamp_ms := integer(),
    max_timestamp_ms := integer(),
    token_id := binary(),
    hmac := kafcod:bytes(),
    throttle_time_ms := integer()
}.
-type create_delegation_token_response_2() :: #{
    correlation_id => integer(),
    error_code := integer(),
    principal_type := binary(),
    principal_name := binary(),
    issue_timestamp_ms := integer(),
    expiry_timestamp_ms := integer(),
    max_timestamp_ms := integer(),
    token_id := binary(),
    hmac := kafcod:bytes(),
    throttle_time_ms := integer()
}.
-type create_delegation_token_response_3() :: #{
    correlation_id => integer(),
    error_code := integer(),
    principal_type := binary(),
    principal_name := binary(),
    token_requester_principal_type := binary(),
    token_requester_principal_name := binary(),
    issue_timestamp_ms := integer(),
    expiry_timestamp_ms := integer(),
    max_timestamp_ms := integer(),
    token_id := binary(),
    hmac := kafcod:bytes(),
    throttle_time_ms := integer()
}.
