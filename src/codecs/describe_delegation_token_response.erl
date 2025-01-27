-module(describe_delegation_token_response).
-export([
    encode_describe_delegation_token_response_0/1,
    decode_describe_delegation_token_response_0/1,
    encode_describe_delegation_token_response_1/1,
    decode_describe_delegation_token_response_1/1,
    encode_describe_delegation_token_response_2/1,
    decode_describe_delegation_token_response_2/1,
    encode_describe_delegation_token_response_3/1,
    decode_describe_delegation_token_response_3/1
]).
-export_type([
    describe_delegation_token_response_0/0,
    described_delegation_token_renewer_0/0,
    described_delegation_token_0/0,
    describe_delegation_token_response_1/0,
    described_delegation_token_renewer_1/0,
    described_delegation_token_1/0,
    describe_delegation_token_response_2/0,
    described_delegation_token_renewer_2/0,
    described_delegation_token_2/0,
    describe_delegation_token_response_3/0,
    described_delegation_token_renewer_3/0,
    described_delegation_token_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_delegation_token_response_0(describe_delegation_token_response_0()) -> iodata().

encode_describe_delegation_token_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The tokens.
        tokens := Tokens,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Tokens),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(Tokens, fun encode_described_delegation_token_0/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_describe_delegation_token_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        tokens => {array, described_delegation_token_0},
        throttle_time_ms => int32
    }).

-spec decode_describe_delegation_token_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_response_0(),
    Rest :: binary().

decode_describe_delegation_token_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(Tokens, Bin1, Bin2, ?_decode_element(decode_described_delegation_token_0)),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    {
        Header#{
            error_code => ErrorCode,
            tokens => Tokens,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    }.

-spec encode_described_delegation_token_renewer_0(described_delegation_token_renewer_0()) -> iodata().

encode_described_delegation_token_renewer_0(
    _Args = #{
        % The renewer principal type
        principal_type := PrincipalType,
        % The renewer principal name
        principal_name := PrincipalName
    }
) when
    ?is_string(PrincipalType),
    ?is_string(PrincipalName)
->
    [
        ?encode_string(PrincipalType),
        ?encode_string(PrincipalName)
    ];
encode_described_delegation_token_renewer_0(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_described_delegation_token_renewer_0(binary()) -> {Decoded, Rest} when
    Decoded :: described_delegation_token_renewer_0(),
    Rest :: binary().

decode_described_delegation_token_renewer_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(PrincipalType, Bin0, Bin1),
    ?_decode_string(PrincipalName, Bin1, Bin2),
    {
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    }.

-spec encode_described_delegation_token_0(described_delegation_token_0()) -> iodata().

encode_described_delegation_token_0(
    _Args = #{
        % The token principal type.
        principal_type := PrincipalType,
        % The token principal name.
        principal_name := PrincipalName,
        % The token issue timestamp in milliseconds.
        issue_timestamp := IssueTimestamp,
        % The token expiry timestamp in milliseconds.
        expiry_timestamp := ExpiryTimestamp,
        % The token maximum timestamp length in milliseconds.
        max_timestamp := MaxTimestamp,
        % The token ID.
        token_id := TokenId,
        % The token HMAC.
        hmac := Hmac,
        % Those who are able to renew this token before it expires.
        renewers := Renewers
    }
) when
    ?is_string(PrincipalType),
    ?is_string(PrincipalName),
    ?is_int64(IssueTimestamp),
    ?is_int64(ExpiryTimestamp),
    ?is_int64(MaxTimestamp),
    ?is_string(TokenId),
    ?is_bytes(Hmac),
    ?is_array(Renewers)
->
    [
        ?encode_string(PrincipalType),
        ?encode_string(PrincipalName),
        ?encode_int64(IssueTimestamp),
        ?encode_int64(ExpiryTimestamp),
        ?encode_int64(MaxTimestamp),
        ?encode_string(TokenId),
        ?encode_bytes(Hmac),
        ?encode_array(Renewers, fun encode_described_delegation_token_renewer_0/1)
    ];
encode_described_delegation_token_0(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string,
        issue_timestamp => int64,
        expiry_timestamp => int64,
        max_timestamp => int64,
        token_id => string,
        hmac => bytes,
        renewers => {array, described_delegation_token_renewer_0}
    }).

-spec decode_described_delegation_token_0(binary()) -> {Decoded, Rest} when
    Decoded :: described_delegation_token_0(),
    Rest :: binary().

decode_described_delegation_token_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(PrincipalType, Bin0, Bin1),
    ?_decode_string(PrincipalName, Bin1, Bin2),
    ?_decode_int64(IssueTimestamp, Bin2, Bin3),
    ?_decode_int64(ExpiryTimestamp, Bin3, Bin4),
    ?_decode_int64(MaxTimestamp, Bin4, Bin5),
    ?_decode_string(TokenId, Bin5, Bin6),
    ?_decode_bytes(Hmac, Bin6, Bin7),
    ?_decode_array(Renewers, Bin7, Bin8, ?_decode_element(decode_described_delegation_token_renewer_0)),
    {
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName,
            issue_timestamp => IssueTimestamp,
            expiry_timestamp => ExpiryTimestamp,
            max_timestamp => MaxTimestamp,
            token_id => TokenId,
            hmac => Hmac,
            renewers => Renewers
        },
        Bin8
    }.

-spec encode_describe_delegation_token_response_1(describe_delegation_token_response_1()) -> iodata().

encode_describe_delegation_token_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The tokens.
        tokens := Tokens,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Tokens),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(Tokens, fun encode_described_delegation_token_1/1),
        ?encode_int32(ThrottleTimeMs)
    ];
encode_describe_delegation_token_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        tokens => {array, described_delegation_token_1},
        throttle_time_ms => int32
    }).

-spec decode_describe_delegation_token_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_response_1(),
    Rest :: binary().

decode_describe_delegation_token_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(Tokens, Bin1, Bin2, ?_decode_element(decode_described_delegation_token_1)),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    {
        Header#{
            error_code => ErrorCode,
            tokens => Tokens,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    }.

-spec encode_described_delegation_token_renewer_1(described_delegation_token_renewer_1()) -> iodata().

encode_described_delegation_token_renewer_1(
    _Args = #{
        % The renewer principal type
        principal_type := PrincipalType,
        % The renewer principal name
        principal_name := PrincipalName
    }
) when
    ?is_string(PrincipalType),
    ?is_string(PrincipalName)
->
    [
        ?encode_string(PrincipalType),
        ?encode_string(PrincipalName)
    ];
encode_described_delegation_token_renewer_1(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_described_delegation_token_renewer_1(binary()) -> {Decoded, Rest} when
    Decoded :: described_delegation_token_renewer_1(),
    Rest :: binary().

decode_described_delegation_token_renewer_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(PrincipalType, Bin0, Bin1),
    ?_decode_string(PrincipalName, Bin1, Bin2),
    {
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    }.

-spec encode_described_delegation_token_1(described_delegation_token_1()) -> iodata().

encode_described_delegation_token_1(
    _Args = #{
        % The token principal type.
        principal_type := PrincipalType,
        % The token principal name.
        principal_name := PrincipalName,
        % The token issue timestamp in milliseconds.
        issue_timestamp := IssueTimestamp,
        % The token expiry timestamp in milliseconds.
        expiry_timestamp := ExpiryTimestamp,
        % The token maximum timestamp length in milliseconds.
        max_timestamp := MaxTimestamp,
        % The token ID.
        token_id := TokenId,
        % The token HMAC.
        hmac := Hmac,
        % Those who are able to renew this token before it expires.
        renewers := Renewers
    }
) when
    ?is_string(PrincipalType),
    ?is_string(PrincipalName),
    ?is_int64(IssueTimestamp),
    ?is_int64(ExpiryTimestamp),
    ?is_int64(MaxTimestamp),
    ?is_string(TokenId),
    ?is_bytes(Hmac),
    ?is_array(Renewers)
->
    [
        ?encode_string(PrincipalType),
        ?encode_string(PrincipalName),
        ?encode_int64(IssueTimestamp),
        ?encode_int64(ExpiryTimestamp),
        ?encode_int64(MaxTimestamp),
        ?encode_string(TokenId),
        ?encode_bytes(Hmac),
        ?encode_array(Renewers, fun encode_described_delegation_token_renewer_1/1)
    ];
encode_described_delegation_token_1(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string,
        issue_timestamp => int64,
        expiry_timestamp => int64,
        max_timestamp => int64,
        token_id => string,
        hmac => bytes,
        renewers => {array, described_delegation_token_renewer_1}
    }).

-spec decode_described_delegation_token_1(binary()) -> {Decoded, Rest} when
    Decoded :: described_delegation_token_1(),
    Rest :: binary().

decode_described_delegation_token_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(PrincipalType, Bin0, Bin1),
    ?_decode_string(PrincipalName, Bin1, Bin2),
    ?_decode_int64(IssueTimestamp, Bin2, Bin3),
    ?_decode_int64(ExpiryTimestamp, Bin3, Bin4),
    ?_decode_int64(MaxTimestamp, Bin4, Bin5),
    ?_decode_string(TokenId, Bin5, Bin6),
    ?_decode_bytes(Hmac, Bin6, Bin7),
    ?_decode_array(Renewers, Bin7, Bin8, ?_decode_element(decode_described_delegation_token_renewer_1)),
    {
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName,
            issue_timestamp => IssueTimestamp,
            expiry_timestamp => ExpiryTimestamp,
            max_timestamp => MaxTimestamp,
            token_id => TokenId,
            hmac => Hmac,
            renewers => Renewers
        },
        Bin8
    }.

-spec encode_describe_delegation_token_response_2(describe_delegation_token_response_2()) -> iodata().

encode_describe_delegation_token_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The tokens.
        tokens := Tokens,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Tokens),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Tokens, fun encode_described_delegation_token_2/1),
        ?encode_int32(ThrottleTimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_delegation_token_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        tokens => {array, described_delegation_token_2},
        throttle_time_ms => int32
    }).

-spec decode_describe_delegation_token_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_response_2(),
    Rest :: binary().

decode_describe_delegation_token_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(Tokens, Bin1, Bin2, ?_decode_element(decode_described_delegation_token_2)),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_describe_delegation_token_response_2_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            tokens => Tokens,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    ).

-spec decode_describe_delegation_token_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_delegation_token_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_described_delegation_token_renewer_2(described_delegation_token_renewer_2()) -> iodata().

encode_described_delegation_token_renewer_2(
    _Args = #{
        % The renewer principal type
        principal_type := PrincipalType,
        % The renewer principal name
        principal_name := PrincipalName
    }
) when
    ?is_string(PrincipalType),
    ?is_string(PrincipalName)
->
    [
        ?encode_compact_string(PrincipalType),
        ?encode_compact_string(PrincipalName),
        ?EMPTY_TAG_BUFFER
    ];
encode_described_delegation_token_renewer_2(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_described_delegation_token_renewer_2(binary()) -> {Decoded, Rest} when
    Decoded :: described_delegation_token_renewer_2(),
    Rest :: binary().

decode_described_delegation_token_renewer_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(PrincipalType, Bin0, Bin1),
    ?_decode_compact_string(PrincipalName, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_described_delegation_token_renewer_2_tagged_field/3,
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    ).

-spec decode_described_delegation_token_renewer_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_described_delegation_token_renewer_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_described_delegation_token_2(described_delegation_token_2()) -> iodata().

encode_described_delegation_token_2(
    _Args = #{
        % The token principal type.
        principal_type := PrincipalType,
        % The token principal name.
        principal_name := PrincipalName,
        % The token issue timestamp in milliseconds.
        issue_timestamp := IssueTimestamp,
        % The token expiry timestamp in milliseconds.
        expiry_timestamp := ExpiryTimestamp,
        % The token maximum timestamp length in milliseconds.
        max_timestamp := MaxTimestamp,
        % The token ID.
        token_id := TokenId,
        % The token HMAC.
        hmac := Hmac,
        % Those who are able to renew this token before it expires.
        renewers := Renewers
    }
) when
    ?is_string(PrincipalType),
    ?is_string(PrincipalName),
    ?is_int64(IssueTimestamp),
    ?is_int64(ExpiryTimestamp),
    ?is_int64(MaxTimestamp),
    ?is_string(TokenId),
    ?is_bytes(Hmac),
    ?is_array(Renewers)
->
    [
        ?encode_compact_string(PrincipalType),
        ?encode_compact_string(PrincipalName),
        ?encode_int64(IssueTimestamp),
        ?encode_int64(ExpiryTimestamp),
        ?encode_int64(MaxTimestamp),
        ?encode_compact_string(TokenId),
        ?encode_compact_bytes(Hmac),
        ?encode_compact_array(Renewers, fun encode_described_delegation_token_renewer_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_described_delegation_token_2(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string,
        issue_timestamp => int64,
        expiry_timestamp => int64,
        max_timestamp => int64,
        token_id => string,
        hmac => bytes,
        renewers => {array, described_delegation_token_renewer_2}
    }).

-spec decode_described_delegation_token_2(binary()) -> {Decoded, Rest} when
    Decoded :: described_delegation_token_2(),
    Rest :: binary().

decode_described_delegation_token_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(PrincipalType, Bin0, Bin1),
    ?_decode_compact_string(PrincipalName, Bin1, Bin2),
    ?_decode_int64(IssueTimestamp, Bin2, Bin3),
    ?_decode_int64(ExpiryTimestamp, Bin3, Bin4),
    ?_decode_int64(MaxTimestamp, Bin4, Bin5),
    ?_decode_compact_string(TokenId, Bin5, Bin6),
    ?_decode_compact_bytes(Hmac, Bin6, Bin7),
    ?_decode_compact_array(Renewers, Bin7, Bin8, ?_decode_element(decode_described_delegation_token_renewer_2)),
    ?decode_tagged_fields(
        fun decode_described_delegation_token_2_tagged_field/3,
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName,
            issue_timestamp => IssueTimestamp,
            expiry_timestamp => ExpiryTimestamp,
            max_timestamp => MaxTimestamp,
            token_id => TokenId,
            hmac => Hmac,
            renewers => Renewers
        },
        Bin8
    ).

-spec decode_described_delegation_token_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_described_delegation_token_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_delegation_token_response_3(describe_delegation_token_response_3()) -> iodata().

encode_describe_delegation_token_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The tokens.
        tokens := Tokens,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Tokens),
    ?is_int32(ThrottleTimeMs)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Tokens, fun encode_described_delegation_token_3/1),
        ?encode_int32(ThrottleTimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_delegation_token_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        tokens => {array, described_delegation_token_3},
        throttle_time_ms => int32
    }).

-spec decode_describe_delegation_token_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_response_3(),
    Rest :: binary().

decode_describe_delegation_token_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(Tokens, Bin1, Bin2, ?_decode_element(decode_described_delegation_token_3)),
    ?_decode_int32(ThrottleTimeMs, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_describe_delegation_token_response_3_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            tokens => Tokens,
            throttle_time_ms => ThrottleTimeMs
        },
        Bin3
    ).

-spec decode_describe_delegation_token_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_delegation_token_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_described_delegation_token_renewer_3(described_delegation_token_renewer_3()) -> iodata().

encode_described_delegation_token_renewer_3(
    _Args = #{
        % The renewer principal type
        principal_type := PrincipalType,
        % The renewer principal name
        principal_name := PrincipalName
    }
) when
    ?is_string(PrincipalType),
    ?is_string(PrincipalName)
->
    [
        ?encode_compact_string(PrincipalType),
        ?encode_compact_string(PrincipalName),
        ?EMPTY_TAG_BUFFER
    ];
encode_described_delegation_token_renewer_3(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_described_delegation_token_renewer_3(binary()) -> {Decoded, Rest} when
    Decoded :: described_delegation_token_renewer_3(),
    Rest :: binary().

decode_described_delegation_token_renewer_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(PrincipalType, Bin0, Bin1),
    ?_decode_compact_string(PrincipalName, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_described_delegation_token_renewer_3_tagged_field/3,
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    ).

-spec decode_described_delegation_token_renewer_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_described_delegation_token_renewer_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_described_delegation_token_3(described_delegation_token_3()) -> iodata().

encode_described_delegation_token_3(
    _Args = #{
        % The token principal type.
        principal_type := PrincipalType,
        % The token principal name.
        principal_name := PrincipalName,
        % The principal type of the requester of the token.
        token_requester_principal_type := TokenRequesterPrincipalType,
        % The principal type of the requester of the token.
        token_requester_principal_name := TokenRequesterPrincipalName,
        % The token issue timestamp in milliseconds.
        issue_timestamp := IssueTimestamp,
        % The token expiry timestamp in milliseconds.
        expiry_timestamp := ExpiryTimestamp,
        % The token maximum timestamp length in milliseconds.
        max_timestamp := MaxTimestamp,
        % The token ID.
        token_id := TokenId,
        % The token HMAC.
        hmac := Hmac,
        % Those who are able to renew this token before it expires.
        renewers := Renewers
    }
) when
    ?is_string(PrincipalType),
    ?is_string(PrincipalName),
    ?is_string(TokenRequesterPrincipalType),
    ?is_string(TokenRequesterPrincipalName),
    ?is_int64(IssueTimestamp),
    ?is_int64(ExpiryTimestamp),
    ?is_int64(MaxTimestamp),
    ?is_string(TokenId),
    ?is_bytes(Hmac),
    ?is_array(Renewers)
->
    [
        ?encode_compact_string(PrincipalType),
        ?encode_compact_string(PrincipalName),
        ?encode_compact_string(TokenRequesterPrincipalType),
        ?encode_compact_string(TokenRequesterPrincipalName),
        ?encode_int64(IssueTimestamp),
        ?encode_int64(ExpiryTimestamp),
        ?encode_int64(MaxTimestamp),
        ?encode_compact_string(TokenId),
        ?encode_compact_bytes(Hmac),
        ?encode_compact_array(Renewers, fun encode_described_delegation_token_renewer_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_described_delegation_token_3(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string,
        token_requester_principal_type => string,
        token_requester_principal_name => string,
        issue_timestamp => int64,
        expiry_timestamp => int64,
        max_timestamp => int64,
        token_id => string,
        hmac => bytes,
        renewers => {array, described_delegation_token_renewer_3}
    }).

-spec decode_described_delegation_token_3(binary()) -> {Decoded, Rest} when
    Decoded :: described_delegation_token_3(),
    Rest :: binary().

decode_described_delegation_token_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(PrincipalType, Bin0, Bin1),
    ?_decode_compact_string(PrincipalName, Bin1, Bin2),
    ?_decode_compact_string(TokenRequesterPrincipalType, Bin2, Bin3),
    ?_decode_compact_string(TokenRequesterPrincipalName, Bin3, Bin4),
    ?_decode_int64(IssueTimestamp, Bin4, Bin5),
    ?_decode_int64(ExpiryTimestamp, Bin5, Bin6),
    ?_decode_int64(MaxTimestamp, Bin6, Bin7),
    ?_decode_compact_string(TokenId, Bin7, Bin8),
    ?_decode_compact_bytes(Hmac, Bin8, Bin9),
    ?_decode_compact_array(Renewers, Bin9, Bin10, ?_decode_element(decode_described_delegation_token_renewer_3)),
    ?decode_tagged_fields(
        fun decode_described_delegation_token_3_tagged_field/3,
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName,
            token_requester_principal_type => TokenRequesterPrincipalType,
            token_requester_principal_name => TokenRequesterPrincipalName,
            issue_timestamp => IssueTimestamp,
            expiry_timestamp => ExpiryTimestamp,
            max_timestamp => MaxTimestamp,
            token_id => TokenId,
            hmac => Hmac,
            renewers => Renewers
        },
        Bin10
    ).

-spec decode_described_delegation_token_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_described_delegation_token_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_delegation_token_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    tokens := list(described_delegation_token_0()),
    throttle_time_ms := integer()
}.
-type described_delegation_token_renewer_0() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type described_delegation_token_0() :: #{
    principal_type := binary(),
    principal_name := binary(),
    issue_timestamp := integer(),
    expiry_timestamp := integer(),
    max_timestamp := integer(),
    token_id := binary(),
    hmac := kafcod:bytes(),
    renewers := list(described_delegation_token_renewer_0())
}.
-type describe_delegation_token_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    tokens := list(described_delegation_token_1()),
    throttle_time_ms := integer()
}.
-type described_delegation_token_renewer_1() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type described_delegation_token_1() :: #{
    principal_type := binary(),
    principal_name := binary(),
    issue_timestamp := integer(),
    expiry_timestamp := integer(),
    max_timestamp := integer(),
    token_id := binary(),
    hmac := kafcod:bytes(),
    renewers := list(described_delegation_token_renewer_1())
}.
-type describe_delegation_token_response_2() :: #{
    correlation_id => integer(),
    error_code := integer(),
    tokens := list(described_delegation_token_2()),
    throttle_time_ms := integer()
}.
-type described_delegation_token_renewer_2() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type described_delegation_token_2() :: #{
    principal_type := binary(),
    principal_name := binary(),
    issue_timestamp := integer(),
    expiry_timestamp := integer(),
    max_timestamp := integer(),
    token_id := binary(),
    hmac := kafcod:bytes(),
    renewers := list(described_delegation_token_renewer_2())
}.
-type describe_delegation_token_response_3() :: #{
    correlation_id => integer(),
    error_code := integer(),
    tokens := list(described_delegation_token_3()),
    throttle_time_ms := integer()
}.
-type described_delegation_token_renewer_3() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type described_delegation_token_3() :: #{
    principal_type := binary(),
    principal_name := binary(),
    token_requester_principal_type := binary(),
    token_requester_principal_name := binary(),
    issue_timestamp := integer(),
    expiry_timestamp := integer(),
    max_timestamp := integer(),
    token_id := binary(),
    hmac := kafcod:bytes(),
    renewers := list(described_delegation_token_renewer_3())
}.
