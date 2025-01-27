-module(expire_delegation_token_request).
-export([
    encode_expire_delegation_token_request_0/1,
    decode_expire_delegation_token_request_0/1,
    encode_expire_delegation_token_request_1/1,
    decode_expire_delegation_token_request_1/1,
    encode_expire_delegation_token_request_2/1,
    decode_expire_delegation_token_request_2/1
]).
-export_type([
    expire_delegation_token_request_0/0,
    expire_delegation_token_request_1/0,
    expire_delegation_token_request_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EXPIRE_DELEGATION_TOKEN_REQUEST, 40).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_expire_delegation_token_request_0(expire_delegation_token_request_0()) -> iodata().

encode_expire_delegation_token_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The HMAC of the delegation token to be expired.
        hmac := Hmac,
        % The expiry time period in milliseconds.
        expiry_time_period_ms := ExpiryTimePeriodMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bytes(Hmac),
    ?is_int64(ExpiryTimePeriodMs)
->
    [
        ?encode_request_header_1(?EXPIRE_DELEGATION_TOKEN_REQUEST, 0, CorrelationId, ClientId),
        ?encode_bytes(Hmac),
        ?encode_int64(ExpiryTimePeriodMs)
    ];
encode_expire_delegation_token_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        hmac => bytes,
        expiry_time_period_ms => int64
    }).

-spec decode_expire_delegation_token_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: expire_delegation_token_request_0(),
    Rest :: binary().

decode_expire_delegation_token_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_bytes(Hmac, Bin0, Bin1),
    ?_decode_int64(ExpiryTimePeriodMs, Bin1, Bin2),
    {
        Header#{
            hmac => Hmac,
            expiry_time_period_ms => ExpiryTimePeriodMs
        },
        Bin2
    }.

-spec encode_expire_delegation_token_request_1(expire_delegation_token_request_1()) -> iodata().

encode_expire_delegation_token_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The HMAC of the delegation token to be expired.
        hmac := Hmac,
        % The expiry time period in milliseconds.
        expiry_time_period_ms := ExpiryTimePeriodMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bytes(Hmac),
    ?is_int64(ExpiryTimePeriodMs)
->
    [
        ?encode_request_header_1(?EXPIRE_DELEGATION_TOKEN_REQUEST, 1, CorrelationId, ClientId),
        ?encode_bytes(Hmac),
        ?encode_int64(ExpiryTimePeriodMs)
    ];
encode_expire_delegation_token_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        hmac => bytes,
        expiry_time_period_ms => int64
    }).

-spec decode_expire_delegation_token_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: expire_delegation_token_request_1(),
    Rest :: binary().

decode_expire_delegation_token_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_bytes(Hmac, Bin0, Bin1),
    ?_decode_int64(ExpiryTimePeriodMs, Bin1, Bin2),
    {
        Header#{
            hmac => Hmac,
            expiry_time_period_ms => ExpiryTimePeriodMs
        },
        Bin2
    }.

-spec encode_expire_delegation_token_request_2(expire_delegation_token_request_2()) -> iodata().

encode_expire_delegation_token_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The HMAC of the delegation token to be expired.
        hmac := Hmac,
        % The expiry time period in milliseconds.
        expiry_time_period_ms := ExpiryTimePeriodMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bytes(Hmac),
    ?is_int64(ExpiryTimePeriodMs)
->
    [
        ?encode_request_header_2(?EXPIRE_DELEGATION_TOKEN_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_bytes(Hmac),
        ?encode_int64(ExpiryTimePeriodMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_expire_delegation_token_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        hmac => bytes,
        expiry_time_period_ms => int64
    }).

-spec decode_expire_delegation_token_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: expire_delegation_token_request_2(),
    Rest :: binary().

decode_expire_delegation_token_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_bytes(Hmac, Bin0, Bin1),
    ?_decode_int64(ExpiryTimePeriodMs, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_expire_delegation_token_request_2_tagged_field/3,
        Header#{
            hmac => Hmac,
            expiry_time_period_ms => ExpiryTimePeriodMs
        },
        Bin2
    ).

-spec decode_expire_delegation_token_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_expire_delegation_token_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type expire_delegation_token_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    hmac := kafcod:bytes(),
    expiry_time_period_ms := integer()
}.
-type expire_delegation_token_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    hmac := kafcod:bytes(),
    expiry_time_period_ms := integer()
}.
-type expire_delegation_token_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    hmac := kafcod:bytes(),
    expiry_time_period_ms := integer()
}.
