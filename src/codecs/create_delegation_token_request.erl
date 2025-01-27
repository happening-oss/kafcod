-module(create_delegation_token_request).
-export([
    encode_create_delegation_token_request_0/1,
    decode_create_delegation_token_request_0/1,
    encode_create_delegation_token_request_1/1,
    decode_create_delegation_token_request_1/1,
    encode_create_delegation_token_request_2/1,
    decode_create_delegation_token_request_2/1,
    encode_create_delegation_token_request_3/1,
    decode_create_delegation_token_request_3/1
]).
-export_type([
    create_delegation_token_request_0/0,
    creatable_renewers_0/0,
    create_delegation_token_request_1/0,
    creatable_renewers_1/0,
    create_delegation_token_request_2/0,
    creatable_renewers_2/0,
    create_delegation_token_request_3/0,
    creatable_renewers_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(CREATE_DELEGATION_TOKEN_REQUEST, 38).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_create_delegation_token_request_0(create_delegation_token_request_0()) -> iodata().

encode_create_delegation_token_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % A list of those who are allowed to renew this token before it expires.
        renewers := Renewers,
        % The maximum lifetime of the token in milliseconds, or -1 to use the server side default.
        max_lifetime_ms := MaxLifetimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Renewers),
    ?is_int64(MaxLifetimeMs)
->
    [
        ?encode_request_header_1(?CREATE_DELEGATION_TOKEN_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Renewers, fun encode_creatable_renewers_0/1),
        ?encode_int64(MaxLifetimeMs)
    ];
encode_create_delegation_token_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        renewers => {array, creatable_renewers_0},
        max_lifetime_ms => int64
    }).

-spec decode_create_delegation_token_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_delegation_token_request_0(),
    Rest :: binary().

decode_create_delegation_token_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Renewers, Bin0, Bin1, ?_decode_element(decode_creatable_renewers_0)),
    ?_decode_int64(MaxLifetimeMs, Bin1, Bin2),
    {
        Header#{
            renewers => Renewers,
            max_lifetime_ms => MaxLifetimeMs
        },
        Bin2
    }.

-spec encode_creatable_renewers_0(creatable_renewers_0()) -> iodata().

encode_creatable_renewers_0(
    _Args = #{
        % The type of the Kafka principal.
        principal_type := PrincipalType,
        % The name of the Kafka principal.
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
encode_creatable_renewers_0(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_creatable_renewers_0(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_renewers_0(),
    Rest :: binary().

decode_creatable_renewers_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(PrincipalType, Bin0, Bin1),
    ?_decode_string(PrincipalName, Bin1, Bin2),
    {
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    }.

-spec encode_create_delegation_token_request_1(create_delegation_token_request_1()) -> iodata().

encode_create_delegation_token_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % A list of those who are allowed to renew this token before it expires.
        renewers := Renewers,
        % The maximum lifetime of the token in milliseconds, or -1 to use the server side default.
        max_lifetime_ms := MaxLifetimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Renewers),
    ?is_int64(MaxLifetimeMs)
->
    [
        ?encode_request_header_1(?CREATE_DELEGATION_TOKEN_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Renewers, fun encode_creatable_renewers_1/1),
        ?encode_int64(MaxLifetimeMs)
    ];
encode_create_delegation_token_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        renewers => {array, creatable_renewers_1},
        max_lifetime_ms => int64
    }).

-spec decode_create_delegation_token_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_delegation_token_request_1(),
    Rest :: binary().

decode_create_delegation_token_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Renewers, Bin0, Bin1, ?_decode_element(decode_creatable_renewers_1)),
    ?_decode_int64(MaxLifetimeMs, Bin1, Bin2),
    {
        Header#{
            renewers => Renewers,
            max_lifetime_ms => MaxLifetimeMs
        },
        Bin2
    }.

-spec encode_creatable_renewers_1(creatable_renewers_1()) -> iodata().

encode_creatable_renewers_1(
    _Args = #{
        % The type of the Kafka principal.
        principal_type := PrincipalType,
        % The name of the Kafka principal.
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
encode_creatable_renewers_1(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_creatable_renewers_1(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_renewers_1(),
    Rest :: binary().

decode_creatable_renewers_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(PrincipalType, Bin0, Bin1),
    ?_decode_string(PrincipalName, Bin1, Bin2),
    {
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    }.

-spec encode_create_delegation_token_request_2(create_delegation_token_request_2()) -> iodata().

encode_create_delegation_token_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % A list of those who are allowed to renew this token before it expires.
        renewers := Renewers,
        % The maximum lifetime of the token in milliseconds, or -1 to use the server side default.
        max_lifetime_ms := MaxLifetimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Renewers),
    ?is_int64(MaxLifetimeMs)
->
    [
        ?encode_request_header_2(?CREATE_DELEGATION_TOKEN_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_array(Renewers, fun encode_creatable_renewers_2/1),
        ?encode_int64(MaxLifetimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_delegation_token_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        renewers => {array, creatable_renewers_2},
        max_lifetime_ms => int64
    }).

-spec decode_create_delegation_token_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_delegation_token_request_2(),
    Rest :: binary().

decode_create_delegation_token_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Renewers, Bin0, Bin1, ?_decode_element(decode_creatable_renewers_2)),
    ?_decode_int64(MaxLifetimeMs, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_create_delegation_token_request_2_tagged_field/3,
        Header#{
            renewers => Renewers,
            max_lifetime_ms => MaxLifetimeMs
        },
        Bin2
    ).

-spec decode_create_delegation_token_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_delegation_token_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_renewers_2(creatable_renewers_2()) -> iodata().

encode_creatable_renewers_2(
    _Args = #{
        % The type of the Kafka principal.
        principal_type := PrincipalType,
        % The name of the Kafka principal.
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
encode_creatable_renewers_2(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_creatable_renewers_2(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_renewers_2(),
    Rest :: binary().

decode_creatable_renewers_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(PrincipalType, Bin0, Bin1),
    ?_decode_compact_string(PrincipalName, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_creatable_renewers_2_tagged_field/3,
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    ).

-spec decode_creatable_renewers_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_creatable_renewers_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_delegation_token_request_3(create_delegation_token_request_3()) -> iodata().

encode_create_delegation_token_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The principal type of the owner of the token. If it's null it defaults to the token request principal.
        owner_principal_type := OwnerPrincipalType,
        % The principal name of the owner of the token. If it's null it defaults to the token request principal.
        owner_principal_name := OwnerPrincipalName,
        % A list of those who are allowed to renew this token before it expires.
        renewers := Renewers,
        % The maximum lifetime of the token in milliseconds, or -1 to use the server side default.
        max_lifetime_ms := MaxLifetimeMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(OwnerPrincipalType),
    ?is_nullable_string(OwnerPrincipalName),
    ?is_array(Renewers),
    ?is_int64(MaxLifetimeMs)
->
    [
        ?encode_request_header_2(?CREATE_DELEGATION_TOKEN_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_nullable_string(OwnerPrincipalType),
        ?encode_compact_nullable_string(OwnerPrincipalName),
        ?encode_compact_array(Renewers, fun encode_creatable_renewers_3/1),
        ?encode_int64(MaxLifetimeMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_delegation_token_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        owner_principal_type => nullable_string,
        owner_principal_name => nullable_string,
        renewers => {array, creatable_renewers_3},
        max_lifetime_ms => int64
    }).

-spec decode_create_delegation_token_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_delegation_token_request_3(),
    Rest :: binary().

decode_create_delegation_token_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(OwnerPrincipalType, Bin0, Bin1),
    ?_decode_compact_nullable_string(OwnerPrincipalName, Bin1, Bin2),
    ?_decode_compact_array(Renewers, Bin2, Bin3, ?_decode_element(decode_creatable_renewers_3)),
    ?_decode_int64(MaxLifetimeMs, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_create_delegation_token_request_3_tagged_field/3,
        Header#{
            owner_principal_type => OwnerPrincipalType,
            owner_principal_name => OwnerPrincipalName,
            renewers => Renewers,
            max_lifetime_ms => MaxLifetimeMs
        },
        Bin4
    ).

-spec decode_create_delegation_token_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_delegation_token_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_creatable_renewers_3(creatable_renewers_3()) -> iodata().

encode_creatable_renewers_3(
    _Args = #{
        % The type of the Kafka principal.
        principal_type := PrincipalType,
        % The name of the Kafka principal.
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
encode_creatable_renewers_3(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_creatable_renewers_3(binary()) -> {Decoded, Rest} when
    Decoded :: creatable_renewers_3(),
    Rest :: binary().

decode_creatable_renewers_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(PrincipalType, Bin0, Bin1),
    ?_decode_compact_string(PrincipalName, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_creatable_renewers_3_tagged_field/3,
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    ).

-spec decode_creatable_renewers_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_creatable_renewers_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type create_delegation_token_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    renewers := list(creatable_renewers_0()),
    max_lifetime_ms := integer()
}.
-type creatable_renewers_0() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type create_delegation_token_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    renewers := list(creatable_renewers_1()),
    max_lifetime_ms := integer()
}.
-type creatable_renewers_1() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type create_delegation_token_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    renewers := list(creatable_renewers_2()),
    max_lifetime_ms := integer()
}.
-type creatable_renewers_2() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type create_delegation_token_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    owner_principal_type := binary() | null,
    owner_principal_name := binary() | null,
    renewers := list(creatable_renewers_3()),
    max_lifetime_ms := integer()
}.
-type creatable_renewers_3() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
