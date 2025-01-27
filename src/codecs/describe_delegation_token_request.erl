-module(describe_delegation_token_request).
-export([
    encode_describe_delegation_token_request_0/1,
    decode_describe_delegation_token_request_0/1,
    encode_describe_delegation_token_request_1/1,
    decode_describe_delegation_token_request_1/1,
    encode_describe_delegation_token_request_2/1,
    decode_describe_delegation_token_request_2/1,
    encode_describe_delegation_token_request_3/1,
    decode_describe_delegation_token_request_3/1
]).
-export_type([
    describe_delegation_token_request_0/0,
    describe_delegation_token_owner_0/0,
    describe_delegation_token_request_1/0,
    describe_delegation_token_owner_1/0,
    describe_delegation_token_request_2/0,
    describe_delegation_token_owner_2/0,
    describe_delegation_token_request_3/0,
    describe_delegation_token_owner_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_DELEGATION_TOKEN_REQUEST, 41).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_delegation_token_request_0(describe_delegation_token_request_0()) -> iodata().

encode_describe_delegation_token_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each owner that we want to describe delegation tokens for, or null to describe all tokens.
        owners := Owners
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Owners)
->
    [
        ?encode_request_header_1(?DESCRIBE_DELEGATION_TOKEN_REQUEST, 0, CorrelationId, ClientId),
        ?encode_nullable_array(Owners, fun encode_describe_delegation_token_owner_0/1)
    ];
encode_describe_delegation_token_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        owners => {nullable_array, describe_delegation_token_owner_0}
    }).

-spec decode_describe_delegation_token_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_request_0(),
    Rest :: binary().

decode_describe_delegation_token_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Owners, Bin0, Bin1, ?_decode_element(decode_describe_delegation_token_owner_0)),
    {
        Header#{
            owners => Owners
        },
        Bin1
    }.

-spec encode_describe_delegation_token_owner_0(describe_delegation_token_owner_0()) -> iodata().

encode_describe_delegation_token_owner_0(
    _Args = #{
        % The owner principal type.
        principal_type := PrincipalType,
        % The owner principal name.
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
encode_describe_delegation_token_owner_0(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_describe_delegation_token_owner_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_owner_0(),
    Rest :: binary().

decode_describe_delegation_token_owner_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(PrincipalType, Bin0, Bin1),
    ?_decode_string(PrincipalName, Bin1, Bin2),
    {
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    }.

-spec encode_describe_delegation_token_request_1(describe_delegation_token_request_1()) -> iodata().

encode_describe_delegation_token_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each owner that we want to describe delegation tokens for, or null to describe all tokens.
        owners := Owners
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Owners)
->
    [
        ?encode_request_header_1(?DESCRIBE_DELEGATION_TOKEN_REQUEST, 1, CorrelationId, ClientId),
        ?encode_nullable_array(Owners, fun encode_describe_delegation_token_owner_1/1)
    ];
encode_describe_delegation_token_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        owners => {nullable_array, describe_delegation_token_owner_1}
    }).

-spec decode_describe_delegation_token_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_request_1(),
    Rest :: binary().

decode_describe_delegation_token_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Owners, Bin0, Bin1, ?_decode_element(decode_describe_delegation_token_owner_1)),
    {
        Header#{
            owners => Owners
        },
        Bin1
    }.

-spec encode_describe_delegation_token_owner_1(describe_delegation_token_owner_1()) -> iodata().

encode_describe_delegation_token_owner_1(
    _Args = #{
        % The owner principal type.
        principal_type := PrincipalType,
        % The owner principal name.
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
encode_describe_delegation_token_owner_1(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_describe_delegation_token_owner_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_owner_1(),
    Rest :: binary().

decode_describe_delegation_token_owner_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(PrincipalType, Bin0, Bin1),
    ?_decode_string(PrincipalName, Bin1, Bin2),
    {
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    }.

-spec encode_describe_delegation_token_request_2(describe_delegation_token_request_2()) -> iodata().

encode_describe_delegation_token_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each owner that we want to describe delegation tokens for, or null to describe all tokens.
        owners := Owners
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Owners)
->
    [
        ?encode_request_header_2(?DESCRIBE_DELEGATION_TOKEN_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Owners, fun encode_describe_delegation_token_owner_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_delegation_token_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        owners => {nullable_array, describe_delegation_token_owner_2}
    }).

-spec decode_describe_delegation_token_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_request_2(),
    Rest :: binary().

decode_describe_delegation_token_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Owners, Bin0, Bin1, ?_decode_element(decode_describe_delegation_token_owner_2)),
    ?decode_tagged_fields(
        fun decode_describe_delegation_token_request_2_tagged_field/3,
        Header#{
            owners => Owners
        },
        Bin1
    ).

-spec decode_describe_delegation_token_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_delegation_token_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_delegation_token_owner_2(describe_delegation_token_owner_2()) -> iodata().

encode_describe_delegation_token_owner_2(
    _Args = #{
        % The owner principal type.
        principal_type := PrincipalType,
        % The owner principal name.
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
encode_describe_delegation_token_owner_2(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_describe_delegation_token_owner_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_owner_2(),
    Rest :: binary().

decode_describe_delegation_token_owner_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(PrincipalType, Bin0, Bin1),
    ?_decode_compact_string(PrincipalName, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_describe_delegation_token_owner_2_tagged_field/3,
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    ).

-spec decode_describe_delegation_token_owner_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_delegation_token_owner_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_delegation_token_request_3(describe_delegation_token_request_3()) -> iodata().

encode_describe_delegation_token_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each owner that we want to describe delegation tokens for, or null to describe all tokens.
        owners := Owners
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Owners)
->
    [
        ?encode_request_header_2(?DESCRIBE_DELEGATION_TOKEN_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Owners, fun encode_describe_delegation_token_owner_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_delegation_token_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        owners => {nullable_array, describe_delegation_token_owner_3}
    }).

-spec decode_describe_delegation_token_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_request_3(),
    Rest :: binary().

decode_describe_delegation_token_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Owners, Bin0, Bin1, ?_decode_element(decode_describe_delegation_token_owner_3)),
    ?decode_tagged_fields(
        fun decode_describe_delegation_token_request_3_tagged_field/3,
        Header#{
            owners => Owners
        },
        Bin1
    ).

-spec decode_describe_delegation_token_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_delegation_token_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_delegation_token_owner_3(describe_delegation_token_owner_3()) -> iodata().

encode_describe_delegation_token_owner_3(
    _Args = #{
        % The owner principal type.
        principal_type := PrincipalType,
        % The owner principal name.
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
encode_describe_delegation_token_owner_3(Args) ->
    ?encoder_error(Args, #{
        principal_type => string,
        principal_name => string
    }).

-spec decode_describe_delegation_token_owner_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_delegation_token_owner_3(),
    Rest :: binary().

decode_describe_delegation_token_owner_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(PrincipalType, Bin0, Bin1),
    ?_decode_compact_string(PrincipalName, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_describe_delegation_token_owner_3_tagged_field/3,
        #{
            principal_type => PrincipalType,
            principal_name => PrincipalName
        },
        Bin2
    ).

-spec decode_describe_delegation_token_owner_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_delegation_token_owner_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_delegation_token_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    owners := list(describe_delegation_token_owner_0()) | null
}.
-type describe_delegation_token_owner_0() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type describe_delegation_token_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    owners := list(describe_delegation_token_owner_1()) | null
}.
-type describe_delegation_token_owner_1() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type describe_delegation_token_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    owners := list(describe_delegation_token_owner_2()) | null
}.
-type describe_delegation_token_owner_2() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
-type describe_delegation_token_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    owners := list(describe_delegation_token_owner_3()) | null
}.
-type describe_delegation_token_owner_3() :: #{
    principal_type := binary(),
    principal_name := binary()
}.
