-module(describe_user_scram_credentials_request).
-export([
    encode_describe_user_scram_credentials_request_0/1,
    decode_describe_user_scram_credentials_request_0/1
]).
-export_type([
    describe_user_scram_credentials_request_0/0,
    user_name_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_USER_SCRAM_CREDENTIALS_REQUEST, 50).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_user_scram_credentials_request_0(describe_user_scram_credentials_request_0()) -> iodata().

encode_describe_user_scram_credentials_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The users to describe, or null/empty to describe all users.
        users := Users
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Users)
->
    [
        ?encode_request_header_2(?DESCRIBE_USER_SCRAM_CREDENTIALS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Users, fun encode_user_name_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_user_scram_credentials_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        users => {nullable_array, user_name_0}
    }).

-spec decode_describe_user_scram_credentials_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_user_scram_credentials_request_0(),
    Rest :: binary().

decode_describe_user_scram_credentials_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Users, Bin0, Bin1, ?_decode_element(decode_user_name_0)),
    ?decode_tagged_fields(
        fun decode_describe_user_scram_credentials_request_0_tagged_field/3,
        Header#{
            users => Users
        },
        Bin1
    ).

-spec decode_describe_user_scram_credentials_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_user_scram_credentials_request_0().

decode_describe_user_scram_credentials_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_user_name_0(user_name_0()) -> iodata().

encode_user_name_0(
    _Args = #{
        % The user name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_compact_string(Name),
        ?EMPTY_TAG_BUFFER
    ];
encode_user_name_0(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_user_name_0(binary()) -> {Decoded, Rest} when
    Decoded :: user_name_0(),
    Rest :: binary().

decode_user_name_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_user_name_0_tagged_field/3,
        #{
            name => Name
        },
        Bin1
    ).

-spec decode_user_name_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: user_name_0().

decode_user_name_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_user_scram_credentials_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    users := list(user_name_0()) | null
}.
-type user_name_0() :: #{
    name := binary()
}.
