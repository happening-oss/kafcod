-module(default_principal_data).
-export([
    encode_default_principal_data_0/1,
    decode_default_principal_data_0/1
]).
-export_type([
    default_principal_data_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_default_principal_data_0(default_principal_data_0()) -> iodata().

encode_default_principal_data_0(
    _Args = #{
        % The principal type
        type := Type,
        % The principal name
        name := Name,
        % Whether the principal was authenticated by a delegation token on the forwarding broker.
        token_authenticated := TokenAuthenticated
    }
) when
    ?is_string(Type),
    ?is_string(Name),
    ?is_bool(TokenAuthenticated)
->
    [
        ?encode_compact_string(Type),
        ?encode_compact_string(Name),
        ?encode_bool(TokenAuthenticated),
        ?EMPTY_TAG_BUFFER
    ];
encode_default_principal_data_0(Args) ->
    ?encoder_error(Args, #{
        type => string,
        name => string,
        token_authenticated => bool
    }).

-spec decode_default_principal_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: default_principal_data_0(),
    Rest :: binary().

decode_default_principal_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Type, Bin0, Bin1),
    ?_decode_compact_string(Name, Bin1, Bin2),
    ?_decode_bool(TokenAuthenticated, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_default_principal_data_0_tagged_field/3,
        #{
            type => Type,
            name => Name,
            token_authenticated => TokenAuthenticated
        },
        Bin3
    ).

-spec decode_default_principal_data_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_default_principal_data_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type default_principal_data_0() :: #{
    type := binary(),
    name := binary(),
    token_authenticated := boolean()
}.
