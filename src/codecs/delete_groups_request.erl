-module(delete_groups_request).
-export([
    encode_delete_groups_request_0/1,
    decode_delete_groups_request_0/1,
    encode_delete_groups_request_1/1,
    decode_delete_groups_request_1/1,
    encode_delete_groups_request_2/1,
    decode_delete_groups_request_2/1
]).
-export_type([
    delete_groups_request_0/0,
    delete_groups_request_1/0,
    delete_groups_request_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DELETE_GROUPS_REQUEST, 42).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_delete_groups_request_0(delete_groups_request_0()) -> iodata().

encode_delete_groups_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group names to delete.
        groups_names := GroupsNames
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(GroupsNames)
->
    [
        ?encode_request_header_1(?DELETE_GROUPS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(GroupsNames, ?encode_string_)
    ];
encode_delete_groups_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups_names => {array, string}
    }).

-spec decode_delete_groups_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_groups_request_0(),
    Rest :: binary().

decode_delete_groups_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(GroupsNames, Bin0, Bin1, ?decode_string_),
    {
        Header#{
            groups_names => GroupsNames
        },
        Bin1
    }.

-spec encode_delete_groups_request_1(delete_groups_request_1()) -> iodata().

encode_delete_groups_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group names to delete.
        groups_names := GroupsNames
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(GroupsNames)
->
    [
        ?encode_request_header_1(?DELETE_GROUPS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(GroupsNames, ?encode_string_)
    ];
encode_delete_groups_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups_names => {array, string}
    }).

-spec decode_delete_groups_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_groups_request_1(),
    Rest :: binary().

decode_delete_groups_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(GroupsNames, Bin0, Bin1, ?decode_string_),
    {
        Header#{
            groups_names => GroupsNames
        },
        Bin1
    }.

-spec encode_delete_groups_request_2(delete_groups_request_2()) -> iodata().

encode_delete_groups_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group names to delete.
        groups_names := GroupsNames
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(GroupsNames)
->
    [
        ?encode_request_header_2(?DELETE_GROUPS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_array(GroupsNames, ?encode_compact_string_),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_groups_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups_names => {array, string}
    }).

-spec decode_delete_groups_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_groups_request_2(),
    Rest :: binary().

decode_delete_groups_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(GroupsNames, Bin0, Bin1, ?decode_string_),
    ?decode_tagged_fields(
        fun decode_delete_groups_request_2_tagged_field/3,
        Header#{
            groups_names => GroupsNames
        },
        Bin1
    ).

-spec decode_delete_groups_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_groups_request_2().

decode_delete_groups_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type delete_groups_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups_names := list(binary())
}.
-type delete_groups_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups_names := list(binary())
}.
-type delete_groups_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups_names := list(binary())
}.
