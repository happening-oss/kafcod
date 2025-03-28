-module(list_groups_request).
-export([
    encode_list_groups_request_0/1,
    decode_list_groups_request_0/1,
    encode_list_groups_request_1/1,
    decode_list_groups_request_1/1,
    encode_list_groups_request_2/1,
    decode_list_groups_request_2/1,
    encode_list_groups_request_3/1,
    decode_list_groups_request_3/1,
    encode_list_groups_request_4/1,
    decode_list_groups_request_4/1,
    encode_list_groups_request_5/1,
    decode_list_groups_request_5/1
]).
-export_type([
    list_groups_request_0/0,
    list_groups_request_1/0,
    list_groups_request_2/0,
    list_groups_request_3/0,
    list_groups_request_4/0,
    list_groups_request_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(LIST_GROUPS_REQUEST, 16).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_groups_request_0(list_groups_request_0()) -> iodata().

encode_list_groups_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId)
->
    [
        ?encode_request_header_1(?LIST_GROUPS_REQUEST, 0, CorrelationId, ClientId)
    ];
encode_list_groups_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string
    }).

-spec decode_list_groups_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_request_0(),
    Rest :: binary().

decode_list_groups_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    {
        Header#{

        },
        Bin0
    }.

-spec encode_list_groups_request_1(list_groups_request_1()) -> iodata().

encode_list_groups_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId)
->
    [
        ?encode_request_header_1(?LIST_GROUPS_REQUEST, 1, CorrelationId, ClientId)
    ];
encode_list_groups_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string
    }).

-spec decode_list_groups_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_request_1(),
    Rest :: binary().

decode_list_groups_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    {
        Header#{

        },
        Bin0
    }.

-spec encode_list_groups_request_2(list_groups_request_2()) -> iodata().

encode_list_groups_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId)
->
    [
        ?encode_request_header_1(?LIST_GROUPS_REQUEST, 2, CorrelationId, ClientId)
    ];
encode_list_groups_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string
    }).

-spec decode_list_groups_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_request_2(),
    Rest :: binary().

decode_list_groups_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    {
        Header#{

        },
        Bin0
    }.

-spec encode_list_groups_request_3(list_groups_request_3()) -> iodata().

encode_list_groups_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId)
->
    [
        ?encode_request_header_2(?LIST_GROUPS_REQUEST, 3, CorrelationId, ClientId),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_groups_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string
    }).

-spec decode_list_groups_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_request_3(),
    Rest :: binary().

decode_list_groups_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?decode_tagged_fields(
        fun decode_list_groups_request_3_tagged_field/3,
        Header#{

        },
        Bin0
    ).

-spec decode_list_groups_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_groups_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_groups_request_4(list_groups_request_4()) -> iodata().

encode_list_groups_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The states of the groups we want to list. If empty, all groups are returned with their state.
        states_filter := StatesFilter
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(StatesFilter)
->
    [
        ?encode_request_header_2(?LIST_GROUPS_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_array(StatesFilter, ?encode_compact_string_),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_groups_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        states_filter => {array, string}
    }).

-spec decode_list_groups_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_request_4(),
    Rest :: binary().

decode_list_groups_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(StatesFilter, Bin0, Bin1, ?decode_string_),
    ?decode_tagged_fields(
        fun decode_list_groups_request_4_tagged_field/3,
        Header#{
            states_filter => StatesFilter
        },
        Bin1
    ).

-spec decode_list_groups_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_groups_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_groups_request_5(list_groups_request_5()) -> iodata().

encode_list_groups_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The states of the groups we want to list. If empty, all groups are returned with their state.
        states_filter := StatesFilter,
        % The types of the groups we want to list. If empty, all groups are returned with their type.
        types_filter := TypesFilter
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(StatesFilter),
    ?is_array(TypesFilter)
->
    [
        ?encode_request_header_2(?LIST_GROUPS_REQUEST, 5, CorrelationId, ClientId),
        ?encode_compact_array(StatesFilter, ?encode_compact_string_),
        ?encode_compact_array(TypesFilter, ?encode_compact_string_),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_groups_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        states_filter => {array, string},
        types_filter => {array, string}
    }).

-spec decode_list_groups_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: list_groups_request_5(),
    Rest :: binary().

decode_list_groups_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(StatesFilter, Bin0, Bin1, ?decode_string_),
    ?_decode_compact_array(TypesFilter, Bin1, Bin2, ?decode_string_),
    ?decode_tagged_fields(
        fun decode_list_groups_request_5_tagged_field/3,
        Header#{
            states_filter => StatesFilter,
            types_filter => TypesFilter
        },
        Bin2
    ).

-spec decode_list_groups_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_groups_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_groups_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null
}.
-type list_groups_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null
}.
-type list_groups_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null
}.
-type list_groups_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null
}.
-type list_groups_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    states_filter := list(binary())
}.
-type list_groups_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    states_filter := list(binary()),
    types_filter := list(binary())
}.
