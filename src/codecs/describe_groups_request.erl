-module(describe_groups_request).
-export([
    encode_describe_groups_request_0/1,
    decode_describe_groups_request_0/1,
    encode_describe_groups_request_1/1,
    decode_describe_groups_request_1/1,
    encode_describe_groups_request_2/1,
    decode_describe_groups_request_2/1,
    encode_describe_groups_request_3/1,
    decode_describe_groups_request_3/1,
    encode_describe_groups_request_4/1,
    decode_describe_groups_request_4/1,
    encode_describe_groups_request_5/1,
    decode_describe_groups_request_5/1
]).
-export_type([
    describe_groups_request_0/0,
    describe_groups_request_1/0,
    describe_groups_request_2/0,
    describe_groups_request_3/0,
    describe_groups_request_4/0,
    describe_groups_request_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_GROUPS_REQUEST, 15).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_groups_request_0(describe_groups_request_0()) -> iodata().

encode_describe_groups_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the groups to describe
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Groups)
->
    [
        ?encode_request_header_1(?DESCRIBE_GROUPS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Groups, ?encode_string_)
    ];
encode_describe_groups_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups => {array, string}
    }).

-spec decode_describe_groups_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_request_0(),
    Rest :: binary().

decode_describe_groups_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Groups, Bin0, Bin1, ?decode_string_),
    {
        Header#{
            groups => Groups
        },
        Bin1
    }.

-spec encode_describe_groups_request_1(describe_groups_request_1()) -> iodata().

encode_describe_groups_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the groups to describe
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Groups)
->
    [
        ?encode_request_header_1(?DESCRIBE_GROUPS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Groups, ?encode_string_)
    ];
encode_describe_groups_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups => {array, string}
    }).

-spec decode_describe_groups_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_request_1(),
    Rest :: binary().

decode_describe_groups_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Groups, Bin0, Bin1, ?decode_string_),
    {
        Header#{
            groups => Groups
        },
        Bin1
    }.

-spec encode_describe_groups_request_2(describe_groups_request_2()) -> iodata().

encode_describe_groups_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the groups to describe
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Groups)
->
    [
        ?encode_request_header_1(?DESCRIBE_GROUPS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_array(Groups, ?encode_string_)
    ];
encode_describe_groups_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups => {array, string}
    }).

-spec decode_describe_groups_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_request_2(),
    Rest :: binary().

decode_describe_groups_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Groups, Bin0, Bin1, ?decode_string_),
    {
        Header#{
            groups => Groups
        },
        Bin1
    }.

-spec encode_describe_groups_request_3(describe_groups_request_3()) -> iodata().

encode_describe_groups_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the groups to describe
        groups := Groups,
        % Whether to include authorized operations.
        include_authorized_operations := IncludeAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Groups),
    ?is_bool(IncludeAuthorizedOperations)
->
    [
        ?encode_request_header_1(?DESCRIBE_GROUPS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_array(Groups, ?encode_string_),
        ?encode_bool(IncludeAuthorizedOperations)
    ];
encode_describe_groups_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups => {array, string},
        include_authorized_operations => bool
    }).

-spec decode_describe_groups_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_request_3(),
    Rest :: binary().

decode_describe_groups_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Groups, Bin0, Bin1, ?decode_string_),
    ?_decode_bool(IncludeAuthorizedOperations, Bin1, Bin2),
    {
        Header#{
            groups => Groups,
            include_authorized_operations => IncludeAuthorizedOperations
        },
        Bin2
    }.

-spec encode_describe_groups_request_4(describe_groups_request_4()) -> iodata().

encode_describe_groups_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the groups to describe
        groups := Groups,
        % Whether to include authorized operations.
        include_authorized_operations := IncludeAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Groups),
    ?is_bool(IncludeAuthorizedOperations)
->
    [
        ?encode_request_header_1(?DESCRIBE_GROUPS_REQUEST, 4, CorrelationId, ClientId),
        ?encode_array(Groups, ?encode_string_),
        ?encode_bool(IncludeAuthorizedOperations)
    ];
encode_describe_groups_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups => {array, string},
        include_authorized_operations => bool
    }).

-spec decode_describe_groups_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_request_4(),
    Rest :: binary().

decode_describe_groups_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Groups, Bin0, Bin1, ?decode_string_),
    ?_decode_bool(IncludeAuthorizedOperations, Bin1, Bin2),
    {
        Header#{
            groups => Groups,
            include_authorized_operations => IncludeAuthorizedOperations
        },
        Bin2
    }.

-spec encode_describe_groups_request_5(describe_groups_request_5()) -> iodata().

encode_describe_groups_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the groups to describe
        groups := Groups,
        % Whether to include authorized operations.
        include_authorized_operations := IncludeAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Groups),
    ?is_bool(IncludeAuthorizedOperations)
->
    [
        ?encode_request_header_2(?DESCRIBE_GROUPS_REQUEST, 5, CorrelationId, ClientId),
        ?encode_compact_array(Groups, ?encode_compact_string_),
        ?encode_bool(IncludeAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_groups_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups => {array, string},
        include_authorized_operations => bool
    }).

-spec decode_describe_groups_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_request_5(),
    Rest :: binary().

decode_describe_groups_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Groups, Bin0, Bin1, ?decode_string_),
    ?_decode_bool(IncludeAuthorizedOperations, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_describe_groups_request_5_tagged_field/3,
        Header#{
            groups => Groups,
            include_authorized_operations => IncludeAuthorizedOperations
        },
        Bin2
    ).

-spec decode_describe_groups_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_groups_request_5().

decode_describe_groups_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_groups_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups := list(binary())
}.
-type describe_groups_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups := list(binary())
}.
-type describe_groups_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups := list(binary())
}.
-type describe_groups_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups := list(binary()),
    include_authorized_operations := boolean()
}.
-type describe_groups_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups := list(binary()),
    include_authorized_operations := boolean()
}.
-type describe_groups_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups := list(binary()),
    include_authorized_operations := boolean()
}.
