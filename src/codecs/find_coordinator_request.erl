-module(find_coordinator_request).
-export([
    encode_find_coordinator_request_0/1,
    decode_find_coordinator_request_0/1,
    encode_find_coordinator_request_1/1,
    decode_find_coordinator_request_1/1,
    encode_find_coordinator_request_2/1,
    decode_find_coordinator_request_2/1,
    encode_find_coordinator_request_3/1,
    decode_find_coordinator_request_3/1,
    encode_find_coordinator_request_4/1,
    decode_find_coordinator_request_4/1,
    encode_find_coordinator_request_5/1,
    decode_find_coordinator_request_5/1
]).
-export_type([
    find_coordinator_request_0/0,
    find_coordinator_request_1/0,
    find_coordinator_request_2/0,
    find_coordinator_request_3/0,
    find_coordinator_request_4/0,
    find_coordinator_request_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(FIND_COORDINATOR_REQUEST, 10).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_find_coordinator_request_0(find_coordinator_request_0()) -> iodata().

encode_find_coordinator_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The coordinator key.
        key := Key
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(Key)
->
    [
        ?encode_request_header_1(?FIND_COORDINATOR_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(Key)
    ];
encode_find_coordinator_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        key => string
    }).

-spec decode_find_coordinator_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_request_0(),
    Rest :: binary().

decode_find_coordinator_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(Key, Bin0, Bin1),
    {
        Header#{
            key => Key
        },
        Bin1
    }.

-spec encode_find_coordinator_request_1(find_coordinator_request_1()) -> iodata().

encode_find_coordinator_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The coordinator key.
        key := Key,
        % The coordinator key type. (Group, transaction, etc.)
        key_type := KeyType
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(Key),
    ?is_int8(KeyType)
->
    [
        ?encode_request_header_1(?FIND_COORDINATOR_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(Key),
        ?encode_int8(KeyType)
    ];
encode_find_coordinator_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        key => string,
        key_type => int8
    }).

-spec decode_find_coordinator_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_request_1(),
    Rest :: binary().

decode_find_coordinator_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(Key, Bin0, Bin1),
    ?_decode_int8(KeyType, Bin1, Bin2),
    {
        Header#{
            key => Key,
            key_type => KeyType
        },
        Bin2
    }.

-spec encode_find_coordinator_request_2(find_coordinator_request_2()) -> iodata().

encode_find_coordinator_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The coordinator key.
        key := Key,
        % The coordinator key type. (Group, transaction, etc.)
        key_type := KeyType
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(Key),
    ?is_int8(KeyType)
->
    [
        ?encode_request_header_1(?FIND_COORDINATOR_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(Key),
        ?encode_int8(KeyType)
    ];
encode_find_coordinator_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        key => string,
        key_type => int8
    }).

-spec decode_find_coordinator_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_request_2(),
    Rest :: binary().

decode_find_coordinator_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(Key, Bin0, Bin1),
    ?_decode_int8(KeyType, Bin1, Bin2),
    {
        Header#{
            key => Key,
            key_type => KeyType
        },
        Bin2
    }.

-spec encode_find_coordinator_request_3(find_coordinator_request_3()) -> iodata().

encode_find_coordinator_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The coordinator key.
        key := Key,
        % The coordinator key type. (Group, transaction, etc.)
        key_type := KeyType
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(Key),
    ?is_int8(KeyType)
->
    [
        ?encode_request_header_2(?FIND_COORDINATOR_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_string(Key),
        ?encode_int8(KeyType),
        ?EMPTY_TAG_BUFFER
    ];
encode_find_coordinator_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        key => string,
        key_type => int8
    }).

-spec decode_find_coordinator_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_request_3(),
    Rest :: binary().

decode_find_coordinator_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(Key, Bin0, Bin1),
    ?_decode_int8(KeyType, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_find_coordinator_request_3_tagged_field/3,
        Header#{
            key => Key,
            key_type => KeyType
        },
        Bin2
    ).

-spec decode_find_coordinator_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: find_coordinator_request_3().

decode_find_coordinator_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_find_coordinator_request_4(find_coordinator_request_4()) -> iodata().

encode_find_coordinator_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The coordinator key type. (Group, transaction, etc.)
        key_type := KeyType,
        % The coordinator keys.
        coordinator_keys := CoordinatorKeys
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int8(KeyType),
    ?is_array(CoordinatorKeys)
->
    [
        ?encode_request_header_2(?FIND_COORDINATOR_REQUEST, 4, CorrelationId, ClientId),
        ?encode_int8(KeyType),
        ?encode_compact_array(CoordinatorKeys, ?encode_compact_string_),
        ?EMPTY_TAG_BUFFER
    ];
encode_find_coordinator_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        key_type => int8,
        coordinator_keys => {array, string}
    }).

-spec decode_find_coordinator_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_request_4(),
    Rest :: binary().

decode_find_coordinator_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int8(KeyType, Bin0, Bin1),
    ?_decode_compact_array(CoordinatorKeys, Bin1, Bin2, ?decode_string_),
    ?decode_tagged_fields(
        fun decode_find_coordinator_request_4_tagged_field/3,
        Header#{
            key_type => KeyType,
            coordinator_keys => CoordinatorKeys
        },
        Bin2
    ).

-spec decode_find_coordinator_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: find_coordinator_request_4().

decode_find_coordinator_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_find_coordinator_request_5(find_coordinator_request_5()) -> iodata().

encode_find_coordinator_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The coordinator key type. (Group, transaction, etc.)
        key_type := KeyType,
        % The coordinator keys.
        coordinator_keys := CoordinatorKeys
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int8(KeyType),
    ?is_array(CoordinatorKeys)
->
    [
        ?encode_request_header_2(?FIND_COORDINATOR_REQUEST, 5, CorrelationId, ClientId),
        ?encode_int8(KeyType),
        ?encode_compact_array(CoordinatorKeys, ?encode_compact_string_),
        ?EMPTY_TAG_BUFFER
    ];
encode_find_coordinator_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        key_type => int8,
        coordinator_keys => {array, string}
    }).

-spec decode_find_coordinator_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_request_5(),
    Rest :: binary().

decode_find_coordinator_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int8(KeyType, Bin0, Bin1),
    ?_decode_compact_array(CoordinatorKeys, Bin1, Bin2, ?decode_string_),
    ?decode_tagged_fields(
        fun decode_find_coordinator_request_5_tagged_field/3,
        Header#{
            key_type => KeyType,
            coordinator_keys => CoordinatorKeys
        },
        Bin2
    ).

-spec decode_find_coordinator_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: find_coordinator_request_5().

decode_find_coordinator_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type find_coordinator_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    key := binary()
}.
-type find_coordinator_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    key := binary(),
    key_type := integer()
}.
-type find_coordinator_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    key := binary(),
    key_type := integer()
}.
-type find_coordinator_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    key := binary(),
    key_type := integer()
}.
-type find_coordinator_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    key_type := integer(),
    coordinator_keys := list(binary())
}.
-type find_coordinator_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    key_type := integer(),
    coordinator_keys := list(binary())
}.
