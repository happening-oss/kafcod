-module(list_transactions_request).
-export([
    encode_list_transactions_request_0/1,
    decode_list_transactions_request_0/1,
    encode_list_transactions_request_1/1,
    decode_list_transactions_request_1/1
]).
-export_type([
    list_transactions_request_0/0,
    list_transactions_request_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(LIST_TRANSACTIONS_REQUEST, 66).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_transactions_request_0(list_transactions_request_0()) -> iodata().

encode_list_transactions_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transaction states to filter by: if empty, all transactions are returned; if non-empty, then only transactions matching one of the filtered states will be returned
        state_filters := StateFilters,
        % The producerIds to filter by: if empty, all transactions will be returned; if non-empty, only transactions which match one of the filtered producerIds will be returned
        producer_id_filters := ProducerIdFilters
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(StateFilters),
    ?is_array(ProducerIdFilters)
->
    [
        ?encode_request_header_2(?LIST_TRANSACTIONS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_array(StateFilters, ?encode_compact_string_),
        ?encode_compact_array(ProducerIdFilters, ?encode_int64_),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_transactions_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        state_filters => {array, string},
        producer_id_filters => {array, int64}
    }).

-spec decode_list_transactions_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_transactions_request_0(),
    Rest :: binary().

decode_list_transactions_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(StateFilters, Bin0, Bin1, ?decode_string_),
    ?_decode_compact_array(ProducerIdFilters, Bin1, Bin2, ?decode_int64_),
    ?decode_tagged_fields(
        fun decode_list_transactions_request_0_tagged_field/3,
        Header#{
            state_filters => StateFilters,
            producer_id_filters => ProducerIdFilters
        },
        Bin2
    ).

-spec decode_list_transactions_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_transactions_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_transactions_request_1(list_transactions_request_1()) -> iodata().

encode_list_transactions_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The transaction states to filter by: if empty, all transactions are returned; if non-empty, then only transactions matching one of the filtered states will be returned
        state_filters := StateFilters,
        % The producerIds to filter by: if empty, all transactions will be returned; if non-empty, only transactions which match one of the filtered producerIds will be returned
        producer_id_filters := ProducerIdFilters,
        % Duration (in millis) to filter by: if < 0, all transactions will be returned; otherwise, only transactions running longer than this duration will be returned
        duration_filter := DurationFilter
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(StateFilters),
    ?is_array(ProducerIdFilters),
    ?is_int64(DurationFilter)
->
    [
        ?encode_request_header_2(?LIST_TRANSACTIONS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_compact_array(StateFilters, ?encode_compact_string_),
        ?encode_compact_array(ProducerIdFilters, ?encode_int64_),
        ?encode_int64(DurationFilter),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_transactions_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        state_filters => {array, string},
        producer_id_filters => {array, int64},
        duration_filter => int64
    }).

-spec decode_list_transactions_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: list_transactions_request_1(),
    Rest :: binary().

decode_list_transactions_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(StateFilters, Bin0, Bin1, ?decode_string_),
    ?_decode_compact_array(ProducerIdFilters, Bin1, Bin2, ?decode_int64_),
    ?_decode_int64(DurationFilter, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_list_transactions_request_1_tagged_field/3,
        Header#{
            state_filters => StateFilters,
            producer_id_filters => ProducerIdFilters,
            duration_filter => DurationFilter
        },
        Bin3
    ).

-spec decode_list_transactions_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_transactions_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_transactions_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    state_filters := list(binary()),
    producer_id_filters := list(integer())
}.
-type list_transactions_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    state_filters := list(binary()),
    producer_id_filters := list(integer()),
    duration_filter := integer()
}.
