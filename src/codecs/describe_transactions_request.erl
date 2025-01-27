-module(describe_transactions_request).
-export([
    encode_describe_transactions_request_0/1,
    decode_describe_transactions_request_0/1
]).
-export_type([
    describe_transactions_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_TRANSACTIONS_REQUEST, 65).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_transactions_request_0(describe_transactions_request_0()) -> iodata().

encode_describe_transactions_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Array of transactionalIds to include in describe results. If empty, then no results will be returned.
        transactional_ids := TransactionalIds
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(TransactionalIds)
->
    [
        ?encode_request_header_2(?DESCRIBE_TRANSACTIONS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_array(TransactionalIds, ?encode_compact_string_),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_transactions_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_ids => {array, string}
    }).

-spec decode_describe_transactions_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_transactions_request_0(),
    Rest :: binary().

decode_describe_transactions_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(TransactionalIds, Bin0, Bin1, ?decode_string_),
    ?decode_tagged_fields(
        fun decode_describe_transactions_request_0_tagged_field/3,
        Header#{
            transactional_ids => TransactionalIds
        },
        Bin1
    ).

-spec decode_describe_transactions_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_transactions_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_transactions_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_ids := list(binary())
}.
