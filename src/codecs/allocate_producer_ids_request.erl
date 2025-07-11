-module(allocate_producer_ids_request).
-export([
    encode_allocate_producer_ids_request_0/1,
    decode_allocate_producer_ids_request_0/1
]).
-export_type([
    allocate_producer_ids_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ALLOCATE_PRODUCER_IDS_REQUEST, 67).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_allocate_producer_ids_request_0(allocate_producer_ids_request_0()) -> iodata().

encode_allocate_producer_ids_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the requesting broker
        broker_id := BrokerId,
        % The epoch of the requesting broker
        broker_epoch := BrokerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch)
->
    [
        ?encode_request_header_2(?ALLOCATE_PRODUCER_IDS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_allocate_producer_ids_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64
    }).

-spec decode_allocate_producer_ids_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: allocate_producer_ids_request_0(),
    Rest :: binary().

decode_allocate_producer_ids_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_allocate_producer_ids_request_0_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch
        },
        Bin2
    ).

-spec decode_allocate_producer_ids_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: allocate_producer_ids_request_0().

decode_allocate_producer_ids_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type allocate_producer_ids_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer()
}.
