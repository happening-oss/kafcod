-module(unregister_broker_request).
-export([
    encode_unregister_broker_request_0/1,
    decode_unregister_broker_request_0/1
]).
-export_type([
    unregister_broker_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(UNREGISTER_BROKER_REQUEST, 64).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_unregister_broker_request_0(unregister_broker_request_0()) -> iodata().

encode_unregister_broker_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID to unregister.
        broker_id := BrokerId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId)
->
    [
        ?encode_request_header_2(?UNREGISTER_BROKER_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?EMPTY_TAG_BUFFER
    ];
encode_unregister_broker_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32
    }).

-spec decode_unregister_broker_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: unregister_broker_request_0(),
    Rest :: binary().

decode_unregister_broker_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_unregister_broker_request_0_tagged_field/3,
        Header#{
            broker_id => BrokerId
        },
        Bin1
    ).

-spec decode_unregister_broker_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: unregister_broker_request_0().

decode_unregister_broker_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type unregister_broker_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer()
}.
