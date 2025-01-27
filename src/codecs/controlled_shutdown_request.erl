-module(controlled_shutdown_request).
-export([
    encode_controlled_shutdown_request_0/1,
    decode_controlled_shutdown_request_0/1,
    encode_controlled_shutdown_request_1/1,
    decode_controlled_shutdown_request_1/1,
    encode_controlled_shutdown_request_2/1,
    decode_controlled_shutdown_request_2/1,
    encode_controlled_shutdown_request_3/1,
    decode_controlled_shutdown_request_3/1
]).
-export_type([
    controlled_shutdown_request_0/0,
    controlled_shutdown_request_1/0,
    controlled_shutdown_request_2/0,
    controlled_shutdown_request_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(CONTROLLED_SHUTDOWN_REQUEST, 7).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_controlled_shutdown_request_0(controlled_shutdown_request_0()) -> iodata().

encode_controlled_shutdown_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The id of the broker for which controlled shutdown has been requested.
        broker_id := BrokerId
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(BrokerId)
->
    [
        ?encode_request_header_0(?CONTROLLED_SHUTDOWN_REQUEST, 0, CorrelationId),
        ?encode_int32(BrokerId)
    ];
encode_controlled_shutdown_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        broker_id => int32
    }).

-spec decode_controlled_shutdown_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: controlled_shutdown_request_0(),
    Rest :: binary().

decode_controlled_shutdown_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_0(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    {
        Header#{
            broker_id => BrokerId
        },
        Bin1
    }.

-spec encode_controlled_shutdown_request_1(controlled_shutdown_request_1()) -> iodata().

encode_controlled_shutdown_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The id of the broker for which controlled shutdown has been requested.
        broker_id := BrokerId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId)
->
    [
        ?encode_request_header_1(?CONTROLLED_SHUTDOWN_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(BrokerId)
    ];
encode_controlled_shutdown_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32
    }).

-spec decode_controlled_shutdown_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: controlled_shutdown_request_1(),
    Rest :: binary().

decode_controlled_shutdown_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    {
        Header#{
            broker_id => BrokerId
        },
        Bin1
    }.

-spec encode_controlled_shutdown_request_2(controlled_shutdown_request_2()) -> iodata().

encode_controlled_shutdown_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The id of the broker for which controlled shutdown has been requested.
        broker_id := BrokerId,
        % The broker epoch.
        broker_epoch := BrokerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch)
->
    [
        ?encode_request_header_1(?CONTROLLED_SHUTDOWN_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch)
    ];
encode_controlled_shutdown_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64
    }).

-spec decode_controlled_shutdown_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: controlled_shutdown_request_2(),
    Rest :: binary().

decode_controlled_shutdown_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    {
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch
        },
        Bin2
    }.

-spec encode_controlled_shutdown_request_3(controlled_shutdown_request_3()) -> iodata().

encode_controlled_shutdown_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The id of the broker for which controlled shutdown has been requested.
        broker_id := BrokerId,
        % The broker epoch.
        broker_epoch := BrokerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch)
->
    [
        ?encode_request_header_2(?CONTROLLED_SHUTDOWN_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_controlled_shutdown_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64
    }).

-spec decode_controlled_shutdown_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: controlled_shutdown_request_3(),
    Rest :: binary().

decode_controlled_shutdown_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_controlled_shutdown_request_3_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch
        },
        Bin2
    ).

-spec decode_controlled_shutdown_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_controlled_shutdown_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type controlled_shutdown_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    broker_id := integer()
}.
-type controlled_shutdown_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer()
}.
-type controlled_shutdown_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer()
}.
-type controlled_shutdown_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer()
}.
