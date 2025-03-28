-module(get_telemetry_subscriptions_request).
-export([
    encode_get_telemetry_subscriptions_request_0/1,
    decode_get_telemetry_subscriptions_request_0/1
]).
-export_type([
    get_telemetry_subscriptions_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(GET_TELEMETRY_SUBSCRIPTIONS_REQUEST, 71).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_get_telemetry_subscriptions_request_0(get_telemetry_subscriptions_request_0()) -> iodata().

encode_get_telemetry_subscriptions_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Unique id for this client instance, must be set to 0 on the first request.
        client_instance_id := ClientInstanceId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_uuid(ClientInstanceId)
->
    [
        ?encode_request_header_2(?GET_TELEMETRY_SUBSCRIPTIONS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_uuid(ClientInstanceId),
        ?EMPTY_TAG_BUFFER
    ];
encode_get_telemetry_subscriptions_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        client_instance_id => uuid
    }).

-spec decode_get_telemetry_subscriptions_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: get_telemetry_subscriptions_request_0(),
    Rest :: binary().

decode_get_telemetry_subscriptions_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_uuid(ClientInstanceId, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_get_telemetry_subscriptions_request_0_tagged_field/3,
        Header#{
            client_instance_id => ClientInstanceId
        },
        Bin1
    ).

-spec decode_get_telemetry_subscriptions_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_get_telemetry_subscriptions_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type get_telemetry_subscriptions_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    client_instance_id := kafcod:uuid()
}.
