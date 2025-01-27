-module(kafcod_request_header).
-export([
    encode_request_header_0/3,
    decode_request_header_0/1,
    encode_request_header_1/4,
    decode_request_header_1/1,
    encode_request_header_2/4,
    decode_request_header_2/1
]).

-define(EMPTY_TAG_BUFFER, <<0:8/big>>).

-spec encode_request_header_0(
    ApiKey :: kafcod:api_key(),
    ApiVersion :: non_neg_integer(),
    CorrelationId :: non_neg_integer()
) ->
    iodata().

encode_request_header_0(ApiKey, ApiVersion, CorrelationId) when
    is_integer(ApiKey), is_integer(ApiVersion), is_integer(CorrelationId)
->
    [
        <<ApiKey:16/big, ApiVersion:16/big, CorrelationId:32/big>>
    ].

-spec decode_request_header_0(Input :: binary()) -> {request_header_0(), Rest :: binary()}.
-type request_header_0() :: #{
    api_key := kafcod:api_key(),
    api_version := non_neg_integer(),
    correlation_id := non_neg_integer()
}.

decode_request_header_0(Bin) when is_binary(Bin) ->
    <<ApiKey:16/big, ApiVersion:16/big, CorrelationId:32/big, Rest/binary>> = Bin,
    {#{api_key => ApiKey, api_version => ApiVersion, correlation_id => CorrelationId}, Rest}.

% It's a little unclear what ClientId should be set to. The protocol schema simply says: "The client ID string."
%
% The official Kafka documentation at https://kafka.apache.org/documentation/#producerconfigs_client.id says the
% following:
%
%  > An id string to pass to the server when making requests. The purpose of this is to be able to track the source of
%  > requests beyond just ip/port by allowing a logical application name to be included in server-side request logging.
%
% The emphasis on logging, to me, implies that you might want it to be fairly fine-grained, so you can identify a
% particular instance of an application.
%
% This is echoed by the Confluent documentation at
% https://docs.confluent.io/platform/current/clients/producer.html#ak-producer-configuration:
%
%  > Although not required, you should always set a client.id since this allows you to easily correlate requests on the
%  > broker with the client instance which made it.
%
% However, the Kafka documentation specifically says "logical application name", *not* "client instance".
%
% The Confluent documentation at
% https://docs.confluent.io/platform/current/clients/consumer.html#ak-consumer-configuration gives us another hint:
%
%  > Typically, all consumers within the same group will share the same client ID in order to enforce client quotas.
%
% ...and the documentation for quotas, at https://docs.confluent.io/platform/current/kafka/post-deployment.html#quotas:
%
%  > A client-id logically identifies an application making a request.
%
% Which takes us back to "logical application name" mentioned earlier.
%
% The documentation for Kafka.js, at https://kafka.js.org/docs/configuration#client-id, suggests the following:
%
%  > Therefore the clientId should be shared across multiple instances in a cluster or horizontally scaled application,
%  > but distinct for each application.
%
% It gives an example: "booking-events-processor".
%
% Kafire hard-codes it to "kafire"; this is *wrong*.
% The 'kcat' utility defaults to 'rdkafka', but allows you to set it with (e.g.) -X client.id=booking-events-processor

-spec encode_request_header_1(
    ApiKey :: kafcod:api_key(),
    ApiVersion :: non_neg_integer(),
    CorrelationId :: non_neg_integer(),
    ClientId :: binary() | null
) ->
    iodata().

encode_request_header_1(ApiKey, ApiVersion, CorrelationId, ClientId) when
    is_integer(ApiKey), is_integer(ApiVersion), is_integer(CorrelationId)
->
    [
        <<ApiKey:16/big, ApiVersion:16/big, CorrelationId:32/big>>,
        kafcod_primitives:encode_nullable_string(ClientId)
    ].

-spec decode_request_header_1(Input :: binary()) -> {request_header_1(), Rest :: binary()}.
-type request_header_1() :: #{
    api_key := kafcod:api_key(),
    api_version := non_neg_integer(),
    correlation_id := non_neg_integer(),
    client_id := binary() | null
}.

decode_request_header_1(Bin) when is_binary(Bin) ->
    <<ApiKey:16/big, ApiVersion:16/big, CorrelationId:32/big, Bin0/binary>> = Bin,
    {ClientId, Rest} = kafcod_primitives:decode_nullable_string(Bin0),
    {
        #{
            api_key => ApiKey,
            api_version => ApiVersion,
            correlation_id => CorrelationId,
            client_id => ClientId
        },
        Rest
    }.

-spec encode_request_header_2(
    ApiKey :: kafcod:api_key(),
    ApiVersion :: non_neg_integer(),
    CorrelationId :: non_neg_integer(),
    ClientId :: binary() | null
) ->
    iodata().

encode_request_header_2(ApiKey, ApiVersion, CorrelationId, ClientId) when
    is_integer(ApiKey), is_integer(ApiVersion), is_integer(CorrelationId)
->
    [
        <<ApiKey:16/big, ApiVersion:16/big, CorrelationId:32/big>>,
        kafcod_primitives:encode_nullable_string(ClientId),
        ?EMPTY_TAG_BUFFER
    ].

-spec decode_request_header_2(Input :: binary()) -> {request_header_2(), Rest :: binary()}.
-type request_header_2() :: #{
    api_key := kafcod:api_key(),
    api_version := non_neg_integer(),
    correlation_id := non_neg_integer(),
    client_id := binary() | null
}.

decode_request_header_2(Bin) when is_binary(Bin) ->
    <<ApiKey:16/big, ApiVersion:16/big, CorrelationId:32/big, Bin0/binary>> = Bin,
    {ClientId, Bin1} = kafcod_primitives:decode_nullable_string(Bin0),
    % Skip over the (assumed empty) TAG_BUFFER.
    <<0:8/big, Rest/binary>> = Bin1,
    {
        #{
            api_key => ApiKey,
            api_version => ApiVersion,
            correlation_id => CorrelationId,
            client_id => ClientId
        },
        Rest
    }.
