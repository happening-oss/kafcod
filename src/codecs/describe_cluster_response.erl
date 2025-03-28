-module(describe_cluster_response).
-export([
    encode_describe_cluster_response_0/1,
    decode_describe_cluster_response_0/1,
    encode_describe_cluster_response_1/1,
    decode_describe_cluster_response_1/1
]).
-export_type([
    describe_cluster_response_0/0,
    describe_cluster_broker_0/0,
    describe_cluster_response_1/0,
    describe_cluster_broker_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_cluster_response_0(describe_cluster_response_0()) -> iodata().

encode_describe_cluster_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top-level error code, or 0 if there was no error
        error_code := ErrorCode,
        % The top-level error message, or null if there was no error.
        error_message := ErrorMessage,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each broker in the response.
        brokers := Brokers,
        % 32-bit bitfield to represent authorized operations for this cluster.
        cluster_authorized_operations := ClusterAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Brokers),
    ?is_int32(ClusterAuthorizedOperations)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_compact_array(Brokers, fun encode_describe_cluster_broker_0/1),
        ?encode_int32(ClusterAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_cluster_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        cluster_id => string,
        controller_id => int32,
        brokers => {array, describe_cluster_broker_0},
        cluster_authorized_operations => int32
    }).

-spec decode_describe_cluster_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_cluster_response_0(),
    Rest :: binary().

decode_describe_cluster_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_string(ClusterId, Bin3, Bin4),
    ?_decode_int32(ControllerId, Bin4, Bin5),
    ?_decode_compact_array(Brokers, Bin5, Bin6, ?_decode_element(decode_describe_cluster_broker_0)),
    ?_decode_int32(ClusterAuthorizedOperations, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_describe_cluster_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            brokers => Brokers,
            cluster_authorized_operations => ClusterAuthorizedOperations
        },
        Bin7
    ).

-spec decode_describe_cluster_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_cluster_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_cluster_broker_0(describe_cluster_broker_0()) -> iodata().

encode_describe_cluster_broker_0(
    _Args = #{
        % The broker ID.
        broker_id := BrokerId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(BrokerId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(BrokerId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_cluster_broker_0(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_describe_cluster_broker_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_cluster_broker_0(),
    Rest :: binary().

decode_describe_cluster_broker_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_compact_nullable_string(Rack, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_describe_cluster_broker_0_tagged_field/3,
        #{
            broker_id => BrokerId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    ).

-spec decode_describe_cluster_broker_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_cluster_broker_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_cluster_response_1(describe_cluster_response_1()) -> iodata().

encode_describe_cluster_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top-level error code, or 0 if there was no error
        error_code := ErrorCode,
        % The top-level error message, or null if there was no error.
        error_message := ErrorMessage,
        % The endpoint type that was described. 1=brokers, 2=controllers.
        endpoint_type := EndpointType,
        % The cluster ID that responding broker belongs to.
        cluster_id := ClusterId,
        % The ID of the controller broker.
        controller_id := ControllerId,
        % Each broker in the response.
        brokers := Brokers,
        % 32-bit bitfield to represent authorized operations for this cluster.
        cluster_authorized_operations := ClusterAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int8(EndpointType),
    ?is_string(ClusterId),
    ?is_int32(ControllerId),
    ?is_array(Brokers),
    ?is_int32(ClusterAuthorizedOperations)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int8(EndpointType),
        ?encode_compact_string(ClusterId),
        ?encode_int32(ControllerId),
        ?encode_compact_array(Brokers, fun encode_describe_cluster_broker_1/1),
        ?encode_int32(ClusterAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_cluster_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        endpoint_type => int8,
        cluster_id => string,
        controller_id => int32,
        brokers => {array, describe_cluster_broker_1},
        cluster_authorized_operations => int32
    }).

-spec decode_describe_cluster_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_cluster_response_1(),
    Rest :: binary().

decode_describe_cluster_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_int8(EndpointType, Bin3, Bin4),
    ?_decode_compact_string(ClusterId, Bin4, Bin5),
    ?_decode_int32(ControllerId, Bin5, Bin6),
    ?_decode_compact_array(Brokers, Bin6, Bin7, ?_decode_element(decode_describe_cluster_broker_1)),
    ?_decode_int32(ClusterAuthorizedOperations, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_describe_cluster_response_1_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            endpoint_type => EndpointType,
            cluster_id => ClusterId,
            controller_id => ControllerId,
            brokers => Brokers,
            cluster_authorized_operations => ClusterAuthorizedOperations
        },
        Bin8
    ).

-spec decode_describe_cluster_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_cluster_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_cluster_broker_1(describe_cluster_broker_1()) -> iodata().

encode_describe_cluster_broker_1(
    _Args = #{
        % The broker ID.
        broker_id := BrokerId,
        % The broker hostname.
        host := Host,
        % The broker port.
        port := Port,
        % The rack of the broker, or null if it has not been assigned to a rack.
        rack := Rack
    }
) when
    ?is_int32(BrokerId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_nullable_string(Rack)
->
    [
        ?encode_int32(BrokerId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_cluster_broker_1(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        host => string,
        port => int32,
        rack => nullable_string
    }).

-spec decode_describe_cluster_broker_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_cluster_broker_1(),
    Rest :: binary().

decode_describe_cluster_broker_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_int32(Port, Bin2, Bin3),
    ?_decode_compact_nullable_string(Rack, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_describe_cluster_broker_1_tagged_field/3,
        #{
            broker_id => BrokerId,
            host => Host,
            port => Port,
            rack => Rack
        },
        Bin4
    ).

-spec decode_describe_cluster_broker_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_cluster_broker_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_cluster_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    cluster_id := binary(),
    controller_id := integer(),
    brokers := list(describe_cluster_broker_0()),
    cluster_authorized_operations := integer()
}.
-type describe_cluster_broker_0() :: #{
    broker_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
-type describe_cluster_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    endpoint_type := integer(),
    cluster_id := binary(),
    controller_id := integer(),
    brokers := list(describe_cluster_broker_1()),
    cluster_authorized_operations := integer()
}.
-type describe_cluster_broker_1() :: #{
    broker_id := integer(),
    host := binary(),
    port := integer(),
    rack := binary() | null
}.
