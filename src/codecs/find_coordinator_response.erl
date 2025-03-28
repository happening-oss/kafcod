-module(find_coordinator_response).
-export([
    encode_find_coordinator_response_0/1,
    decode_find_coordinator_response_0/1,
    encode_find_coordinator_response_1/1,
    decode_find_coordinator_response_1/1,
    encode_find_coordinator_response_2/1,
    decode_find_coordinator_response_2/1,
    encode_find_coordinator_response_3/1,
    decode_find_coordinator_response_3/1,
    encode_find_coordinator_response_4/1,
    decode_find_coordinator_response_4/1,
    encode_find_coordinator_response_5/1,
    decode_find_coordinator_response_5/1
]).
-export_type([
    find_coordinator_response_0/0,
    find_coordinator_response_1/0,
    find_coordinator_response_2/0,
    find_coordinator_response_3/0,
    find_coordinator_response_4/0,
    coordinator_4/0,
    find_coordinator_response_5/0,
    coordinator_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_find_coordinator_response_0(find_coordinator_response_0()) -> iodata().

encode_find_coordinator_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The node id.
        node_id := NodeId,
        % The host name.
        host := Host,
        % The port.
        port := Port
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port)
    ];
encode_find_coordinator_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        node_id => int32,
        host => string,
        port => int32
    }).

-spec decode_find_coordinator_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_response_0(),
    Rest :: binary().

decode_find_coordinator_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_int32(NodeId, Bin1, Bin2),
    ?_decode_string(Host, Bin2, Bin3),
    ?_decode_int32(Port, Bin3, Bin4),
    {
        Header#{
            error_code => ErrorCode,
            node_id => NodeId,
            host => Host,
            port => Port
        },
        Bin4
    }.

-spec encode_find_coordinator_response_1(find_coordinator_response_1()) -> iodata().

encode_find_coordinator_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % The node id.
        node_id := NodeId,
        % The host name.
        host := Host,
        % The port.
        port := Port
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port)
    ];
encode_find_coordinator_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        node_id => int32,
        host => string,
        port => int32
    }).

-spec decode_find_coordinator_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_response_1(),
    Rest :: binary().

decode_find_coordinator_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_int32(NodeId, Bin3, Bin4),
    ?_decode_string(Host, Bin4, Bin5),
    ?_decode_int32(Port, Bin5, Bin6),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            node_id => NodeId,
            host => Host,
            port => Port
        },
        Bin6
    }.

-spec encode_find_coordinator_response_2(find_coordinator_response_2()) -> iodata().

encode_find_coordinator_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % The node id.
        node_id := NodeId,
        % The host name.
        host := Host,
        % The port.
        port := Port
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage),
        ?encode_int32(NodeId),
        ?encode_string(Host),
        ?encode_int32(Port)
    ];
encode_find_coordinator_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        node_id => int32,
        host => string,
        port => int32
    }).

-spec decode_find_coordinator_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_response_2(),
    Rest :: binary().

decode_find_coordinator_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_int32(NodeId, Bin3, Bin4),
    ?_decode_string(Host, Bin4, Bin5),
    ?_decode_int32(Port, Bin5, Bin6),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            node_id => NodeId,
            host => Host,
            port => Port
        },
        Bin6
    }.

-spec encode_find_coordinator_response_3(find_coordinator_response_3()) -> iodata().

encode_find_coordinator_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage,
        % The node id.
        node_id := NodeId,
        % The host name.
        host := Host,
        % The port.
        port := Port
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_int32(NodeId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?EMPTY_TAG_BUFFER
    ];
encode_find_coordinator_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        node_id => int32,
        host => string,
        port => int32
    }).

-spec decode_find_coordinator_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_response_3(),
    Rest :: binary().

decode_find_coordinator_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_int32(NodeId, Bin3, Bin4),
    ?_decode_compact_string(Host, Bin4, Bin5),
    ?_decode_int32(Port, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_find_coordinator_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            node_id => NodeId,
            host => Host,
            port => Port
        },
        Bin6
    ).

-spec decode_find_coordinator_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_find_coordinator_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_find_coordinator_response_4(find_coordinator_response_4()) -> iodata().

encode_find_coordinator_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each coordinator result in the response
        coordinators := Coordinators
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Coordinators)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Coordinators, fun encode_coordinator_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_find_coordinator_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        coordinators => {array, coordinator_4}
    }).

-spec decode_find_coordinator_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_response_4(),
    Rest :: binary().

decode_find_coordinator_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Coordinators, Bin1, Bin2, ?_decode_element(decode_coordinator_4)),
    ?decode_tagged_fields(
        fun decode_find_coordinator_response_4_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            coordinators => Coordinators
        },
        Bin2
    ).

-spec decode_find_coordinator_response_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_find_coordinator_response_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_coordinator_4(coordinator_4()) -> iodata().

encode_coordinator_4(
    _Args = #{
        % The coordinator key.
        key := Key,
        % The node id.
        node_id := NodeId,
        % The host name.
        host := Host,
        % The port.
        port := Port,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Key),
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_compact_string(Key),
        ?encode_int32(NodeId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_coordinator_4(Args) ->
    ?encoder_error(Args, #{
        key => string,
        node_id => int32,
        host => string,
        port => int32,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_coordinator_4(binary()) -> {Decoded, Rest} when
    Decoded :: coordinator_4(),
    Rest :: binary().

decode_coordinator_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Key, Bin0, Bin1),
    ?_decode_int32(NodeId, Bin1, Bin2),
    ?_decode_compact_string(Host, Bin2, Bin3),
    ?_decode_int32(Port, Bin3, Bin4),
    ?_decode_int16(ErrorCode, Bin4, Bin5),
    ?_decode_compact_nullable_string(ErrorMessage, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_coordinator_4_tagged_field/3,
        #{
            key => Key,
            node_id => NodeId,
            host => Host,
            port => Port,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin6
    ).

-spec decode_coordinator_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_coordinator_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_find_coordinator_response_5(find_coordinator_response_5()) -> iodata().

encode_find_coordinator_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each coordinator result in the response
        coordinators := Coordinators
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Coordinators)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Coordinators, fun encode_coordinator_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_find_coordinator_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        coordinators => {array, coordinator_5}
    }).

-spec decode_find_coordinator_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: find_coordinator_response_5(),
    Rest :: binary().

decode_find_coordinator_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Coordinators, Bin1, Bin2, ?_decode_element(decode_coordinator_5)),
    ?decode_tagged_fields(
        fun decode_find_coordinator_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            coordinators => Coordinators
        },
        Bin2
    ).

-spec decode_find_coordinator_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_find_coordinator_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_coordinator_5(coordinator_5()) -> iodata().

encode_coordinator_5(
    _Args = #{
        % The coordinator key.
        key := Key,
        % The node id.
        node_id := NodeId,
        % The host name.
        host := Host,
        % The port.
        port := Port,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_string(Key),
    ?is_int32(NodeId),
    ?is_string(Host),
    ?is_int32(Port),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_compact_string(Key),
        ?encode_int32(NodeId),
        ?encode_compact_string(Host),
        ?encode_int32(Port),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_coordinator_5(Args) ->
    ?encoder_error(Args, #{
        key => string,
        node_id => int32,
        host => string,
        port => int32,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_coordinator_5(binary()) -> {Decoded, Rest} when
    Decoded :: coordinator_5(),
    Rest :: binary().

decode_coordinator_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Key, Bin0, Bin1),
    ?_decode_int32(NodeId, Bin1, Bin2),
    ?_decode_compact_string(Host, Bin2, Bin3),
    ?_decode_int32(Port, Bin3, Bin4),
    ?_decode_int16(ErrorCode, Bin4, Bin5),
    ?_decode_compact_nullable_string(ErrorMessage, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_coordinator_5_tagged_field/3,
        #{
            key => Key,
            node_id => NodeId,
            host => Host,
            port => Port,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin6
    ).

-spec decode_coordinator_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_coordinator_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type find_coordinator_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    node_id := integer(),
    host := binary(),
    port := integer()
}.
-type find_coordinator_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    node_id := integer(),
    host := binary(),
    port := integer()
}.
-type find_coordinator_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    node_id := integer(),
    host := binary(),
    port := integer()
}.
-type find_coordinator_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    node_id := integer(),
    host := binary(),
    port := integer()
}.
-type find_coordinator_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    coordinators := list(coordinator_4())
}.
-type coordinator_4() :: #{
    key := binary(),
    node_id := integer(),
    host := binary(),
    port := integer(),
    error_code := integer(),
    error_message := binary() | null
}.
-type find_coordinator_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    coordinators := list(coordinator_5())
}.
-type coordinator_5() :: #{
    key := binary(),
    node_id := integer(),
    host := binary(),
    port := integer(),
    error_code := integer(),
    error_message := binary() | null
}.
