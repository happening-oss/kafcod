-module(join_group_request).
-export([
    encode_join_group_request_0/1,
    decode_join_group_request_0/1,
    encode_join_group_request_1/1,
    decode_join_group_request_1/1,
    encode_join_group_request_2/1,
    decode_join_group_request_2/1,
    encode_join_group_request_3/1,
    decode_join_group_request_3/1,
    encode_join_group_request_4/1,
    decode_join_group_request_4/1,
    encode_join_group_request_5/1,
    decode_join_group_request_5/1,
    encode_join_group_request_6/1,
    decode_join_group_request_6/1,
    encode_join_group_request_7/1,
    decode_join_group_request_7/1,
    encode_join_group_request_8/1,
    decode_join_group_request_8/1,
    encode_join_group_request_9/1,
    decode_join_group_request_9/1
]).
-export_type([
    join_group_request_0/0,
    join_group_request_protocol_0/0,
    join_group_request_1/0,
    join_group_request_protocol_1/0,
    join_group_request_2/0,
    join_group_request_protocol_2/0,
    join_group_request_3/0,
    join_group_request_protocol_3/0,
    join_group_request_4/0,
    join_group_request_protocol_4/0,
    join_group_request_5/0,
    join_group_request_protocol_5/0,
    join_group_request_6/0,
    join_group_request_protocol_6/0,
    join_group_request_7/0,
    join_group_request_protocol_7/0,
    join_group_request_8/0,
    join_group_request_protocol_8/0,
    join_group_request_9/0,
    join_group_request_protocol_9/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(JOIN_GROUP_REQUEST, 11).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_join_group_request_0(join_group_request_0()) -> iodata().

encode_join_group_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_string(MemberId),
    ?is_string(ProtocolType),
    ?is_array(Protocols)
->
    [
        ?encode_request_header_1(?JOIN_GROUP_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_string(MemberId),
        ?encode_string(ProtocolType),
        ?encode_array(Protocols, fun encode_join_group_request_protocol_0/1)
    ];
encode_join_group_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        member_id => string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_0}
    }).

-spec decode_join_group_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_0(),
    Rest :: binary().

decode_join_group_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_string(ProtocolType, Bin3, Bin4),
    ?_decode_array(Protocols, Bin4, Bin5, ?_decode_element(decode_join_group_request_protocol_0)),
    {
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            member_id => MemberId,
            protocol_type => ProtocolType,
            protocols => Protocols
        },
        Bin5
    }.

-spec encode_join_group_request_protocol_0(join_group_request_protocol_0()) -> iodata().

encode_join_group_request_protocol_0(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(Name),
        ?encode_bytes(Metadata)
    ];
encode_join_group_request_protocol_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_0(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_0(),
    Rest :: binary().

decode_join_group_request_protocol_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_request_1(join_group_request_1()) -> iodata().

encode_join_group_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_string(ProtocolType),
    ?is_array(Protocols)
->
    [
        ?encode_request_header_1(?JOIN_GROUP_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_string(MemberId),
        ?encode_string(ProtocolType),
        ?encode_array(Protocols, fun encode_join_group_request_protocol_1/1)
    ];
encode_join_group_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_1}
    }).

-spec decode_join_group_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_1(),
    Rest :: binary().

decode_join_group_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_string(MemberId, Bin3, Bin4),
    ?_decode_string(ProtocolType, Bin4, Bin5),
    ?_decode_array(Protocols, Bin5, Bin6, ?_decode_element(decode_join_group_request_protocol_1)),
    {
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            protocol_type => ProtocolType,
            protocols => Protocols
        },
        Bin6
    }.

-spec encode_join_group_request_protocol_1(join_group_request_protocol_1()) -> iodata().

encode_join_group_request_protocol_1(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(Name),
        ?encode_bytes(Metadata)
    ];
encode_join_group_request_protocol_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_1(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_1(),
    Rest :: binary().

decode_join_group_request_protocol_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_request_2(join_group_request_2()) -> iodata().

encode_join_group_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_string(ProtocolType),
    ?is_array(Protocols)
->
    [
        ?encode_request_header_1(?JOIN_GROUP_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_string(MemberId),
        ?encode_string(ProtocolType),
        ?encode_array(Protocols, fun encode_join_group_request_protocol_2/1)
    ];
encode_join_group_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_2}
    }).

-spec decode_join_group_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_2(),
    Rest :: binary().

decode_join_group_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_string(MemberId, Bin3, Bin4),
    ?_decode_string(ProtocolType, Bin4, Bin5),
    ?_decode_array(Protocols, Bin5, Bin6, ?_decode_element(decode_join_group_request_protocol_2)),
    {
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            protocol_type => ProtocolType,
            protocols => Protocols
        },
        Bin6
    }.

-spec encode_join_group_request_protocol_2(join_group_request_protocol_2()) -> iodata().

encode_join_group_request_protocol_2(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(Name),
        ?encode_bytes(Metadata)
    ];
encode_join_group_request_protocol_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_2(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_2(),
    Rest :: binary().

decode_join_group_request_protocol_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_request_3(join_group_request_3()) -> iodata().

encode_join_group_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_string(ProtocolType),
    ?is_array(Protocols)
->
    [
        ?encode_request_header_1(?JOIN_GROUP_REQUEST, 3, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_string(MemberId),
        ?encode_string(ProtocolType),
        ?encode_array(Protocols, fun encode_join_group_request_protocol_3/1)
    ];
encode_join_group_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_3}
    }).

-spec decode_join_group_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_3(),
    Rest :: binary().

decode_join_group_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_string(MemberId, Bin3, Bin4),
    ?_decode_string(ProtocolType, Bin4, Bin5),
    ?_decode_array(Protocols, Bin5, Bin6, ?_decode_element(decode_join_group_request_protocol_3)),
    {
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            protocol_type => ProtocolType,
            protocols => Protocols
        },
        Bin6
    }.

-spec encode_join_group_request_protocol_3(join_group_request_protocol_3()) -> iodata().

encode_join_group_request_protocol_3(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(Name),
        ?encode_bytes(Metadata)
    ];
encode_join_group_request_protocol_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_3(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_3(),
    Rest :: binary().

decode_join_group_request_protocol_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_request_4(join_group_request_4()) -> iodata().

encode_join_group_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_string(ProtocolType),
    ?is_array(Protocols)
->
    [
        ?encode_request_header_1(?JOIN_GROUP_REQUEST, 4, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_string(MemberId),
        ?encode_string(ProtocolType),
        ?encode_array(Protocols, fun encode_join_group_request_protocol_4/1)
    ];
encode_join_group_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_4}
    }).

-spec decode_join_group_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_4(),
    Rest :: binary().

decode_join_group_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_string(MemberId, Bin3, Bin4),
    ?_decode_string(ProtocolType, Bin4, Bin5),
    ?_decode_array(Protocols, Bin5, Bin6, ?_decode_element(decode_join_group_request_protocol_4)),
    {
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            protocol_type => ProtocolType,
            protocols => Protocols
        },
        Bin6
    }.

-spec encode_join_group_request_protocol_4(join_group_request_protocol_4()) -> iodata().

encode_join_group_request_protocol_4(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(Name),
        ?encode_bytes(Metadata)
    ];
encode_join_group_request_protocol_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_4(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_4(),
    Rest :: binary().

decode_join_group_request_protocol_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_request_5(join_group_request_5()) -> iodata().

encode_join_group_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_string(ProtocolType),
    ?is_array(Protocols)
->
    [
        ?encode_request_header_1(?JOIN_GROUP_REQUEST, 5, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_string(MemberId),
        ?encode_nullable_string(GroupInstanceId),
        ?encode_string(ProtocolType),
        ?encode_array(Protocols, fun encode_join_group_request_protocol_5/1)
    ];
encode_join_group_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        group_instance_id => nullable_string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_5}
    }).

-spec decode_join_group_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_5(),
    Rest :: binary().

decode_join_group_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_string(MemberId, Bin3, Bin4),
    ?_decode_nullable_string(GroupInstanceId, Bin4, Bin5),
    ?_decode_string(ProtocolType, Bin5, Bin6),
    ?_decode_array(Protocols, Bin6, Bin7, ?_decode_element(decode_join_group_request_protocol_5)),
    {
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            protocol_type => ProtocolType,
            protocols => Protocols
        },
        Bin7
    }.

-spec encode_join_group_request_protocol_5(join_group_request_protocol_5()) -> iodata().

encode_join_group_request_protocol_5(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_string(Name),
        ?encode_bytes(Metadata)
    ];
encode_join_group_request_protocol_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_5(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_5(),
    Rest :: binary().

decode_join_group_request_protocol_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_bytes(Metadata, Bin1, Bin2),
    {
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    }.

-spec encode_join_group_request_6(join_group_request_6()) -> iodata().

encode_join_group_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_string(ProtocolType),
    ?is_array(Protocols)
->
    [
        ?encode_request_header_2(?JOIN_GROUP_REQUEST, 6, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_string(ProtocolType),
        ?encode_compact_array(Protocols, fun encode_join_group_request_protocol_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        group_instance_id => nullable_string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_6}
    }).

-spec decode_join_group_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_6(),
    Rest :: binary().

decode_join_group_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_compact_string(MemberId, Bin3, Bin4),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin4, Bin5),
    ?_decode_compact_string(ProtocolType, Bin5, Bin6),
    ?_decode_compact_array(Protocols, Bin6, Bin7, ?_decode_element(decode_join_group_request_protocol_6)),
    ?decode_tagged_fields(
        fun decode_join_group_request_6_tagged_field/3,
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            protocol_type => ProtocolType,
            protocols => Protocols
        },
        Bin7
    ).

-spec decode_join_group_request_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_request_6().

decode_join_group_request_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_request_protocol_6(join_group_request_protocol_6()) -> iodata().

encode_join_group_request_protocol_6(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_bytes(Metadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_request_protocol_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_6(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_6(),
    Rest :: binary().

decode_join_group_request_protocol_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_bytes(Metadata, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_join_group_request_protocol_6_tagged_field/3,
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    ).

-spec decode_join_group_request_protocol_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_request_protocol_6().

decode_join_group_request_protocol_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_request_7(join_group_request_7()) -> iodata().

encode_join_group_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_string(ProtocolType),
    ?is_array(Protocols)
->
    [
        ?encode_request_header_2(?JOIN_GROUP_REQUEST, 7, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_string(ProtocolType),
        ?encode_compact_array(Protocols, fun encode_join_group_request_protocol_7/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        group_instance_id => nullable_string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_7}
    }).

-spec decode_join_group_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_7(),
    Rest :: binary().

decode_join_group_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_compact_string(MemberId, Bin3, Bin4),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin4, Bin5),
    ?_decode_compact_string(ProtocolType, Bin5, Bin6),
    ?_decode_compact_array(Protocols, Bin6, Bin7, ?_decode_element(decode_join_group_request_protocol_7)),
    ?decode_tagged_fields(
        fun decode_join_group_request_7_tagged_field/3,
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            protocol_type => ProtocolType,
            protocols => Protocols
        },
        Bin7
    ).

-spec decode_join_group_request_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_request_7().

decode_join_group_request_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_request_protocol_7(join_group_request_protocol_7()) -> iodata().

encode_join_group_request_protocol_7(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_bytes(Metadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_request_protocol_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_7(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_7(),
    Rest :: binary().

decode_join_group_request_protocol_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_bytes(Metadata, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_join_group_request_protocol_7_tagged_field/3,
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    ).

-spec decode_join_group_request_protocol_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_request_protocol_7().

decode_join_group_request_protocol_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_request_8(join_group_request_8()) -> iodata().

encode_join_group_request_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols,
        % The reason why the member (re-)joins the group.
        reason := Reason
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_string(ProtocolType),
    ?is_array(Protocols),
    ?is_nullable_string(Reason)
->
    [
        ?encode_request_header_2(?JOIN_GROUP_REQUEST, 8, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_string(ProtocolType),
        ?encode_compact_array(Protocols, fun encode_join_group_request_protocol_8/1),
        ?encode_compact_nullable_string(Reason),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_request_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        group_instance_id => nullable_string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_8},
        reason => nullable_string
    }).

-spec decode_join_group_request_8(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_8(),
    Rest :: binary().

decode_join_group_request_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_compact_string(MemberId, Bin3, Bin4),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin4, Bin5),
    ?_decode_compact_string(ProtocolType, Bin5, Bin6),
    ?_decode_compact_array(Protocols, Bin6, Bin7, ?_decode_element(decode_join_group_request_protocol_8)),
    ?_decode_compact_nullable_string(Reason, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_join_group_request_8_tagged_field/3,
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            protocol_type => ProtocolType,
            protocols => Protocols,
            reason => Reason
        },
        Bin8
    ).

-spec decode_join_group_request_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_request_8().

decode_join_group_request_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_request_protocol_8(join_group_request_protocol_8()) -> iodata().

encode_join_group_request_protocol_8(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_bytes(Metadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_request_protocol_8(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_8(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_8(),
    Rest :: binary().

decode_join_group_request_protocol_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_bytes(Metadata, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_join_group_request_protocol_8_tagged_field/3,
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    ).

-spec decode_join_group_request_protocol_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_request_protocol_8().

decode_join_group_request_protocol_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_request_9(join_group_request_9()) -> iodata().

encode_join_group_request_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group identifier.
        group_id := GroupId,
        % The coordinator considers the consumer dead if it receives no heartbeat after this timeout in milliseconds.
        session_timeout_ms := SessionTimeoutMs,
        % The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebalancing the group.
        rebalance_timeout_ms := RebalanceTimeoutMs,
        % The member id assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The unique name the for class of protocols implemented by the group we want to join.
        protocol_type := ProtocolType,
        % The list of protocols that the member supports.
        protocols := Protocols,
        % The reason why the member (re-)joins the group.
        reason := Reason
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(SessionTimeoutMs),
    ?is_int32(RebalanceTimeoutMs),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_string(ProtocolType),
    ?is_array(Protocols),
    ?is_nullable_string(Reason)
->
    [
        ?encode_request_header_2(?JOIN_GROUP_REQUEST, 9, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(SessionTimeoutMs),
        ?encode_int32(RebalanceTimeoutMs),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_string(ProtocolType),
        ?encode_compact_array(Protocols, fun encode_join_group_request_protocol_9/1),
        ?encode_compact_nullable_string(Reason),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_request_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        session_timeout_ms => int32,
        rebalance_timeout_ms => int32,
        member_id => string,
        group_instance_id => nullable_string,
        protocol_type => string,
        protocols => {array, join_group_request_protocol_9},
        reason => nullable_string
    }).

-spec decode_join_group_request_9(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_9(),
    Rest :: binary().

decode_join_group_request_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(SessionTimeoutMs, Bin1, Bin2),
    ?_decode_int32(RebalanceTimeoutMs, Bin2, Bin3),
    ?_decode_compact_string(MemberId, Bin3, Bin4),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin4, Bin5),
    ?_decode_compact_string(ProtocolType, Bin5, Bin6),
    ?_decode_compact_array(Protocols, Bin6, Bin7, ?_decode_element(decode_join_group_request_protocol_9)),
    ?_decode_compact_nullable_string(Reason, Bin7, Bin8),
    ?decode_tagged_fields(
        fun decode_join_group_request_9_tagged_field/3,
        Header#{
            group_id => GroupId,
            session_timeout_ms => SessionTimeoutMs,
            rebalance_timeout_ms => RebalanceTimeoutMs,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            protocol_type => ProtocolType,
            protocols => Protocols,
            reason => Reason
        },
        Bin8
    ).

-spec decode_join_group_request_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_request_9().

decode_join_group_request_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_join_group_request_protocol_9(join_group_request_protocol_9()) -> iodata().

encode_join_group_request_protocol_9(
    _Args = #{
        % The protocol name.
        name := Name,
        % The protocol metadata.
        metadata := Metadata
    }
) when
    ?is_string(Name),
    ?is_bytes(Metadata)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_bytes(Metadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_join_group_request_protocol_9(Args) ->
    ?encoder_error(Args, #{
        name => string,
        metadata => bytes
    }).

-spec decode_join_group_request_protocol_9(binary()) -> {Decoded, Rest} when
    Decoded :: join_group_request_protocol_9(),
    Rest :: binary().

decode_join_group_request_protocol_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_bytes(Metadata, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_join_group_request_protocol_9_tagged_field/3,
        #{
            name => Name,
            metadata => Metadata
        },
        Bin2
    ).

-spec decode_join_group_request_protocol_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: join_group_request_protocol_9().

decode_join_group_request_protocol_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type join_group_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    member_id := binary(),
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_0())
}.
-type join_group_request_protocol_0() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_1())
}.
-type join_group_request_protocol_1() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_2())
}.
-type join_group_request_protocol_2() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_3())
}.
-type join_group_request_protocol_3() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_4())
}.
-type join_group_request_protocol_4() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_5())
}.
-type join_group_request_protocol_5() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_6())
}.
-type join_group_request_protocol_6() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_7())
}.
-type join_group_request_protocol_7() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_8() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_8()),
    reason := binary() | null
}.
-type join_group_request_protocol_8() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
-type join_group_request_9() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    session_timeout_ms := integer(),
    rebalance_timeout_ms := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    protocol_type := binary(),
    protocols := list(join_group_request_protocol_9()),
    reason := binary() | null
}.
-type join_group_request_protocol_9() :: #{
    name := binary(),
    metadata := kafcod:bytes()
}.
