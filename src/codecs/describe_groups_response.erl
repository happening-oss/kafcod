-module(describe_groups_response).
-export([
    encode_describe_groups_response_0/1,
    decode_describe_groups_response_0/1,
    encode_describe_groups_response_1/1,
    decode_describe_groups_response_1/1,
    encode_describe_groups_response_2/1,
    decode_describe_groups_response_2/1,
    encode_describe_groups_response_3/1,
    decode_describe_groups_response_3/1,
    encode_describe_groups_response_4/1,
    decode_describe_groups_response_4/1,
    encode_describe_groups_response_5/1,
    decode_describe_groups_response_5/1
]).
-export_type([
    describe_groups_response_0/0,
    described_group_member_0/0,
    described_group_0/0,
    describe_groups_response_1/0,
    described_group_member_1/0,
    described_group_1/0,
    describe_groups_response_2/0,
    described_group_member_2/0,
    described_group_2/0,
    describe_groups_response_3/0,
    described_group_member_3/0,
    described_group_3/0,
    describe_groups_response_4/0,
    described_group_member_4/0,
    described_group_4/0,
    describe_groups_response_5/0,
    described_group_member_5/0,
    described_group_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_groups_response_0(describe_groups_response_0()) -> iodata().

encode_describe_groups_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % Each described group.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_array(Groups)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_array(Groups, fun encode_described_group_0/1)
    ];
encode_describe_groups_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        groups => {array, described_group_0}
    }).

-spec decode_describe_groups_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_response_0(),
    Rest :: binary().

decode_describe_groups_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_array(Groups, Bin0, Bin1, ?_decode_element(decode_described_group_0)),
    {
        Header#{
            groups => Groups
        },
        Bin1
    }.

-spec encode_described_group_member_0(described_group_member_0()) -> iodata().

encode_described_group_member_0(
    _Args = #{
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The client ID used in the member's latest join group request.
        client_id := ClientId,
        % The client host.
        client_host := ClientHost,
        % The metadata corresponding to the current group protocol in use.
        member_metadata := MemberMetadata,
        % The current assignment provided by the group leader.
        member_assignment := MemberAssignment
    }
) when
    ?is_string(MemberId),
    ?is_string(ClientId),
    ?is_string(ClientHost),
    ?is_bytes(MemberMetadata),
    ?is_bytes(MemberAssignment)
->
    [
        ?encode_string(MemberId),
        ?encode_string(ClientId),
        ?encode_string(ClientHost),
        ?encode_bytes(MemberMetadata),
        ?encode_bytes(MemberAssignment)
    ];
encode_described_group_member_0(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        client_id => string,
        client_host => string,
        member_metadata => bytes,
        member_assignment => bytes
    }).

-spec decode_described_group_member_0(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_member_0(),
    Rest :: binary().

decode_described_group_member_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_string(ClientId, Bin1, Bin2),
    ?_decode_string(ClientHost, Bin2, Bin3),
    ?_decode_bytes(MemberMetadata, Bin3, Bin4),
    ?_decode_bytes(MemberAssignment, Bin4, Bin5),
    {
        #{
            member_id => MemberId,
            client_id => ClientId,
            client_host => ClientHost,
            member_metadata => MemberMetadata,
            member_assignment => MemberAssignment
        },
        Bin5
    }.

-spec encode_described_group_0(described_group_0()) -> iodata().

encode_described_group_0(
    _Args = #{
        % The describe error, or 0 if there was no error.
        error_code := ErrorCode,
        % The group ID string.
        group_id := GroupId,
        % The group state string, or the empty string.
        group_state := GroupState,
        % The group protocol type, or the empty string.
        protocol_type := ProtocolType,
        % The group protocol data, or the empty string.
        protocol_data := ProtocolData,
        % The group members.
        members := Members
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(GroupId),
    ?is_string(GroupState),
    ?is_string(ProtocolType),
    ?is_string(ProtocolData),
    ?is_array(Members)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(GroupId),
        ?encode_string(GroupState),
        ?encode_string(ProtocolType),
        ?encode_string(ProtocolData),
        ?encode_array(Members, fun encode_described_group_member_0/1)
    ];
encode_described_group_0(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        group_id => string,
        group_state => string,
        protocol_type => string,
        protocol_data => string,
        members => {array, described_group_member_0}
    }).

-spec decode_described_group_0(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_0(),
    Rest :: binary().

decode_described_group_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(GroupId, Bin1, Bin2),
    ?_decode_string(GroupState, Bin2, Bin3),
    ?_decode_string(ProtocolType, Bin3, Bin4),
    ?_decode_string(ProtocolData, Bin4, Bin5),
    ?_decode_array(Members, Bin5, Bin6, ?_decode_element(decode_described_group_member_0)),
    {
        #{
            error_code => ErrorCode,
            group_id => GroupId,
            group_state => GroupState,
            protocol_type => ProtocolType,
            protocol_data => ProtocolData,
            members => Members
        },
        Bin6
    }.

-spec encode_describe_groups_response_1(describe_groups_response_1()) -> iodata().

encode_describe_groups_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each described group.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Groups)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Groups, fun encode_described_group_1/1)
    ];
encode_describe_groups_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        groups => {array, described_group_1}
    }).

-spec decode_describe_groups_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_response_1(),
    Rest :: binary().

decode_describe_groups_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Groups, Bin1, Bin2, ?_decode_element(decode_described_group_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            groups => Groups
        },
        Bin2
    }.

-spec encode_described_group_member_1(described_group_member_1()) -> iodata().

encode_described_group_member_1(
    _Args = #{
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The client ID used in the member's latest join group request.
        client_id := ClientId,
        % The client host.
        client_host := ClientHost,
        % The metadata corresponding to the current group protocol in use.
        member_metadata := MemberMetadata,
        % The current assignment provided by the group leader.
        member_assignment := MemberAssignment
    }
) when
    ?is_string(MemberId),
    ?is_string(ClientId),
    ?is_string(ClientHost),
    ?is_bytes(MemberMetadata),
    ?is_bytes(MemberAssignment)
->
    [
        ?encode_string(MemberId),
        ?encode_string(ClientId),
        ?encode_string(ClientHost),
        ?encode_bytes(MemberMetadata),
        ?encode_bytes(MemberAssignment)
    ];
encode_described_group_member_1(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        client_id => string,
        client_host => string,
        member_metadata => bytes,
        member_assignment => bytes
    }).

-spec decode_described_group_member_1(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_member_1(),
    Rest :: binary().

decode_described_group_member_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_string(ClientId, Bin1, Bin2),
    ?_decode_string(ClientHost, Bin2, Bin3),
    ?_decode_bytes(MemberMetadata, Bin3, Bin4),
    ?_decode_bytes(MemberAssignment, Bin4, Bin5),
    {
        #{
            member_id => MemberId,
            client_id => ClientId,
            client_host => ClientHost,
            member_metadata => MemberMetadata,
            member_assignment => MemberAssignment
        },
        Bin5
    }.

-spec encode_described_group_1(described_group_1()) -> iodata().

encode_described_group_1(
    _Args = #{
        % The describe error, or 0 if there was no error.
        error_code := ErrorCode,
        % The group ID string.
        group_id := GroupId,
        % The group state string, or the empty string.
        group_state := GroupState,
        % The group protocol type, or the empty string.
        protocol_type := ProtocolType,
        % The group protocol data, or the empty string.
        protocol_data := ProtocolData,
        % The group members.
        members := Members
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(GroupId),
    ?is_string(GroupState),
    ?is_string(ProtocolType),
    ?is_string(ProtocolData),
    ?is_array(Members)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(GroupId),
        ?encode_string(GroupState),
        ?encode_string(ProtocolType),
        ?encode_string(ProtocolData),
        ?encode_array(Members, fun encode_described_group_member_1/1)
    ];
encode_described_group_1(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        group_id => string,
        group_state => string,
        protocol_type => string,
        protocol_data => string,
        members => {array, described_group_member_1}
    }).

-spec decode_described_group_1(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_1(),
    Rest :: binary().

decode_described_group_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(GroupId, Bin1, Bin2),
    ?_decode_string(GroupState, Bin2, Bin3),
    ?_decode_string(ProtocolType, Bin3, Bin4),
    ?_decode_string(ProtocolData, Bin4, Bin5),
    ?_decode_array(Members, Bin5, Bin6, ?_decode_element(decode_described_group_member_1)),
    {
        #{
            error_code => ErrorCode,
            group_id => GroupId,
            group_state => GroupState,
            protocol_type => ProtocolType,
            protocol_data => ProtocolData,
            members => Members
        },
        Bin6
    }.

-spec encode_describe_groups_response_2(describe_groups_response_2()) -> iodata().

encode_describe_groups_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each described group.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Groups)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Groups, fun encode_described_group_2/1)
    ];
encode_describe_groups_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        groups => {array, described_group_2}
    }).

-spec decode_describe_groups_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_response_2(),
    Rest :: binary().

decode_describe_groups_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Groups, Bin1, Bin2, ?_decode_element(decode_described_group_2)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            groups => Groups
        },
        Bin2
    }.

-spec encode_described_group_member_2(described_group_member_2()) -> iodata().

encode_described_group_member_2(
    _Args = #{
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The client ID used in the member's latest join group request.
        client_id := ClientId,
        % The client host.
        client_host := ClientHost,
        % The metadata corresponding to the current group protocol in use.
        member_metadata := MemberMetadata,
        % The current assignment provided by the group leader.
        member_assignment := MemberAssignment
    }
) when
    ?is_string(MemberId),
    ?is_string(ClientId),
    ?is_string(ClientHost),
    ?is_bytes(MemberMetadata),
    ?is_bytes(MemberAssignment)
->
    [
        ?encode_string(MemberId),
        ?encode_string(ClientId),
        ?encode_string(ClientHost),
        ?encode_bytes(MemberMetadata),
        ?encode_bytes(MemberAssignment)
    ];
encode_described_group_member_2(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        client_id => string,
        client_host => string,
        member_metadata => bytes,
        member_assignment => bytes
    }).

-spec decode_described_group_member_2(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_member_2(),
    Rest :: binary().

decode_described_group_member_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_string(ClientId, Bin1, Bin2),
    ?_decode_string(ClientHost, Bin2, Bin3),
    ?_decode_bytes(MemberMetadata, Bin3, Bin4),
    ?_decode_bytes(MemberAssignment, Bin4, Bin5),
    {
        #{
            member_id => MemberId,
            client_id => ClientId,
            client_host => ClientHost,
            member_metadata => MemberMetadata,
            member_assignment => MemberAssignment
        },
        Bin5
    }.

-spec encode_described_group_2(described_group_2()) -> iodata().

encode_described_group_2(
    _Args = #{
        % The describe error, or 0 if there was no error.
        error_code := ErrorCode,
        % The group ID string.
        group_id := GroupId,
        % The group state string, or the empty string.
        group_state := GroupState,
        % The group protocol type, or the empty string.
        protocol_type := ProtocolType,
        % The group protocol data, or the empty string.
        protocol_data := ProtocolData,
        % The group members.
        members := Members
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(GroupId),
    ?is_string(GroupState),
    ?is_string(ProtocolType),
    ?is_string(ProtocolData),
    ?is_array(Members)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(GroupId),
        ?encode_string(GroupState),
        ?encode_string(ProtocolType),
        ?encode_string(ProtocolData),
        ?encode_array(Members, fun encode_described_group_member_2/1)
    ];
encode_described_group_2(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        group_id => string,
        group_state => string,
        protocol_type => string,
        protocol_data => string,
        members => {array, described_group_member_2}
    }).

-spec decode_described_group_2(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_2(),
    Rest :: binary().

decode_described_group_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(GroupId, Bin1, Bin2),
    ?_decode_string(GroupState, Bin2, Bin3),
    ?_decode_string(ProtocolType, Bin3, Bin4),
    ?_decode_string(ProtocolData, Bin4, Bin5),
    ?_decode_array(Members, Bin5, Bin6, ?_decode_element(decode_described_group_member_2)),
    {
        #{
            error_code => ErrorCode,
            group_id => GroupId,
            group_state => GroupState,
            protocol_type => ProtocolType,
            protocol_data => ProtocolData,
            members => Members
        },
        Bin6
    }.

-spec encode_describe_groups_response_3(describe_groups_response_3()) -> iodata().

encode_describe_groups_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each described group.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Groups)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Groups, fun encode_described_group_3/1)
    ];
encode_describe_groups_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        groups => {array, described_group_3}
    }).

-spec decode_describe_groups_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_response_3(),
    Rest :: binary().

decode_describe_groups_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Groups, Bin1, Bin2, ?_decode_element(decode_described_group_3)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            groups => Groups
        },
        Bin2
    }.

-spec encode_described_group_member_3(described_group_member_3()) -> iodata().

encode_described_group_member_3(
    _Args = #{
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The client ID used in the member's latest join group request.
        client_id := ClientId,
        % The client host.
        client_host := ClientHost,
        % The metadata corresponding to the current group protocol in use.
        member_metadata := MemberMetadata,
        % The current assignment provided by the group leader.
        member_assignment := MemberAssignment
    }
) when
    ?is_string(MemberId),
    ?is_string(ClientId),
    ?is_string(ClientHost),
    ?is_bytes(MemberMetadata),
    ?is_bytes(MemberAssignment)
->
    [
        ?encode_string(MemberId),
        ?encode_string(ClientId),
        ?encode_string(ClientHost),
        ?encode_bytes(MemberMetadata),
        ?encode_bytes(MemberAssignment)
    ];
encode_described_group_member_3(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        client_id => string,
        client_host => string,
        member_metadata => bytes,
        member_assignment => bytes
    }).

-spec decode_described_group_member_3(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_member_3(),
    Rest :: binary().

decode_described_group_member_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_string(ClientId, Bin1, Bin2),
    ?_decode_string(ClientHost, Bin2, Bin3),
    ?_decode_bytes(MemberMetadata, Bin3, Bin4),
    ?_decode_bytes(MemberAssignment, Bin4, Bin5),
    {
        #{
            member_id => MemberId,
            client_id => ClientId,
            client_host => ClientHost,
            member_metadata => MemberMetadata,
            member_assignment => MemberAssignment
        },
        Bin5
    }.

-spec encode_described_group_3(described_group_3()) -> iodata().

encode_described_group_3(
    _Args = #{
        % The describe error, or 0 if there was no error.
        error_code := ErrorCode,
        % The group ID string.
        group_id := GroupId,
        % The group state string, or the empty string.
        group_state := GroupState,
        % The group protocol type, or the empty string.
        protocol_type := ProtocolType,
        % The group protocol data, or the empty string.
        protocol_data := ProtocolData,
        % The group members.
        members := Members,
        % 32-bit bitfield to represent authorized operations for this group.
        authorized_operations := AuthorizedOperations
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(GroupId),
    ?is_string(GroupState),
    ?is_string(ProtocolType),
    ?is_string(ProtocolData),
    ?is_array(Members),
    ?is_int32(AuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(GroupId),
        ?encode_string(GroupState),
        ?encode_string(ProtocolType),
        ?encode_string(ProtocolData),
        ?encode_array(Members, fun encode_described_group_member_3/1),
        ?encode_int32(AuthorizedOperations)
    ];
encode_described_group_3(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        group_id => string,
        group_state => string,
        protocol_type => string,
        protocol_data => string,
        members => {array, described_group_member_3},
        authorized_operations => int32
    }).

-spec decode_described_group_3(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_3(),
    Rest :: binary().

decode_described_group_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(GroupId, Bin1, Bin2),
    ?_decode_string(GroupState, Bin2, Bin3),
    ?_decode_string(ProtocolType, Bin3, Bin4),
    ?_decode_string(ProtocolData, Bin4, Bin5),
    ?_decode_array(Members, Bin5, Bin6, ?_decode_element(decode_described_group_member_3)),
    ?_decode_int32(AuthorizedOperations, Bin6, Bin7),
    {
        #{
            error_code => ErrorCode,
            group_id => GroupId,
            group_state => GroupState,
            protocol_type => ProtocolType,
            protocol_data => ProtocolData,
            members => Members,
            authorized_operations => AuthorizedOperations
        },
        Bin7
    }.

-spec encode_describe_groups_response_4(describe_groups_response_4()) -> iodata().

encode_describe_groups_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each described group.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Groups)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(Groups, fun encode_described_group_4/1)
    ];
encode_describe_groups_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        groups => {array, described_group_4}
    }).

-spec decode_describe_groups_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_response_4(),
    Rest :: binary().

decode_describe_groups_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(Groups, Bin1, Bin2, ?_decode_element(decode_described_group_4)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            groups => Groups
        },
        Bin2
    }.

-spec encode_described_group_member_4(described_group_member_4()) -> iodata().

encode_described_group_member_4(
    _Args = #{
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The client ID used in the member's latest join group request.
        client_id := ClientId,
        % The client host.
        client_host := ClientHost,
        % The metadata corresponding to the current group protocol in use.
        member_metadata := MemberMetadata,
        % The current assignment provided by the group leader.
        member_assignment := MemberAssignment
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_string(ClientId),
    ?is_string(ClientHost),
    ?is_bytes(MemberMetadata),
    ?is_bytes(MemberAssignment)
->
    [
        ?encode_string(MemberId),
        ?encode_nullable_string(GroupInstanceId),
        ?encode_string(ClientId),
        ?encode_string(ClientHost),
        ?encode_bytes(MemberMetadata),
        ?encode_bytes(MemberAssignment)
    ];
encode_described_group_member_4(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        client_id => string,
        client_host => string,
        member_metadata => bytes,
        member_assignment => bytes
    }).

-spec decode_described_group_member_4(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_member_4(),
    Rest :: binary().

decode_described_group_member_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_string(ClientId, Bin2, Bin3),
    ?_decode_string(ClientHost, Bin3, Bin4),
    ?_decode_bytes(MemberMetadata, Bin4, Bin5),
    ?_decode_bytes(MemberAssignment, Bin5, Bin6),
    {
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            client_id => ClientId,
            client_host => ClientHost,
            member_metadata => MemberMetadata,
            member_assignment => MemberAssignment
        },
        Bin6
    }.

-spec encode_described_group_4(described_group_4()) -> iodata().

encode_described_group_4(
    _Args = #{
        % The describe error, or 0 if there was no error.
        error_code := ErrorCode,
        % The group ID string.
        group_id := GroupId,
        % The group state string, or the empty string.
        group_state := GroupState,
        % The group protocol type, or the empty string.
        protocol_type := ProtocolType,
        % The group protocol data, or the empty string.
        protocol_data := ProtocolData,
        % The group members.
        members := Members,
        % 32-bit bitfield to represent authorized operations for this group.
        authorized_operations := AuthorizedOperations
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(GroupId),
    ?is_string(GroupState),
    ?is_string(ProtocolType),
    ?is_string(ProtocolData),
    ?is_array(Members),
    ?is_int32(AuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_string(GroupId),
        ?encode_string(GroupState),
        ?encode_string(ProtocolType),
        ?encode_string(ProtocolData),
        ?encode_array(Members, fun encode_described_group_member_4/1),
        ?encode_int32(AuthorizedOperations)
    ];
encode_described_group_4(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        group_id => string,
        group_state => string,
        protocol_type => string,
        protocol_data => string,
        members => {array, described_group_member_4},
        authorized_operations => int32
    }).

-spec decode_described_group_4(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_4(),
    Rest :: binary().

decode_described_group_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_string(GroupId, Bin1, Bin2),
    ?_decode_string(GroupState, Bin2, Bin3),
    ?_decode_string(ProtocolType, Bin3, Bin4),
    ?_decode_string(ProtocolData, Bin4, Bin5),
    ?_decode_array(Members, Bin5, Bin6, ?_decode_element(decode_described_group_member_4)),
    ?_decode_int32(AuthorizedOperations, Bin6, Bin7),
    {
        #{
            error_code => ErrorCode,
            group_id => GroupId,
            group_state => GroupState,
            protocol_type => ProtocolType,
            protocol_data => ProtocolData,
            members => Members,
            authorized_operations => AuthorizedOperations
        },
        Bin7
    }.

-spec encode_describe_groups_response_5(describe_groups_response_5()) -> iodata().

encode_describe_groups_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % Each described group.
        groups := Groups
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(Groups)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_compact_array(Groups, fun encode_described_group_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_groups_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        groups => {array, described_group_5}
    }).

-spec decode_describe_groups_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: describe_groups_response_5(),
    Rest :: binary().

decode_describe_groups_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_compact_array(Groups, Bin1, Bin2, ?_decode_element(decode_described_group_5)),
    ?decode_tagged_fields(
        fun decode_describe_groups_response_5_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            groups => Groups
        },
        Bin2
    ).

-spec decode_describe_groups_response_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_groups_response_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_described_group_member_5(described_group_member_5()) -> iodata().

encode_described_group_member_5(
    _Args = #{
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The client ID used in the member's latest join group request.
        client_id := ClientId,
        % The client host.
        client_host := ClientHost,
        % The metadata corresponding to the current group protocol in use.
        member_metadata := MemberMetadata,
        % The current assignment provided by the group leader.
        member_assignment := MemberAssignment
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_string(ClientId),
    ?is_string(ClientHost),
    ?is_bytes(MemberMetadata),
    ?is_bytes(MemberAssignment)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_string(ClientId),
        ?encode_compact_string(ClientHost),
        ?encode_compact_bytes(MemberMetadata),
        ?encode_compact_bytes(MemberAssignment),
        ?EMPTY_TAG_BUFFER
    ];
encode_described_group_member_5(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        client_id => string,
        client_host => string,
        member_metadata => bytes,
        member_assignment => bytes
    }).

-spec decode_described_group_member_5(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_member_5(),
    Rest :: binary().

decode_described_group_member_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_compact_string(ClientId, Bin2, Bin3),
    ?_decode_compact_string(ClientHost, Bin3, Bin4),
    ?_decode_compact_bytes(MemberMetadata, Bin4, Bin5),
    ?_decode_compact_bytes(MemberAssignment, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_described_group_member_5_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            client_id => ClientId,
            client_host => ClientHost,
            member_metadata => MemberMetadata,
            member_assignment => MemberAssignment
        },
        Bin6
    ).

-spec decode_described_group_member_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_described_group_member_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_described_group_5(described_group_5()) -> iodata().

encode_described_group_5(
    _Args = #{
        % The describe error, or 0 if there was no error.
        error_code := ErrorCode,
        % The group ID string.
        group_id := GroupId,
        % The group state string, or the empty string.
        group_state := GroupState,
        % The group protocol type, or the empty string.
        protocol_type := ProtocolType,
        % The group protocol data, or the empty string.
        protocol_data := ProtocolData,
        % The group members.
        members := Members,
        % 32-bit bitfield to represent authorized operations for this group.
        authorized_operations := AuthorizedOperations
    }
) when
    ?is_int16(ErrorCode),
    ?is_string(GroupId),
    ?is_string(GroupState),
    ?is_string(ProtocolType),
    ?is_string(ProtocolData),
    ?is_array(Members),
    ?is_int32(AuthorizedOperations)
->
    [
        ?encode_int16(ErrorCode),
        ?encode_compact_string(GroupId),
        ?encode_compact_string(GroupState),
        ?encode_compact_string(ProtocolType),
        ?encode_compact_string(ProtocolData),
        ?encode_compact_array(Members, fun encode_described_group_member_5/1),
        ?encode_int32(AuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_described_group_5(Args) ->
    ?encoder_error(Args, #{
        error_code => int16,
        group_id => string,
        group_state => string,
        protocol_type => string,
        protocol_data => string,
        members => {array, described_group_member_5},
        authorized_operations => int32
    }).

-spec decode_described_group_5(binary()) -> {Decoded, Rest} when
    Decoded :: described_group_5(),
    Rest :: binary().

decode_described_group_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_string(GroupId, Bin1, Bin2),
    ?_decode_compact_string(GroupState, Bin2, Bin3),
    ?_decode_compact_string(ProtocolType, Bin3, Bin4),
    ?_decode_compact_string(ProtocolData, Bin4, Bin5),
    ?_decode_compact_array(Members, Bin5, Bin6, ?_decode_element(decode_described_group_member_5)),
    ?_decode_int32(AuthorizedOperations, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_described_group_5_tagged_field/3,
        #{
            error_code => ErrorCode,
            group_id => GroupId,
            group_state => GroupState,
            protocol_type => ProtocolType,
            protocol_data => ProtocolData,
            members => Members,
            authorized_operations => AuthorizedOperations
        },
        Bin7
    ).

-spec decode_described_group_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_described_group_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_groups_response_0() :: #{
    correlation_id => integer(),
    groups := list(described_group_0())
}.
-type described_group_member_0() :: #{
    member_id := binary(),
    client_id := binary(),
    client_host := binary(),
    member_metadata := kafcod:bytes(),
    member_assignment := kafcod:bytes()
}.
-type described_group_0() :: #{
    error_code := integer(),
    group_id := binary(),
    group_state := binary(),
    protocol_type := binary(),
    protocol_data := binary(),
    members := list(described_group_member_0())
}.
-type describe_groups_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    groups := list(described_group_1())
}.
-type described_group_member_1() :: #{
    member_id := binary(),
    client_id := binary(),
    client_host := binary(),
    member_metadata := kafcod:bytes(),
    member_assignment := kafcod:bytes()
}.
-type described_group_1() :: #{
    error_code := integer(),
    group_id := binary(),
    group_state := binary(),
    protocol_type := binary(),
    protocol_data := binary(),
    members := list(described_group_member_1())
}.
-type describe_groups_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    groups := list(described_group_2())
}.
-type described_group_member_2() :: #{
    member_id := binary(),
    client_id := binary(),
    client_host := binary(),
    member_metadata := kafcod:bytes(),
    member_assignment := kafcod:bytes()
}.
-type described_group_2() :: #{
    error_code := integer(),
    group_id := binary(),
    group_state := binary(),
    protocol_type := binary(),
    protocol_data := binary(),
    members := list(described_group_member_2())
}.
-type describe_groups_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    groups := list(described_group_3())
}.
-type described_group_member_3() :: #{
    member_id := binary(),
    client_id := binary(),
    client_host := binary(),
    member_metadata := kafcod:bytes(),
    member_assignment := kafcod:bytes()
}.
-type described_group_3() :: #{
    error_code := integer(),
    group_id := binary(),
    group_state := binary(),
    protocol_type := binary(),
    protocol_data := binary(),
    members := list(described_group_member_3()),
    authorized_operations := integer()
}.
-type describe_groups_response_4() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    groups := list(described_group_4())
}.
-type described_group_member_4() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    client_id := binary(),
    client_host := binary(),
    member_metadata := kafcod:bytes(),
    member_assignment := kafcod:bytes()
}.
-type described_group_4() :: #{
    error_code := integer(),
    group_id := binary(),
    group_state := binary(),
    protocol_type := binary(),
    protocol_data := binary(),
    members := list(described_group_member_4()),
    authorized_operations := integer()
}.
-type describe_groups_response_5() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    groups := list(described_group_5())
}.
-type described_group_member_5() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    client_id := binary(),
    client_host := binary(),
    member_metadata := kafcod:bytes(),
    member_assignment := kafcod:bytes()
}.
-type described_group_5() :: #{
    error_code := integer(),
    group_id := binary(),
    group_state := binary(),
    protocol_type := binary(),
    protocol_data := binary(),
    members := list(described_group_member_5()),
    authorized_operations := integer()
}.
