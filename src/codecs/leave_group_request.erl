-module(leave_group_request).
-export([
    encode_leave_group_request_0/1,
    decode_leave_group_request_0/1,
    encode_leave_group_request_1/1,
    decode_leave_group_request_1/1,
    encode_leave_group_request_2/1,
    decode_leave_group_request_2/1,
    encode_leave_group_request_3/1,
    decode_leave_group_request_3/1,
    encode_leave_group_request_4/1,
    decode_leave_group_request_4/1,
    encode_leave_group_request_5/1,
    decode_leave_group_request_5/1
]).
-export_type([
    leave_group_request_0/0,
    leave_group_request_1/0,
    leave_group_request_2/0,
    leave_group_request_3/0,
    member_identity_3/0,
    leave_group_request_4/0,
    member_identity_4/0,
    leave_group_request_5/0,
    member_identity_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(LEAVE_GROUP_REQUEST, 13).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_leave_group_request_0(leave_group_request_0()) -> iodata().

encode_leave_group_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the group to leave.
        group_id := GroupId,
        % The member ID to remove from the group.
        member_id := MemberId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_string(MemberId)
->
    [
        ?encode_request_header_1(?LEAVE_GROUP_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_string(MemberId)
    ];
encode_leave_group_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        member_id => string
    }).

-spec decode_leave_group_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_request_0(),
    Rest :: binary().

decode_leave_group_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_string(MemberId, Bin1, Bin2),
    {
        Header#{
            group_id => GroupId,
            member_id => MemberId
        },
        Bin2
    }.

-spec encode_leave_group_request_1(leave_group_request_1()) -> iodata().

encode_leave_group_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the group to leave.
        group_id := GroupId,
        % The member ID to remove from the group.
        member_id := MemberId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_string(MemberId)
->
    [
        ?encode_request_header_1(?LEAVE_GROUP_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_string(MemberId)
    ];
encode_leave_group_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        member_id => string
    }).

-spec decode_leave_group_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_request_1(),
    Rest :: binary().

decode_leave_group_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_string(MemberId, Bin1, Bin2),
    {
        Header#{
            group_id => GroupId,
            member_id => MemberId
        },
        Bin2
    }.

-spec encode_leave_group_request_2(leave_group_request_2()) -> iodata().

encode_leave_group_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the group to leave.
        group_id := GroupId,
        % The member ID to remove from the group.
        member_id := MemberId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_string(MemberId)
->
    [
        ?encode_request_header_1(?LEAVE_GROUP_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_string(MemberId)
    ];
encode_leave_group_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        member_id => string
    }).

-spec decode_leave_group_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_request_2(),
    Rest :: binary().

decode_leave_group_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_string(MemberId, Bin1, Bin2),
    {
        Header#{
            group_id => GroupId,
            member_id => MemberId
        },
        Bin2
    }.

-spec encode_leave_group_request_3(leave_group_request_3()) -> iodata().

encode_leave_group_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the group to leave.
        group_id := GroupId,
        % List of leaving member identities.
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_array(Members)
->
    [
        ?encode_request_header_1(?LEAVE_GROUP_REQUEST, 3, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_array(Members, fun encode_member_identity_3/1)
    ];
encode_leave_group_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        members => {array, member_identity_3}
    }).

-spec decode_leave_group_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_request_3(),
    Rest :: binary().

decode_leave_group_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_array(Members, Bin1, Bin2, ?_decode_element(decode_member_identity_3)),
    {
        Header#{
            group_id => GroupId,
            members => Members
        },
        Bin2
    }.

-spec encode_member_identity_3(member_identity_3()) -> iodata().

encode_member_identity_3(
    _Args = #{
        % The member ID to remove from the group.
        member_id := MemberId,
        % The group instance ID to remove from the group.
        group_instance_id := GroupInstanceId
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId)
->
    [
        ?encode_string(MemberId),
        ?encode_nullable_string(GroupInstanceId)
    ];
encode_member_identity_3(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string
    }).

-spec decode_member_identity_3(binary()) -> {Decoded, Rest} when
    Decoded :: member_identity_3(),
    Rest :: binary().

decode_member_identity_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_nullable_string(GroupInstanceId, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId
        },
        Bin2
    }.

-spec encode_leave_group_request_4(leave_group_request_4()) -> iodata().

encode_leave_group_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the group to leave.
        group_id := GroupId,
        % List of leaving member identities.
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_array(Members)
->
    [
        ?encode_request_header_2(?LEAVE_GROUP_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_compact_array(Members, fun encode_member_identity_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leave_group_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        members => {array, member_identity_4}
    }).

-spec decode_leave_group_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_request_4(),
    Rest :: binary().

decode_leave_group_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_array(Members, Bin1, Bin2, ?_decode_element(decode_member_identity_4)),
    ?decode_tagged_fields(
        fun decode_leave_group_request_4_tagged_field/3,
        Header#{
            group_id => GroupId,
            members => Members
        },
        Bin2
    ).

-spec decode_leave_group_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leave_group_request_4().

decode_leave_group_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_member_identity_4(member_identity_4()) -> iodata().

encode_member_identity_4(
    _Args = #{
        % The member ID to remove from the group.
        member_id := MemberId,
        % The group instance ID to remove from the group.
        group_instance_id := GroupInstanceId
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?EMPTY_TAG_BUFFER
    ];
encode_member_identity_4(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string
    }).

-spec decode_member_identity_4(binary()) -> {Decoded, Rest} when
    Decoded :: member_identity_4(),
    Rest :: binary().

decode_member_identity_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_member_identity_4_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId
        },
        Bin2
    ).

-spec decode_member_identity_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: member_identity_4().

decode_member_identity_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leave_group_request_5(leave_group_request_5()) -> iodata().

encode_leave_group_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the group to leave.
        group_id := GroupId,
        % List of leaving member identities.
        members := Members
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_array(Members)
->
    [
        ?encode_request_header_2(?LEAVE_GROUP_REQUEST, 5, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_compact_array(Members, fun encode_member_identity_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leave_group_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        members => {array, member_identity_5}
    }).

-spec decode_leave_group_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: leave_group_request_5(),
    Rest :: binary().

decode_leave_group_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_array(Members, Bin1, Bin2, ?_decode_element(decode_member_identity_5)),
    ?decode_tagged_fields(
        fun decode_leave_group_request_5_tagged_field/3,
        Header#{
            group_id => GroupId,
            members => Members
        },
        Bin2
    ).

-spec decode_leave_group_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leave_group_request_5().

decode_leave_group_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_member_identity_5(member_identity_5()) -> iodata().

encode_member_identity_5(
    _Args = #{
        % The member ID to remove from the group.
        member_id := MemberId,
        % The group instance ID to remove from the group.
        group_instance_id := GroupInstanceId,
        % The reason why the member left the group.
        reason := Reason
    }
) when
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_nullable_string(Reason)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_nullable_string(Reason),
        ?EMPTY_TAG_BUFFER
    ];
encode_member_identity_5(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        group_instance_id => nullable_string,
        reason => nullable_string
    }).

-spec decode_member_identity_5(binary()) -> {Decoded, Rest} when
    Decoded :: member_identity_5(),
    Rest :: binary().

decode_member_identity_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin1, Bin2),
    ?_decode_compact_nullable_string(Reason, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_member_identity_5_tagged_field/3,
        #{
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            reason => Reason
        },
        Bin3
    ).

-spec decode_member_identity_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: member_identity_5().

decode_member_identity_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type leave_group_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    member_id := binary()
}.
-type leave_group_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    member_id := binary()
}.
-type leave_group_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    member_id := binary()
}.
-type leave_group_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    members := list(member_identity_3())
}.
-type member_identity_3() :: #{
    member_id := binary(),
    group_instance_id := binary() | null
}.
-type leave_group_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    members := list(member_identity_4())
}.
-type member_identity_4() :: #{
    member_id := binary(),
    group_instance_id := binary() | null
}.
-type leave_group_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    members := list(member_identity_5())
}.
-type member_identity_5() :: #{
    member_id := binary(),
    group_instance_id := binary() | null,
    reason := binary() | null
}.
