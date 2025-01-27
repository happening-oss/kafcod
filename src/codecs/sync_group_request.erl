-module(sync_group_request).
-export([
    encode_sync_group_request_0/1,
    decode_sync_group_request_0/1,
    encode_sync_group_request_1/1,
    decode_sync_group_request_1/1,
    encode_sync_group_request_2/1,
    decode_sync_group_request_2/1,
    encode_sync_group_request_3/1,
    decode_sync_group_request_3/1,
    encode_sync_group_request_4/1,
    decode_sync_group_request_4/1,
    encode_sync_group_request_5/1,
    decode_sync_group_request_5/1
]).
-export_type([
    sync_group_request_0/0,
    sync_group_request_assignment_0/0,
    sync_group_request_1/0,
    sync_group_request_assignment_1/0,
    sync_group_request_2/0,
    sync_group_request_assignment_2/0,
    sync_group_request_3/0,
    sync_group_request_assignment_3/0,
    sync_group_request_4/0,
    sync_group_request_assignment_4/0,
    sync_group_request_5/0,
    sync_group_request_assignment_5/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(SYNC_GROUP_REQUEST, 14).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_sync_group_request_0(sync_group_request_0()) -> iodata().

encode_sync_group_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID assigned by the group.
        member_id := MemberId,
        % Each assignment.
        assignments := Assignments
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_array(Assignments)
->
    [
        ?encode_request_header_1(?SYNC_GROUP_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_string(MemberId),
        ?encode_array(Assignments, fun encode_sync_group_request_assignment_0/1)
    ];
encode_sync_group_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string,
        assignments => {array, sync_group_request_assignment_0}
    }).

-spec decode_sync_group_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_0(),
    Rest :: binary().

decode_sync_group_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_array(Assignments, Bin3, Bin4, ?_decode_element(decode_sync_group_request_assignment_0)),
    {
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId,
            assignments => Assignments
        },
        Bin4
    }.

-spec encode_sync_group_request_assignment_0(sync_group_request_assignment_0()) -> iodata().

encode_sync_group_request_assignment_0(
    _Args = #{
        % The ID of the member to assign.
        member_id := MemberId,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Assignment)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Assignment)
    ];
encode_sync_group_request_assignment_0(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        assignment => bytes
    }).

-spec decode_sync_group_request_assignment_0(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_assignment_0(),
    Rest :: binary().

decode_sync_group_request_assignment_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Assignment, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            assignment => Assignment
        },
        Bin2
    }.

-spec encode_sync_group_request_1(sync_group_request_1()) -> iodata().

encode_sync_group_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID assigned by the group.
        member_id := MemberId,
        % Each assignment.
        assignments := Assignments
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_array(Assignments)
->
    [
        ?encode_request_header_1(?SYNC_GROUP_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_string(MemberId),
        ?encode_array(Assignments, fun encode_sync_group_request_assignment_1/1)
    ];
encode_sync_group_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string,
        assignments => {array, sync_group_request_assignment_1}
    }).

-spec decode_sync_group_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_1(),
    Rest :: binary().

decode_sync_group_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_array(Assignments, Bin3, Bin4, ?_decode_element(decode_sync_group_request_assignment_1)),
    {
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId,
            assignments => Assignments
        },
        Bin4
    }.

-spec encode_sync_group_request_assignment_1(sync_group_request_assignment_1()) -> iodata().

encode_sync_group_request_assignment_1(
    _Args = #{
        % The ID of the member to assign.
        member_id := MemberId,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Assignment)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Assignment)
    ];
encode_sync_group_request_assignment_1(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        assignment => bytes
    }).

-spec decode_sync_group_request_assignment_1(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_assignment_1(),
    Rest :: binary().

decode_sync_group_request_assignment_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Assignment, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            assignment => Assignment
        },
        Bin2
    }.

-spec encode_sync_group_request_2(sync_group_request_2()) -> iodata().

encode_sync_group_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID assigned by the group.
        member_id := MemberId,
        % Each assignment.
        assignments := Assignments
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_array(Assignments)
->
    [
        ?encode_request_header_1(?SYNC_GROUP_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_string(MemberId),
        ?encode_array(Assignments, fun encode_sync_group_request_assignment_2/1)
    ];
encode_sync_group_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string,
        assignments => {array, sync_group_request_assignment_2}
    }).

-spec decode_sync_group_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_2(),
    Rest :: binary().

decode_sync_group_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_array(Assignments, Bin3, Bin4, ?_decode_element(decode_sync_group_request_assignment_2)),
    {
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId,
            assignments => Assignments
        },
        Bin4
    }.

-spec encode_sync_group_request_assignment_2(sync_group_request_assignment_2()) -> iodata().

encode_sync_group_request_assignment_2(
    _Args = #{
        % The ID of the member to assign.
        member_id := MemberId,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Assignment)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Assignment)
    ];
encode_sync_group_request_assignment_2(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        assignment => bytes
    }).

-spec decode_sync_group_request_assignment_2(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_assignment_2(),
    Rest :: binary().

decode_sync_group_request_assignment_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Assignment, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            assignment => Assignment
        },
        Bin2
    }.

-spec encode_sync_group_request_3(sync_group_request_3()) -> iodata().

encode_sync_group_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID assigned by the group.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % Each assignment.
        assignments := Assignments
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_array(Assignments)
->
    [
        ?encode_request_header_1(?SYNC_GROUP_REQUEST, 3, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_string(MemberId),
        ?encode_nullable_string(GroupInstanceId),
        ?encode_array(Assignments, fun encode_sync_group_request_assignment_3/1)
    ];
encode_sync_group_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string,
        group_instance_id => nullable_string,
        assignments => {array, sync_group_request_assignment_3}
    }).

-spec decode_sync_group_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_3(),
    Rest :: binary().

decode_sync_group_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_nullable_string(GroupInstanceId, Bin3, Bin4),
    ?_decode_array(Assignments, Bin4, Bin5, ?_decode_element(decode_sync_group_request_assignment_3)),
    {
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            assignments => Assignments
        },
        Bin5
    }.

-spec encode_sync_group_request_assignment_3(sync_group_request_assignment_3()) -> iodata().

encode_sync_group_request_assignment_3(
    _Args = #{
        % The ID of the member to assign.
        member_id := MemberId,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Assignment)
->
    [
        ?encode_string(MemberId),
        ?encode_bytes(Assignment)
    ];
encode_sync_group_request_assignment_3(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        assignment => bytes
    }).

-spec decode_sync_group_request_assignment_3(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_assignment_3(),
    Rest :: binary().

decode_sync_group_request_assignment_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(MemberId, Bin0, Bin1),
    ?_decode_bytes(Assignment, Bin1, Bin2),
    {
        #{
            member_id => MemberId,
            assignment => Assignment
        },
        Bin2
    }.

-spec encode_sync_group_request_4(sync_group_request_4()) -> iodata().

encode_sync_group_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID assigned by the group.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % Each assignment.
        assignments := Assignments
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_array(Assignments)
->
    [
        ?encode_request_header_2(?SYNC_GROUP_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_array(Assignments, fun encode_sync_group_request_assignment_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_sync_group_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string,
        group_instance_id => nullable_string,
        assignments => {array, sync_group_request_assignment_4}
    }).

-spec decode_sync_group_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_4(),
    Rest :: binary().

decode_sync_group_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_compact_string(MemberId, Bin2, Bin3),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin3, Bin4),
    ?_decode_compact_array(Assignments, Bin4, Bin5, ?_decode_element(decode_sync_group_request_assignment_4)),
    ?decode_tagged_fields(
        fun decode_sync_group_request_4_tagged_field/3,
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            assignments => Assignments
        },
        Bin5
    ).

-spec decode_sync_group_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_sync_group_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_sync_group_request_assignment_4(sync_group_request_assignment_4()) -> iodata().

encode_sync_group_request_assignment_4(
    _Args = #{
        % The ID of the member to assign.
        member_id := MemberId,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Assignment)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_bytes(Assignment),
        ?EMPTY_TAG_BUFFER
    ];
encode_sync_group_request_assignment_4(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        assignment => bytes
    }).

-spec decode_sync_group_request_assignment_4(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_assignment_4(),
    Rest :: binary().

decode_sync_group_request_assignment_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_bytes(Assignment, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_sync_group_request_assignment_4_tagged_field/3,
        #{
            member_id => MemberId,
            assignment => Assignment
        },
        Bin2
    ).

-spec decode_sync_group_request_assignment_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_sync_group_request_assignment_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_sync_group_request_5(sync_group_request_5()) -> iodata().

encode_sync_group_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID assigned by the group.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The group protocol type.
        protocol_type := ProtocolType,
        % The group protocol name.
        protocol_name := ProtocolName,
        % Each assignment.
        assignments := Assignments
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_nullable_string(ProtocolType),
    ?is_nullable_string(ProtocolName),
    ?is_array(Assignments)
->
    [
        ?encode_request_header_2(?SYNC_GROUP_REQUEST, 5, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_nullable_string(ProtocolType),
        ?encode_compact_nullable_string(ProtocolName),
        ?encode_compact_array(Assignments, fun encode_sync_group_request_assignment_5/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_sync_group_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string,
        group_instance_id => nullable_string,
        protocol_type => nullable_string,
        protocol_name => nullable_string,
        assignments => {array, sync_group_request_assignment_5}
    }).

-spec decode_sync_group_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_5(),
    Rest :: binary().

decode_sync_group_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_compact_string(MemberId, Bin2, Bin3),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin3, Bin4),
    ?_decode_compact_nullable_string(ProtocolType, Bin4, Bin5),
    ?_decode_compact_nullable_string(ProtocolName, Bin5, Bin6),
    ?_decode_compact_array(Assignments, Bin6, Bin7, ?_decode_element(decode_sync_group_request_assignment_5)),
    ?decode_tagged_fields(
        fun decode_sync_group_request_5_tagged_field/3,
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            protocol_type => ProtocolType,
            protocol_name => ProtocolName,
            assignments => Assignments
        },
        Bin7
    ).

-spec decode_sync_group_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_sync_group_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_sync_group_request_assignment_5(sync_group_request_assignment_5()) -> iodata().

encode_sync_group_request_assignment_5(
    _Args = #{
        % The ID of the member to assign.
        member_id := MemberId,
        % The member assignment.
        assignment := Assignment
    }
) when
    ?is_string(MemberId),
    ?is_bytes(Assignment)
->
    [
        ?encode_compact_string(MemberId),
        ?encode_compact_bytes(Assignment),
        ?EMPTY_TAG_BUFFER
    ];
encode_sync_group_request_assignment_5(Args) ->
    ?encoder_error(Args, #{
        member_id => string,
        assignment => bytes
    }).

-spec decode_sync_group_request_assignment_5(binary()) -> {Decoded, Rest} when
    Decoded :: sync_group_request_assignment_5(),
    Rest :: binary().

decode_sync_group_request_assignment_5(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(MemberId, Bin0, Bin1),
    ?_decode_compact_bytes(Assignment, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_sync_group_request_assignment_5_tagged_field/3,
        #{
            member_id => MemberId,
            assignment => Assignment
        },
        Bin2
    ).

-spec decode_sync_group_request_assignment_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_sync_group_request_assignment_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type sync_group_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary(),
    assignments := list(sync_group_request_assignment_0())
}.
-type sync_group_request_assignment_0() :: #{
    member_id := binary(),
    assignment := kafcod:bytes()
}.
-type sync_group_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary(),
    assignments := list(sync_group_request_assignment_1())
}.
-type sync_group_request_assignment_1() :: #{
    member_id := binary(),
    assignment := kafcod:bytes()
}.
-type sync_group_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary(),
    assignments := list(sync_group_request_assignment_2())
}.
-type sync_group_request_assignment_2() :: #{
    member_id := binary(),
    assignment := kafcod:bytes()
}.
-type sync_group_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    assignments := list(sync_group_request_assignment_3())
}.
-type sync_group_request_assignment_3() :: #{
    member_id := binary(),
    assignment := kafcod:bytes()
}.
-type sync_group_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    assignments := list(sync_group_request_assignment_4())
}.
-type sync_group_request_assignment_4() :: #{
    member_id := binary(),
    assignment := kafcod:bytes()
}.
-type sync_group_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    protocol_type := binary() | null,
    protocol_name := binary() | null,
    assignments := list(sync_group_request_assignment_5())
}.
-type sync_group_request_assignment_5() :: #{
    member_id := binary(),
    assignment := kafcod:bytes()
}.
