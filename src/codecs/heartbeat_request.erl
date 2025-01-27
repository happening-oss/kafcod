-module(heartbeat_request).
-export([
    encode_heartbeat_request_0/1,
    decode_heartbeat_request_0/1,
    encode_heartbeat_request_1/1,
    decode_heartbeat_request_1/1,
    encode_heartbeat_request_2/1,
    decode_heartbeat_request_2/1,
    encode_heartbeat_request_3/1,
    decode_heartbeat_request_3/1,
    encode_heartbeat_request_4/1,
    decode_heartbeat_request_4/1
]).
-export_type([
    heartbeat_request_0/0,
    heartbeat_request_1/0,
    heartbeat_request_2/0,
    heartbeat_request_3/0,
    heartbeat_request_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(HEARTBEAT_REQUEST, 12).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_heartbeat_request_0(heartbeat_request_0()) -> iodata().

encode_heartbeat_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group id.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID.
        member_id := MemberId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId)
->
    [
        ?encode_request_header_1(?HEARTBEAT_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_string(MemberId)
    ];
encode_heartbeat_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string
    }).

-spec decode_heartbeat_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_request_0(),
    Rest :: binary().

decode_heartbeat_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    {
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId
        },
        Bin3
    }.

-spec encode_heartbeat_request_1(heartbeat_request_1()) -> iodata().

encode_heartbeat_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group id.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID.
        member_id := MemberId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId)
->
    [
        ?encode_request_header_1(?HEARTBEAT_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_string(MemberId)
    ];
encode_heartbeat_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string
    }).

-spec decode_heartbeat_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_request_1(),
    Rest :: binary().

decode_heartbeat_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    {
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId
        },
        Bin3
    }.

-spec encode_heartbeat_request_2(heartbeat_request_2()) -> iodata().

encode_heartbeat_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group id.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID.
        member_id := MemberId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId)
->
    [
        ?encode_request_header_1(?HEARTBEAT_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_string(MemberId)
    ];
encode_heartbeat_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string
    }).

-spec decode_heartbeat_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_request_2(),
    Rest :: binary().

decode_heartbeat_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    {
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId
        },
        Bin3
    }.

-spec encode_heartbeat_request_3(heartbeat_request_3()) -> iodata().

encode_heartbeat_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group id.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId)
->
    [
        ?encode_request_header_1(?HEARTBEAT_REQUEST, 3, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_string(MemberId),
        ?encode_nullable_string(GroupInstanceId)
    ];
encode_heartbeat_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string,
        group_instance_id => nullable_string
    }).

-spec decode_heartbeat_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_request_3(),
    Rest :: binary().

decode_heartbeat_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_nullable_string(GroupInstanceId, Bin3, Bin4),
    {
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId,
            group_instance_id => GroupInstanceId
        },
        Bin4
    }.

-spec encode_heartbeat_request_4(heartbeat_request_4()) -> iodata().

encode_heartbeat_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group id.
        group_id := GroupId,
        % The generation of the group.
        generation_id := GenerationId,
        % The member ID.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId)
->
    [
        ?encode_request_header_2(?HEARTBEAT_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(GenerationId),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?EMPTY_TAG_BUFFER
    ];
encode_heartbeat_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id => int32,
        member_id => string,
        group_instance_id => nullable_string
    }).

-spec decode_heartbeat_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: heartbeat_request_4(),
    Rest :: binary().

decode_heartbeat_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationId, Bin1, Bin2),
    ?_decode_compact_string(MemberId, Bin2, Bin3),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_heartbeat_request_4_tagged_field/3,
        Header#{
            group_id => GroupId,
            generation_id => GenerationId,
            member_id => MemberId,
            group_instance_id => GroupInstanceId
        },
        Bin4
    ).

-spec decode_heartbeat_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_heartbeat_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type heartbeat_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary()
}.
-type heartbeat_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary()
}.
-type heartbeat_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary()
}.
-type heartbeat_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary(),
    group_instance_id := binary() | null
}.
-type heartbeat_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id := integer(),
    member_id := binary(),
    group_instance_id := binary() | null
}.
