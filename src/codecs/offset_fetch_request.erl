-module(offset_fetch_request).
-export([
    encode_offset_fetch_request_0/1,
    decode_offset_fetch_request_0/1,
    encode_offset_fetch_request_1/1,
    decode_offset_fetch_request_1/1,
    encode_offset_fetch_request_2/1,
    decode_offset_fetch_request_2/1,
    encode_offset_fetch_request_3/1,
    decode_offset_fetch_request_3/1,
    encode_offset_fetch_request_4/1,
    decode_offset_fetch_request_4/1,
    encode_offset_fetch_request_5/1,
    decode_offset_fetch_request_5/1,
    encode_offset_fetch_request_6/1,
    decode_offset_fetch_request_6/1,
    encode_offset_fetch_request_7/1,
    decode_offset_fetch_request_7/1,
    encode_offset_fetch_request_8/1,
    decode_offset_fetch_request_8/1,
    encode_offset_fetch_request_9/1,
    decode_offset_fetch_request_9/1
]).
-export_type([
    offset_fetch_request_0/0,
    offset_fetch_request_topic_0/0,
    offset_fetch_request_1/0,
    offset_fetch_request_topic_1/0,
    offset_fetch_request_2/0,
    offset_fetch_request_topic_2/0,
    offset_fetch_request_3/0,
    offset_fetch_request_topic_3/0,
    offset_fetch_request_4/0,
    offset_fetch_request_topic_4/0,
    offset_fetch_request_5/0,
    offset_fetch_request_topic_5/0,
    offset_fetch_request_6/0,
    offset_fetch_request_topic_6/0,
    offset_fetch_request_7/0,
    offset_fetch_request_topic_7/0,
    offset_fetch_request_8/0,
    offset_fetch_request_topics_8/0,
    offset_fetch_request_group_8/0,
    offset_fetch_request_9/0,
    offset_fetch_request_topics_9/0,
    offset_fetch_request_group_9/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(OFFSET_FETCH_REQUEST, 9).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_offset_fetch_request_0(offset_fetch_request_0()) -> iodata().

encode_offset_fetch_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group to fetch offsets for.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FETCH_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_array(Topics, fun encode_offset_fetch_request_topic_0/1)
    ];
encode_offset_fetch_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {array, offset_fetch_request_topic_0}
    }).

-spec decode_offset_fetch_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_0(),
    Rest :: binary().

decode_offset_fetch_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topic_0)),
    {
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_fetch_request_topic_0(offset_fetch_request_topic_0()) -> iodata().

encode_offset_fetch_request_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionIndexes, ?encode_int32_)
    ];
encode_offset_fetch_request_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topic_0(),
    Rest :: binary().

decode_offset_fetch_request_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    }.

-spec encode_offset_fetch_request_1(offset_fetch_request_1()) -> iodata().

encode_offset_fetch_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group to fetch offsets for.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FETCH_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_array(Topics, fun encode_offset_fetch_request_topic_1/1)
    ];
encode_offset_fetch_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {array, offset_fetch_request_topic_1}
    }).

-spec decode_offset_fetch_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_1(),
    Rest :: binary().

decode_offset_fetch_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topic_1)),
    {
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_fetch_request_topic_1(offset_fetch_request_topic_1()) -> iodata().

encode_offset_fetch_request_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionIndexes, ?encode_int32_)
    ];
encode_offset_fetch_request_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topic_1(),
    Rest :: binary().

decode_offset_fetch_request_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    }.

-spec encode_offset_fetch_request_2(offset_fetch_request_2()) -> iodata().

encode_offset_fetch_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group to fetch offsets for.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FETCH_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_nullable_array(Topics, fun encode_offset_fetch_request_topic_2/1)
    ];
encode_offset_fetch_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {nullable_array, offset_fetch_request_topic_2}
    }).

-spec decode_offset_fetch_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_2(),
    Rest :: binary().

decode_offset_fetch_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_nullable_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topic_2)),
    {
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_fetch_request_topic_2(offset_fetch_request_topic_2()) -> iodata().

encode_offset_fetch_request_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionIndexes, ?encode_int32_)
    ];
encode_offset_fetch_request_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topic_2(),
    Rest :: binary().

decode_offset_fetch_request_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    }.

-spec encode_offset_fetch_request_3(offset_fetch_request_3()) -> iodata().

encode_offset_fetch_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group to fetch offsets for.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FETCH_REQUEST, 3, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_nullable_array(Topics, fun encode_offset_fetch_request_topic_3/1)
    ];
encode_offset_fetch_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {nullable_array, offset_fetch_request_topic_3}
    }).

-spec decode_offset_fetch_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_3(),
    Rest :: binary().

decode_offset_fetch_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_nullable_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topic_3)),
    {
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_fetch_request_topic_3(offset_fetch_request_topic_3()) -> iodata().

encode_offset_fetch_request_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionIndexes, ?encode_int32_)
    ];
encode_offset_fetch_request_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topic_3(),
    Rest :: binary().

decode_offset_fetch_request_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    }.

-spec encode_offset_fetch_request_4(offset_fetch_request_4()) -> iodata().

encode_offset_fetch_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group to fetch offsets for.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FETCH_REQUEST, 4, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_nullable_array(Topics, fun encode_offset_fetch_request_topic_4/1)
    ];
encode_offset_fetch_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {nullable_array, offset_fetch_request_topic_4}
    }).

-spec decode_offset_fetch_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_4(),
    Rest :: binary().

decode_offset_fetch_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_nullable_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topic_4)),
    {
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_fetch_request_topic_4(offset_fetch_request_topic_4()) -> iodata().

encode_offset_fetch_request_topic_4(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionIndexes, ?encode_int32_)
    ];
encode_offset_fetch_request_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topic_4(),
    Rest :: binary().

decode_offset_fetch_request_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    }.

-spec encode_offset_fetch_request_5(offset_fetch_request_5()) -> iodata().

encode_offset_fetch_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group to fetch offsets for.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FETCH_REQUEST, 5, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_nullable_array(Topics, fun encode_offset_fetch_request_topic_5/1)
    ];
encode_offset_fetch_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {nullable_array, offset_fetch_request_topic_5}
    }).

-spec decode_offset_fetch_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_5(),
    Rest :: binary().

decode_offset_fetch_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_nullable_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topic_5)),
    {
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_fetch_request_topic_5(offset_fetch_request_topic_5()) -> iodata().

encode_offset_fetch_request_topic_5(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_string(Name),
        ?encode_array(PartitionIndexes, ?encode_int32_)
    ];
encode_offset_fetch_request_topic_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topic_5(),
    Rest :: binary().

decode_offset_fetch_request_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    }.

-spec encode_offset_fetch_request_6(offset_fetch_request_6()) -> iodata().

encode_offset_fetch_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group to fetch offsets for.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_2(?OFFSET_FETCH_REQUEST, 6, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_compact_nullable_array(Topics, fun encode_offset_fetch_request_topic_6/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {nullable_array, offset_fetch_request_topic_6}
    }).

-spec decode_offset_fetch_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_6(),
    Rest :: binary().

decode_offset_fetch_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_nullable_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topic_6)),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_6_tagged_field/3,
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    ).

-spec decode_offset_fetch_request_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_6().

decode_offset_fetch_request_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_topic_6(offset_fetch_request_topic_6()) -> iodata().

encode_offset_fetch_request_topic_6(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionIndexes, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_topic_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topic_6(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topic_6(),
    Rest :: binary().

decode_offset_fetch_request_topic_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_topic_6_tagged_field/3,
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    ).

-spec decode_offset_fetch_request_topic_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_topic_6().

decode_offset_fetch_request_topic_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_7(offset_fetch_request_7()) -> iodata().

encode_offset_fetch_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The group to fetch offsets for.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics,
        % Whether broker should hold on returning unstable offsets but set a retriable error code for the partitions.
        require_stable := RequireStable
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_nullable_array(Topics),
    ?is_bool(RequireStable)
->
    [
        ?encode_request_header_2(?OFFSET_FETCH_REQUEST, 7, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_compact_nullable_array(Topics, fun encode_offset_fetch_request_topic_7/1),
        ?encode_bool(RequireStable),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {nullable_array, offset_fetch_request_topic_7},
        require_stable => bool
    }).

-spec decode_offset_fetch_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_7(),
    Rest :: binary().

decode_offset_fetch_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_nullable_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topic_7)),
    ?_decode_bool(RequireStable, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_7_tagged_field/3,
        Header#{
            group_id => GroupId,
            topics => Topics,
            require_stable => RequireStable
        },
        Bin3
    ).

-spec decode_offset_fetch_request_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_7().

decode_offset_fetch_request_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_topic_7(offset_fetch_request_topic_7()) -> iodata().

encode_offset_fetch_request_topic_7(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionIndexes, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_topic_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topic_7(),
    Rest :: binary().

decode_offset_fetch_request_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_topic_7_tagged_field/3,
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    ).

-spec decode_offset_fetch_request_topic_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_topic_7().

decode_offset_fetch_request_topic_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_8(offset_fetch_request_8()) -> iodata().

encode_offset_fetch_request_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each group we would like to fetch offsets for
        groups := Groups,
        % Whether broker should hold on returning unstable offsets but set a retriable error code for the partitions.
        require_stable := RequireStable
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Groups),
    ?is_bool(RequireStable)
->
    [
        ?encode_request_header_2(?OFFSET_FETCH_REQUEST, 8, CorrelationId, ClientId),
        ?encode_compact_array(Groups, fun encode_offset_fetch_request_group_8/1),
        ?encode_bool(RequireStable),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups => {array, offset_fetch_request_group_8},
        require_stable => bool
    }).

-spec decode_offset_fetch_request_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_8(),
    Rest :: binary().

decode_offset_fetch_request_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Groups, Bin0, Bin1, ?_decode_element(decode_offset_fetch_request_group_8)),
    ?_decode_bool(RequireStable, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_8_tagged_field/3,
        Header#{
            groups => Groups,
            require_stable => RequireStable
        },
        Bin2
    ).

-spec decode_offset_fetch_request_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_8().

decode_offset_fetch_request_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_topics_8(offset_fetch_request_topics_8()) -> iodata().

encode_offset_fetch_request_topics_8(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionIndexes, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_topics_8(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topics_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topics_8(),
    Rest :: binary().

decode_offset_fetch_request_topics_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_topics_8_tagged_field/3,
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    ).

-spec decode_offset_fetch_request_topics_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_topics_8().

decode_offset_fetch_request_topics_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_group_8(offset_fetch_request_group_8()) -> iodata().

encode_offset_fetch_request_group_8(
    _Args = #{
        % The group ID.
        group_id := GroupId,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_string(GroupId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_compact_string(GroupId),
        ?encode_compact_nullable_array(Topics, fun encode_offset_fetch_request_topics_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_group_8(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        topics => {nullable_array, offset_fetch_request_topics_8}
    }).

-spec decode_offset_fetch_request_group_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_group_8(),
    Rest :: binary().

decode_offset_fetch_request_group_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_nullable_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_fetch_request_topics_8)),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_group_8_tagged_field/3,
        #{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    ).

-spec decode_offset_fetch_request_group_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_group_8().

decode_offset_fetch_request_group_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_9(offset_fetch_request_9()) -> iodata().

encode_offset_fetch_request_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each group we would like to fetch offsets for
        groups := Groups,
        % Whether broker should hold on returning unstable offsets but set a retriable error code for the partitions.
        require_stable := RequireStable
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Groups),
    ?is_bool(RequireStable)
->
    [
        ?encode_request_header_2(?OFFSET_FETCH_REQUEST, 9, CorrelationId, ClientId),
        ?encode_compact_array(Groups, fun encode_offset_fetch_request_group_9/1),
        ?encode_bool(RequireStable),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        groups => {array, offset_fetch_request_group_9},
        require_stable => bool
    }).

-spec decode_offset_fetch_request_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_9(),
    Rest :: binary().

decode_offset_fetch_request_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Groups, Bin0, Bin1, ?_decode_element(decode_offset_fetch_request_group_9)),
    ?_decode_bool(RequireStable, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_9_tagged_field/3,
        Header#{
            groups => Groups,
            require_stable => RequireStable
        },
        Bin2
    ).

-spec decode_offset_fetch_request_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_9().

decode_offset_fetch_request_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_topics_9(offset_fetch_request_topics_9()) -> iodata().

encode_offset_fetch_request_topics_9(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes we would like to fetch offsets for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionIndexes, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_topics_9(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_offset_fetch_request_topics_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_topics_9(),
    Rest :: binary().

decode_offset_fetch_request_topics_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_topics_9_tagged_field/3,
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    ).

-spec decode_offset_fetch_request_topics_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_topics_9().

decode_offset_fetch_request_topics_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_fetch_request_group_9(offset_fetch_request_group_9()) -> iodata().

encode_offset_fetch_request_group_9(
    _Args = #{
        % The group ID.
        group_id := GroupId,
        % The member ID assigned by the group coordinator if using the new consumer protocol (KIP-848).
        member_id := MemberId,
        % The member epoch if using the new consumer protocol (KIP-848).
        member_epoch := MemberEpoch,
        % Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
        topics := Topics
    }
) when
    ?is_string(GroupId),
    ?is_nullable_string(MemberId),
    ?is_int32(MemberEpoch),
    ?is_nullable_array(Topics)
->
    [
        ?encode_compact_string(GroupId),
        ?encode_compact_nullable_string(MemberId),
        ?encode_int32(MemberEpoch),
        ?encode_compact_nullable_array(Topics, fun encode_offset_fetch_request_topics_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_fetch_request_group_9(Args) ->
    ?encoder_error(Args, #{
        group_id => string,
        member_id => nullable_string,
        member_epoch => int32,
        topics => {nullable_array, offset_fetch_request_topics_9}
    }).

-spec decode_offset_fetch_request_group_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_fetch_request_group_9(),
    Rest :: binary().

decode_offset_fetch_request_group_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_compact_nullable_string(MemberId, Bin1, Bin2),
    ?_decode_int32(MemberEpoch, Bin2, Bin3),
    ?_decode_compact_nullable_array(Topics, Bin3, Bin4, ?_decode_element(decode_offset_fetch_request_topics_9)),
    ?decode_tagged_fields(
        fun decode_offset_fetch_request_group_9_tagged_field/3,
        #{
            group_id => GroupId,
            member_id => MemberId,
            member_epoch => MemberEpoch,
            topics => Topics
        },
        Bin4
    ).

-spec decode_offset_fetch_request_group_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_fetch_request_group_9().

decode_offset_fetch_request_group_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type offset_fetch_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_fetch_request_topic_0())
}.
-type offset_fetch_request_topic_0() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_fetch_request_topic_1())
}.
-type offset_fetch_request_topic_1() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_fetch_request_topic_2()) | null
}.
-type offset_fetch_request_topic_2() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_fetch_request_topic_3()) | null
}.
-type offset_fetch_request_topic_3() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_fetch_request_topic_4()) | null
}.
-type offset_fetch_request_topic_4() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_fetch_request_topic_5()) | null
}.
-type offset_fetch_request_topic_5() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_fetch_request_topic_6()) | null
}.
-type offset_fetch_request_topic_6() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_fetch_request_topic_7()) | null,
    require_stable := boolean()
}.
-type offset_fetch_request_topic_7() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_8() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups := list(offset_fetch_request_group_8()),
    require_stable := boolean()
}.
-type offset_fetch_request_topics_8() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_group_8() :: #{
    group_id := binary(),
    topics := list(offset_fetch_request_topics_8()) | null
}.
-type offset_fetch_request_9() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    groups := list(offset_fetch_request_group_9()),
    require_stable := boolean()
}.
-type offset_fetch_request_topics_9() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
-type offset_fetch_request_group_9() :: #{
    group_id := binary(),
    member_id := binary() | null,
    member_epoch := integer(),
    topics := list(offset_fetch_request_topics_9()) | null
}.
