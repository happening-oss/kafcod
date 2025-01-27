-module(offset_commit_request).
-export([
    encode_offset_commit_request_0/1,
    decode_offset_commit_request_0/1,
    encode_offset_commit_request_1/1,
    decode_offset_commit_request_1/1,
    encode_offset_commit_request_2/1,
    decode_offset_commit_request_2/1,
    encode_offset_commit_request_3/1,
    decode_offset_commit_request_3/1,
    encode_offset_commit_request_4/1,
    decode_offset_commit_request_4/1,
    encode_offset_commit_request_5/1,
    decode_offset_commit_request_5/1,
    encode_offset_commit_request_6/1,
    decode_offset_commit_request_6/1,
    encode_offset_commit_request_7/1,
    decode_offset_commit_request_7/1,
    encode_offset_commit_request_8/1,
    decode_offset_commit_request_8/1,
    encode_offset_commit_request_9/1,
    decode_offset_commit_request_9/1
]).
-export_type([
    offset_commit_request_0/0,
    offset_commit_request_partition_0/0,
    offset_commit_request_topic_0/0,
    offset_commit_request_1/0,
    offset_commit_request_partition_1/0,
    offset_commit_request_topic_1/0,
    offset_commit_request_2/0,
    offset_commit_request_partition_2/0,
    offset_commit_request_topic_2/0,
    offset_commit_request_3/0,
    offset_commit_request_partition_3/0,
    offset_commit_request_topic_3/0,
    offset_commit_request_4/0,
    offset_commit_request_partition_4/0,
    offset_commit_request_topic_4/0,
    offset_commit_request_5/0,
    offset_commit_request_partition_5/0,
    offset_commit_request_topic_5/0,
    offset_commit_request_6/0,
    offset_commit_request_partition_6/0,
    offset_commit_request_topic_6/0,
    offset_commit_request_7/0,
    offset_commit_request_partition_7/0,
    offset_commit_request_topic_7/0,
    offset_commit_request_8/0,
    offset_commit_request_partition_8/0,
    offset_commit_request_topic_8/0,
    offset_commit_request_9/0,
    offset_commit_request_partition_9/0,
    offset_commit_request_topic_9/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(OFFSET_COMMIT_REQUEST, 8).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_offset_commit_request_0(offset_commit_request_0()) -> iodata().

encode_offset_commit_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_COMMIT_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_array(Topics, fun encode_offset_commit_request_topic_0/1)
    ];
encode_offset_commit_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {array, offset_commit_request_topic_0}
    }).

-spec decode_offset_commit_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_0(),
    Rest :: binary().

decode_offset_commit_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_topic_0)),
    {
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_commit_request_partition_0(offset_commit_request_partition_0()) -> iodata().

encode_offset_commit_request_partition_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_nullable_string(CommittedMetadata)
    ];
encode_offset_commit_request_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_0(),
    Rest :: binary().

decode_offset_commit_request_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_nullable_string(CommittedMetadata, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_metadata => CommittedMetadata
        },
        Bin3
    }.

-spec encode_offset_commit_request_topic_0(offset_commit_request_topic_0()) -> iodata().

encode_offset_commit_request_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_request_partition_0/1)
    ];
encode_offset_commit_request_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_0}
    }).

-spec decode_offset_commit_request_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_0(),
    Rest :: binary().

decode_offset_commit_request_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_request_1(offset_commit_request_1()) -> iodata().

encode_offset_commit_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_COMMIT_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_string(MemberId),
        ?encode_array(Topics, fun encode_offset_commit_request_topic_1/1)
    ];
encode_offset_commit_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        topics => {array, offset_commit_request_topic_1}
    }).

-spec decode_offset_commit_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_1(),
    Rest :: binary().

decode_offset_commit_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_array(Topics, Bin3, Bin4, ?_decode_element(decode_offset_commit_request_topic_1)),
    {
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            topics => Topics
        },
        Bin4
    }.

-spec encode_offset_commit_request_partition_1(offset_commit_request_partition_1()) -> iodata().

encode_offset_commit_request_partition_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % The timestamp of the commit.
        commit_timestamp := CommitTimestamp,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_int64(CommitTimestamp),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_int64(CommitTimestamp),
        ?encode_nullable_string(CommittedMetadata)
    ];
encode_offset_commit_request_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        commit_timestamp => int64,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_1(),
    Rest :: binary().

decode_offset_commit_request_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_int64(CommitTimestamp, Bin2, Bin3),
    ?_decode_nullable_string(CommittedMetadata, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            commit_timestamp => CommitTimestamp,
            committed_metadata => CommittedMetadata
        },
        Bin4
    }.

-spec encode_offset_commit_request_topic_1(offset_commit_request_topic_1()) -> iodata().

encode_offset_commit_request_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_request_partition_1/1)
    ];
encode_offset_commit_request_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_1}
    }).

-spec decode_offset_commit_request_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_1(),
    Rest :: binary().

decode_offset_commit_request_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_request_2(offset_commit_request_2()) -> iodata().

encode_offset_commit_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The time period in ms to retain the offset.
        retention_time_ms := RetentionTimeMs,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_int64(RetentionTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_COMMIT_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_string(MemberId),
        ?encode_int64(RetentionTimeMs),
        ?encode_array(Topics, fun encode_offset_commit_request_topic_2/1)
    ];
encode_offset_commit_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        retention_time_ms => int64,
        topics => {array, offset_commit_request_topic_2}
    }).

-spec decode_offset_commit_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_2(),
    Rest :: binary().

decode_offset_commit_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_int64(RetentionTimeMs, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_offset_commit_request_topic_2)),
    {
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            retention_time_ms => RetentionTimeMs,
            topics => Topics
        },
        Bin5
    }.

-spec encode_offset_commit_request_partition_2(offset_commit_request_partition_2()) -> iodata().

encode_offset_commit_request_partition_2(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_nullable_string(CommittedMetadata)
    ];
encode_offset_commit_request_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_2(),
    Rest :: binary().

decode_offset_commit_request_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_nullable_string(CommittedMetadata, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_metadata => CommittedMetadata
        },
        Bin3
    }.

-spec encode_offset_commit_request_topic_2(offset_commit_request_topic_2()) -> iodata().

encode_offset_commit_request_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_request_partition_2/1)
    ];
encode_offset_commit_request_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_2}
    }).

-spec decode_offset_commit_request_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_2(),
    Rest :: binary().

decode_offset_commit_request_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_2)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_request_3(offset_commit_request_3()) -> iodata().

encode_offset_commit_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The time period in ms to retain the offset.
        retention_time_ms := RetentionTimeMs,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_int64(RetentionTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_COMMIT_REQUEST, 3, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_string(MemberId),
        ?encode_int64(RetentionTimeMs),
        ?encode_array(Topics, fun encode_offset_commit_request_topic_3/1)
    ];
encode_offset_commit_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        retention_time_ms => int64,
        topics => {array, offset_commit_request_topic_3}
    }).

-spec decode_offset_commit_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_3(),
    Rest :: binary().

decode_offset_commit_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_int64(RetentionTimeMs, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_offset_commit_request_topic_3)),
    {
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            retention_time_ms => RetentionTimeMs,
            topics => Topics
        },
        Bin5
    }.

-spec encode_offset_commit_request_partition_3(offset_commit_request_partition_3()) -> iodata().

encode_offset_commit_request_partition_3(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_nullable_string(CommittedMetadata)
    ];
encode_offset_commit_request_partition_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_3(),
    Rest :: binary().

decode_offset_commit_request_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_nullable_string(CommittedMetadata, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_metadata => CommittedMetadata
        },
        Bin3
    }.

-spec encode_offset_commit_request_topic_3(offset_commit_request_topic_3()) -> iodata().

encode_offset_commit_request_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_request_partition_3/1)
    ];
encode_offset_commit_request_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_3}
    }).

-spec decode_offset_commit_request_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_3(),
    Rest :: binary().

decode_offset_commit_request_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_3)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_request_4(offset_commit_request_4()) -> iodata().

encode_offset_commit_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The time period in ms to retain the offset.
        retention_time_ms := RetentionTimeMs,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_int64(RetentionTimeMs),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_COMMIT_REQUEST, 4, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_string(MemberId),
        ?encode_int64(RetentionTimeMs),
        ?encode_array(Topics, fun encode_offset_commit_request_topic_4/1)
    ];
encode_offset_commit_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        retention_time_ms => int64,
        topics => {array, offset_commit_request_topic_4}
    }).

-spec decode_offset_commit_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_4(),
    Rest :: binary().

decode_offset_commit_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_int64(RetentionTimeMs, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_offset_commit_request_topic_4)),
    {
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            retention_time_ms => RetentionTimeMs,
            topics => Topics
        },
        Bin5
    }.

-spec encode_offset_commit_request_partition_4(offset_commit_request_partition_4()) -> iodata().

encode_offset_commit_request_partition_4(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_nullable_string(CommittedMetadata)
    ];
encode_offset_commit_request_partition_4(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_4(),
    Rest :: binary().

decode_offset_commit_request_partition_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_nullable_string(CommittedMetadata, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_metadata => CommittedMetadata
        },
        Bin3
    }.

-spec encode_offset_commit_request_topic_4(offset_commit_request_topic_4()) -> iodata().

encode_offset_commit_request_topic_4(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_request_partition_4/1)
    ];
encode_offset_commit_request_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_4}
    }).

-spec decode_offset_commit_request_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_4(),
    Rest :: binary().

decode_offset_commit_request_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_4)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_request_5(offset_commit_request_5()) -> iodata().

encode_offset_commit_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_COMMIT_REQUEST, 5, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_string(MemberId),
        ?encode_array(Topics, fun encode_offset_commit_request_topic_5/1)
    ];
encode_offset_commit_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        topics => {array, offset_commit_request_topic_5}
    }).

-spec decode_offset_commit_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_5(),
    Rest :: binary().

decode_offset_commit_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_array(Topics, Bin3, Bin4, ?_decode_element(decode_offset_commit_request_topic_5)),
    {
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            topics => Topics
        },
        Bin4
    }.

-spec encode_offset_commit_request_partition_5(offset_commit_request_partition_5()) -> iodata().

encode_offset_commit_request_partition_5(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_nullable_string(CommittedMetadata)
    ];
encode_offset_commit_request_partition_5(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_5(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_5(),
    Rest :: binary().

decode_offset_commit_request_partition_5(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_nullable_string(CommittedMetadata, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_metadata => CommittedMetadata
        },
        Bin3
    }.

-spec encode_offset_commit_request_topic_5(offset_commit_request_topic_5()) -> iodata().

encode_offset_commit_request_topic_5(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_request_partition_5/1)
    ];
encode_offset_commit_request_topic_5(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_5}
    }).

-spec decode_offset_commit_request_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_5(),
    Rest :: binary().

decode_offset_commit_request_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_5)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_request_6(offset_commit_request_6()) -> iodata().

encode_offset_commit_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_COMMIT_REQUEST, 6, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_string(MemberId),
        ?encode_array(Topics, fun encode_offset_commit_request_topic_6/1)
    ];
encode_offset_commit_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        topics => {array, offset_commit_request_topic_6}
    }).

-spec decode_offset_commit_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_6(),
    Rest :: binary().

decode_offset_commit_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_array(Topics, Bin3, Bin4, ?_decode_element(decode_offset_commit_request_topic_6)),
    {
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            topics => Topics
        },
        Bin4
    }.

-spec encode_offset_commit_request_partition_6(offset_commit_request_partition_6()) -> iodata().

encode_offset_commit_request_partition_6(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % The leader epoch of this partition.
        committed_leader_epoch := CommittedLeaderEpoch,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_int32(CommittedLeaderEpoch),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_int32(CommittedLeaderEpoch),
        ?encode_nullable_string(CommittedMetadata)
    ];
encode_offset_commit_request_partition_6(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_leader_epoch => int32,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_6(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_6(),
    Rest :: binary().

decode_offset_commit_request_partition_6(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_int32(CommittedLeaderEpoch, Bin2, Bin3),
    ?_decode_nullable_string(CommittedMetadata, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_leader_epoch => CommittedLeaderEpoch,
            committed_metadata => CommittedMetadata
        },
        Bin4
    }.

-spec encode_offset_commit_request_topic_6(offset_commit_request_topic_6()) -> iodata().

encode_offset_commit_request_topic_6(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_request_partition_6/1)
    ];
encode_offset_commit_request_topic_6(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_6}
    }).

-spec decode_offset_commit_request_topic_6(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_6(),
    Rest :: binary().

decode_offset_commit_request_topic_6(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_6)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_request_7(offset_commit_request_7()) -> iodata().

encode_offset_commit_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_COMMIT_REQUEST, 7, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_string(MemberId),
        ?encode_nullable_string(GroupInstanceId),
        ?encode_array(Topics, fun encode_offset_commit_request_topic_7/1)
    ];
encode_offset_commit_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        group_instance_id => nullable_string,
        topics => {array, offset_commit_request_topic_7}
    }).

-spec decode_offset_commit_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_7(),
    Rest :: binary().

decode_offset_commit_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_string(MemberId, Bin2, Bin3),
    ?_decode_nullable_string(GroupInstanceId, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_offset_commit_request_topic_7)),
    {
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            topics => Topics
        },
        Bin5
    }.

-spec encode_offset_commit_request_partition_7(offset_commit_request_partition_7()) -> iodata().

encode_offset_commit_request_partition_7(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % The leader epoch of this partition.
        committed_leader_epoch := CommittedLeaderEpoch,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_int32(CommittedLeaderEpoch),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_int32(CommittedLeaderEpoch),
        ?encode_nullable_string(CommittedMetadata)
    ];
encode_offset_commit_request_partition_7(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_leader_epoch => int32,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_7(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_7(),
    Rest :: binary().

decode_offset_commit_request_partition_7(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_int32(CommittedLeaderEpoch, Bin2, Bin3),
    ?_decode_nullable_string(CommittedMetadata, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_leader_epoch => CommittedLeaderEpoch,
            committed_metadata => CommittedMetadata
        },
        Bin4
    }.

-spec encode_offset_commit_request_topic_7(offset_commit_request_topic_7()) -> iodata().

encode_offset_commit_request_topic_7(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_commit_request_partition_7/1)
    ];
encode_offset_commit_request_topic_7(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_7}
    }).

-spec decode_offset_commit_request_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_7(),
    Rest :: binary().

decode_offset_commit_request_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_7)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_commit_request_8(offset_commit_request_8()) -> iodata().

encode_offset_commit_request_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?OFFSET_COMMIT_REQUEST, 8, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_array(Topics, fun encode_offset_commit_request_topic_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_request_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        group_instance_id => nullable_string,
        topics => {array, offset_commit_request_topic_8}
    }).

-spec decode_offset_commit_request_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_8(),
    Rest :: binary().

decode_offset_commit_request_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_compact_string(MemberId, Bin2, Bin3),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_offset_commit_request_topic_8)),
    ?decode_tagged_fields(
        fun decode_offset_commit_request_8_tagged_field/3,
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            topics => Topics
        },
        Bin5
    ).

-spec decode_offset_commit_request_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_offset_commit_request_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_request_partition_8(offset_commit_request_partition_8()) -> iodata().

encode_offset_commit_request_partition_8(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % The leader epoch of this partition.
        committed_leader_epoch := CommittedLeaderEpoch,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_int32(CommittedLeaderEpoch),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_int32(CommittedLeaderEpoch),
        ?encode_compact_nullable_string(CommittedMetadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_request_partition_8(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_leader_epoch => int32,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_8(),
    Rest :: binary().

decode_offset_commit_request_partition_8(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_int32(CommittedLeaderEpoch, Bin2, Bin3),
    ?_decode_compact_nullable_string(CommittedMetadata, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_offset_commit_request_partition_8_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_leader_epoch => CommittedLeaderEpoch,
            committed_metadata => CommittedMetadata
        },
        Bin4
    ).

-spec decode_offset_commit_request_partition_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_offset_commit_request_partition_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_request_topic_8(offset_commit_request_topic_8()) -> iodata().

encode_offset_commit_request_topic_8(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_offset_commit_request_partition_8/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_request_topic_8(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_8}
    }).

-spec decode_offset_commit_request_topic_8(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_8(),
    Rest :: binary().

decode_offset_commit_request_topic_8(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_8)),
    ?decode_tagged_fields(
        fun decode_offset_commit_request_topic_8_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_offset_commit_request_topic_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_offset_commit_request_topic_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_request_9(offset_commit_request_9()) -> iodata().

encode_offset_commit_request_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The generation of the group if using the generic group protocol or the member epoch if using the consumer protocol.
        generation_id_or_member_epoch := GenerationIdOrMemberEpoch,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % The topics to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_int32(GenerationIdOrMemberEpoch),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?OFFSET_COMMIT_REQUEST, 9, CorrelationId, ClientId),
        ?encode_compact_string(GroupId),
        ?encode_int32(GenerationIdOrMemberEpoch),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_array(Topics, fun encode_offset_commit_request_topic_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_request_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        generation_id_or_member_epoch => int32,
        member_id => string,
        group_instance_id => nullable_string,
        topics => {array, offset_commit_request_topic_9}
    }).

-spec decode_offset_commit_request_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_9(),
    Rest :: binary().

decode_offset_commit_request_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(GroupId, Bin0, Bin1),
    ?_decode_int32(GenerationIdOrMemberEpoch, Bin1, Bin2),
    ?_decode_compact_string(MemberId, Bin2, Bin3),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin3, Bin4),
    ?_decode_compact_array(Topics, Bin4, Bin5, ?_decode_element(decode_offset_commit_request_topic_9)),
    ?decode_tagged_fields(
        fun decode_offset_commit_request_9_tagged_field/3,
        Header#{
            group_id => GroupId,
            generation_id_or_member_epoch => GenerationIdOrMemberEpoch,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            topics => Topics
        },
        Bin5
    ).

-spec decode_offset_commit_request_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_offset_commit_request_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_request_partition_9(offset_commit_request_partition_9()) -> iodata().

encode_offset_commit_request_partition_9(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % The leader epoch of this partition.
        committed_leader_epoch := CommittedLeaderEpoch,
        % Any associated metadata the client wants to keep.
        committed_metadata := CommittedMetadata
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int64(CommittedOffset),
    ?is_int32(CommittedLeaderEpoch),
    ?is_nullable_string(CommittedMetadata)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int64(CommittedOffset),
        ?encode_int32(CommittedLeaderEpoch),
        ?encode_compact_nullable_string(CommittedMetadata),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_request_partition_9(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_leader_epoch => int32,
        committed_metadata => nullable_string
    }).

-spec decode_offset_commit_request_partition_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_partition_9(),
    Rest :: binary().

decode_offset_commit_request_partition_9(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_int32(CommittedLeaderEpoch, Bin2, Bin3),
    ?_decode_compact_nullable_string(CommittedMetadata, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_offset_commit_request_partition_9_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_leader_epoch => CommittedLeaderEpoch,
            committed_metadata => CommittedMetadata
        },
        Bin4
    ).

-spec decode_offset_commit_request_partition_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_offset_commit_request_partition_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_commit_request_topic_9(offset_commit_request_topic_9()) -> iodata().

encode_offset_commit_request_topic_9(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_offset_commit_request_partition_9/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_commit_request_topic_9(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_commit_request_partition_9}
    }).

-spec decode_offset_commit_request_topic_9(binary()) -> {Decoded, Rest} when
    Decoded :: offset_commit_request_topic_9(),
    Rest :: binary().

decode_offset_commit_request_topic_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_commit_request_partition_9)),
    ?decode_tagged_fields(
        fun decode_offset_commit_request_topic_9_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_offset_commit_request_topic_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_offset_commit_request_topic_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type offset_commit_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_commit_request_topic_0())
}.
-type offset_commit_request_partition_0() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_0() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_0())
}.
-type offset_commit_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    topics := list(offset_commit_request_topic_1())
}.
-type offset_commit_request_partition_1() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    commit_timestamp := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_1() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_1())
}.
-type offset_commit_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    retention_time_ms := integer(),
    topics := list(offset_commit_request_topic_2())
}.
-type offset_commit_request_partition_2() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_2() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_2())
}.
-type offset_commit_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    retention_time_ms := integer(),
    topics := list(offset_commit_request_topic_3())
}.
-type offset_commit_request_partition_3() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_3() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_3())
}.
-type offset_commit_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    retention_time_ms := integer(),
    topics := list(offset_commit_request_topic_4())
}.
-type offset_commit_request_partition_4() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_4() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_4())
}.
-type offset_commit_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    topics := list(offset_commit_request_topic_5())
}.
-type offset_commit_request_partition_5() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_5() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_5())
}.
-type offset_commit_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    topics := list(offset_commit_request_topic_6())
}.
-type offset_commit_request_partition_6() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_leader_epoch := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_6() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_6())
}.
-type offset_commit_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    topics := list(offset_commit_request_topic_7())
}.
-type offset_commit_request_partition_7() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_leader_epoch := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_7() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_7())
}.
-type offset_commit_request_8() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    topics := list(offset_commit_request_topic_8())
}.
-type offset_commit_request_partition_8() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_leader_epoch := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_8() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_8())
}.
-type offset_commit_request_9() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    generation_id_or_member_epoch := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    topics := list(offset_commit_request_topic_9())
}.
-type offset_commit_request_partition_9() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_leader_epoch := integer(),
    committed_metadata := binary() | null
}.
-type offset_commit_request_topic_9() :: #{
    name := binary(),
    partitions := list(offset_commit_request_partition_9())
}.
