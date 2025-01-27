-module(txn_offset_commit_request).
-export([
    encode_txn_offset_commit_request_0/1,
    decode_txn_offset_commit_request_0/1,
    encode_txn_offset_commit_request_1/1,
    decode_txn_offset_commit_request_1/1,
    encode_txn_offset_commit_request_2/1,
    decode_txn_offset_commit_request_2/1,
    encode_txn_offset_commit_request_3/1,
    decode_txn_offset_commit_request_3/1
]).
-export_type([
    txn_offset_commit_request_0/0,
    txn_offset_commit_request_partition_0/0,
    txn_offset_commit_request_topic_0/0,
    txn_offset_commit_request_1/0,
    txn_offset_commit_request_partition_1/0,
    txn_offset_commit_request_topic_1/0,
    txn_offset_commit_request_2/0,
    txn_offset_commit_request_partition_2/0,
    txn_offset_commit_request_topic_2/0,
    txn_offset_commit_request_3/0,
    txn_offset_commit_request_partition_3/0,
    txn_offset_commit_request_topic_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(TXN_OFFSET_COMMIT_REQUEST, 28).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_txn_offset_commit_request_0(txn_offset_commit_request_0()) -> iodata().

encode_txn_offset_commit_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the transaction.
        transactional_id := TransactionalId,
        % The ID of the group.
        group_id := GroupId,
        % The current producer ID in use by the transactional ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer ID.
        producer_epoch := ProducerEpoch,
        % Each topic that we want to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(TransactionalId),
    ?is_string(GroupId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?TXN_OFFSET_COMMIT_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(TransactionalId),
        ?encode_string(GroupId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_array(Topics, fun encode_txn_offset_commit_request_topic_0/1)
    ];
encode_txn_offset_commit_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => string,
        group_id => string,
        producer_id => int64,
        producer_epoch => int16,
        topics => {array, txn_offset_commit_request_topic_0}
    }).

-spec decode_txn_offset_commit_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_0(),
    Rest :: binary().

decode_txn_offset_commit_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(TransactionalId, Bin0, Bin1),
    ?_decode_string(GroupId, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_txn_offset_commit_request_topic_0)),
    {
        Header#{
            transactional_id => TransactionalId,
            group_id => GroupId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            topics => Topics
        },
        Bin5
    }.

-spec encode_txn_offset_commit_request_partition_0(txn_offset_commit_request_partition_0()) -> iodata().

encode_txn_offset_commit_request_partition_0(
    _Args = #{
        % The index of the partition within the topic.
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
encode_txn_offset_commit_request_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_metadata => nullable_string
    }).

-spec decode_txn_offset_commit_request_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_partition_0(),
    Rest :: binary().

decode_txn_offset_commit_request_partition_0(Bin0) when is_binary(Bin0) ->
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

-spec encode_txn_offset_commit_request_topic_0(txn_offset_commit_request_topic_0()) -> iodata().

encode_txn_offset_commit_request_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The partitions inside the topic that we want to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_txn_offset_commit_request_partition_0/1)
    ];
encode_txn_offset_commit_request_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_request_partition_0}
    }).

-spec decode_txn_offset_commit_request_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_topic_0(),
    Rest :: binary().

decode_txn_offset_commit_request_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_request_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_txn_offset_commit_request_1(txn_offset_commit_request_1()) -> iodata().

encode_txn_offset_commit_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the transaction.
        transactional_id := TransactionalId,
        % The ID of the group.
        group_id := GroupId,
        % The current producer ID in use by the transactional ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer ID.
        producer_epoch := ProducerEpoch,
        % Each topic that we want to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(TransactionalId),
    ?is_string(GroupId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?TXN_OFFSET_COMMIT_REQUEST, 1, CorrelationId, ClientId),
        ?encode_string(TransactionalId),
        ?encode_string(GroupId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_array(Topics, fun encode_txn_offset_commit_request_topic_1/1)
    ];
encode_txn_offset_commit_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => string,
        group_id => string,
        producer_id => int64,
        producer_epoch => int16,
        topics => {array, txn_offset_commit_request_topic_1}
    }).

-spec decode_txn_offset_commit_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_1(),
    Rest :: binary().

decode_txn_offset_commit_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(TransactionalId, Bin0, Bin1),
    ?_decode_string(GroupId, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_txn_offset_commit_request_topic_1)),
    {
        Header#{
            transactional_id => TransactionalId,
            group_id => GroupId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            topics => Topics
        },
        Bin5
    }.

-spec encode_txn_offset_commit_request_partition_1(txn_offset_commit_request_partition_1()) -> iodata().

encode_txn_offset_commit_request_partition_1(
    _Args = #{
        % The index of the partition within the topic.
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
encode_txn_offset_commit_request_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_metadata => nullable_string
    }).

-spec decode_txn_offset_commit_request_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_partition_1(),
    Rest :: binary().

decode_txn_offset_commit_request_partition_1(Bin0) when is_binary(Bin0) ->
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

-spec encode_txn_offset_commit_request_topic_1(txn_offset_commit_request_topic_1()) -> iodata().

encode_txn_offset_commit_request_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The partitions inside the topic that we want to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_txn_offset_commit_request_partition_1/1)
    ];
encode_txn_offset_commit_request_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_request_partition_1}
    }).

-spec decode_txn_offset_commit_request_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_topic_1(),
    Rest :: binary().

decode_txn_offset_commit_request_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_request_partition_1)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_txn_offset_commit_request_2(txn_offset_commit_request_2()) -> iodata().

encode_txn_offset_commit_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the transaction.
        transactional_id := TransactionalId,
        % The ID of the group.
        group_id := GroupId,
        % The current producer ID in use by the transactional ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer ID.
        producer_epoch := ProducerEpoch,
        % Each topic that we want to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(TransactionalId),
    ?is_string(GroupId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?TXN_OFFSET_COMMIT_REQUEST, 2, CorrelationId, ClientId),
        ?encode_string(TransactionalId),
        ?encode_string(GroupId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_array(Topics, fun encode_txn_offset_commit_request_topic_2/1)
    ];
encode_txn_offset_commit_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => string,
        group_id => string,
        producer_id => int64,
        producer_epoch => int16,
        topics => {array, txn_offset_commit_request_topic_2}
    }).

-spec decode_txn_offset_commit_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_2(),
    Rest :: binary().

decode_txn_offset_commit_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(TransactionalId, Bin0, Bin1),
    ?_decode_string(GroupId, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?_decode_array(Topics, Bin4, Bin5, ?_decode_element(decode_txn_offset_commit_request_topic_2)),
    {
        Header#{
            transactional_id => TransactionalId,
            group_id => GroupId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            topics => Topics
        },
        Bin5
    }.

-spec encode_txn_offset_commit_request_partition_2(txn_offset_commit_request_partition_2()) -> iodata().

encode_txn_offset_commit_request_partition_2(
    _Args = #{
        % The index of the partition within the topic.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % The leader epoch of the last consumed record.
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
encode_txn_offset_commit_request_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_leader_epoch => int32,
        committed_metadata => nullable_string
    }).

-spec decode_txn_offset_commit_request_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_partition_2(),
    Rest :: binary().

decode_txn_offset_commit_request_partition_2(Bin0) when is_binary(Bin0) ->
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

-spec encode_txn_offset_commit_request_topic_2(txn_offset_commit_request_topic_2()) -> iodata().

encode_txn_offset_commit_request_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The partitions inside the topic that we want to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_txn_offset_commit_request_partition_2/1)
    ];
encode_txn_offset_commit_request_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_request_partition_2}
    }).

-spec decode_txn_offset_commit_request_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_topic_2(),
    Rest :: binary().

decode_txn_offset_commit_request_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_request_partition_2)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_txn_offset_commit_request_3(txn_offset_commit_request_3()) -> iodata().

encode_txn_offset_commit_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the transaction.
        transactional_id := TransactionalId,
        % The ID of the group.
        group_id := GroupId,
        % The current producer ID in use by the transactional ID.
        producer_id := ProducerId,
        % The current epoch associated with the producer ID.
        producer_epoch := ProducerEpoch,
        % The generation of the consumer.
        generation_id := GenerationId,
        % The member ID assigned by the group coordinator.
        member_id := MemberId,
        % The unique identifier of the consumer instance provided by end user.
        group_instance_id := GroupInstanceId,
        % Each topic that we want to commit offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(TransactionalId),
    ?is_string(GroupId),
    ?is_int64(ProducerId),
    ?is_int16(ProducerEpoch),
    ?is_int32(GenerationId),
    ?is_string(MemberId),
    ?is_nullable_string(GroupInstanceId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?TXN_OFFSET_COMMIT_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_string(TransactionalId),
        ?encode_compact_string(GroupId),
        ?encode_int64(ProducerId),
        ?encode_int16(ProducerEpoch),
        ?encode_int32(GenerationId),
        ?encode_compact_string(MemberId),
        ?encode_compact_nullable_string(GroupInstanceId),
        ?encode_compact_array(Topics, fun encode_txn_offset_commit_request_topic_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_txn_offset_commit_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        transactional_id => string,
        group_id => string,
        producer_id => int64,
        producer_epoch => int16,
        generation_id => int32,
        member_id => string,
        group_instance_id => nullable_string,
        topics => {array, txn_offset_commit_request_topic_3}
    }).

-spec decode_txn_offset_commit_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_3(),
    Rest :: binary().

decode_txn_offset_commit_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_string(TransactionalId, Bin0, Bin1),
    ?_decode_compact_string(GroupId, Bin1, Bin2),
    ?_decode_int64(ProducerId, Bin2, Bin3),
    ?_decode_int16(ProducerEpoch, Bin3, Bin4),
    ?_decode_int32(GenerationId, Bin4, Bin5),
    ?_decode_compact_string(MemberId, Bin5, Bin6),
    ?_decode_compact_nullable_string(GroupInstanceId, Bin6, Bin7),
    ?_decode_compact_array(Topics, Bin7, Bin8, ?_decode_element(decode_txn_offset_commit_request_topic_3)),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_request_3_tagged_field/3,
        Header#{
            transactional_id => TransactionalId,
            group_id => GroupId,
            producer_id => ProducerId,
            producer_epoch => ProducerEpoch,
            generation_id => GenerationId,
            member_id => MemberId,
            group_instance_id => GroupInstanceId,
            topics => Topics
        },
        Bin8
    ).

-spec decode_txn_offset_commit_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_txn_offset_commit_request_partition_3(txn_offset_commit_request_partition_3()) -> iodata().

encode_txn_offset_commit_request_partition_3(
    _Args = #{
        % The index of the partition within the topic.
        partition_index := PartitionIndex,
        % The message offset to be committed.
        committed_offset := CommittedOffset,
        % The leader epoch of the last consumed record.
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
encode_txn_offset_commit_request_partition_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        committed_offset => int64,
        committed_leader_epoch => int32,
        committed_metadata => nullable_string
    }).

-spec decode_txn_offset_commit_request_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_partition_3(),
    Rest :: binary().

decode_txn_offset_commit_request_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int64(CommittedOffset, Bin1, Bin2),
    ?_decode_int32(CommittedLeaderEpoch, Bin2, Bin3),
    ?_decode_compact_nullable_string(CommittedMetadata, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_request_partition_3_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            committed_offset => CommittedOffset,
            committed_leader_epoch => CommittedLeaderEpoch,
            committed_metadata => CommittedMetadata
        },
        Bin4
    ).

-spec decode_txn_offset_commit_request_partition_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_request_partition_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_txn_offset_commit_request_topic_3(txn_offset_commit_request_topic_3()) -> iodata().

encode_txn_offset_commit_request_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The partitions inside the topic that we want to commit offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_txn_offset_commit_request_partition_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_txn_offset_commit_request_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, txn_offset_commit_request_partition_3}
    }).

-spec decode_txn_offset_commit_request_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: txn_offset_commit_request_topic_3(),
    Rest :: binary().

decode_txn_offset_commit_request_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_txn_offset_commit_request_partition_3)),
    ?decode_tagged_fields(
        fun decode_txn_offset_commit_request_topic_3_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_txn_offset_commit_request_topic_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_txn_offset_commit_request_topic_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type txn_offset_commit_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary(),
    group_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    topics := list(txn_offset_commit_request_topic_0())
}.
-type txn_offset_commit_request_partition_0() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_metadata := binary() | null
}.
-type txn_offset_commit_request_topic_0() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_request_partition_0())
}.
-type txn_offset_commit_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary(),
    group_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    topics := list(txn_offset_commit_request_topic_1())
}.
-type txn_offset_commit_request_partition_1() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_metadata := binary() | null
}.
-type txn_offset_commit_request_topic_1() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_request_partition_1())
}.
-type txn_offset_commit_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary(),
    group_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    topics := list(txn_offset_commit_request_topic_2())
}.
-type txn_offset_commit_request_partition_2() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_leader_epoch := integer(),
    committed_metadata := binary() | null
}.
-type txn_offset_commit_request_topic_2() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_request_partition_2())
}.
-type txn_offset_commit_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    transactional_id := binary(),
    group_id := binary(),
    producer_id := integer(),
    producer_epoch := integer(),
    generation_id := integer(),
    member_id := binary(),
    group_instance_id := binary() | null,
    topics := list(txn_offset_commit_request_topic_3())
}.
-type txn_offset_commit_request_partition_3() :: #{
    partition_index := integer(),
    committed_offset := integer(),
    committed_leader_epoch := integer(),
    committed_metadata := binary() | null
}.
-type txn_offset_commit_request_topic_3() :: #{
    name := binary(),
    partitions := list(txn_offset_commit_request_partition_3())
}.
