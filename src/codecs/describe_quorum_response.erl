-module(describe_quorum_response).
-export([
    encode_describe_quorum_response_0/1,
    decode_describe_quorum_response_0/1,
    encode_describe_quorum_response_1/1,
    decode_describe_quorum_response_1/1
]).
-export_type([
    describe_quorum_response_0/0,
    partition_data_0/0,
    topic_data_0/0,
    replica_state_0/0,
    describe_quorum_response_1/0,
    partition_data_1/0,
    topic_data_1/0,
    replica_state_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_quorum_response_0(describe_quorum_response_0()) -> iodata().

encode_describe_quorum_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top level error code.
        error_code := ErrorCode,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_topic_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_quorum_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        topics => {array, topic_data_0}
    }).

-spec decode_describe_quorum_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_quorum_response_0(),
    Rest :: binary().

decode_describe_quorum_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_topic_data_0)),
    ?decode_tagged_fields(
        fun decode_describe_quorum_response_0_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            topics => Topics
        },
        Bin2
    ).

-spec decode_describe_quorum_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_quorum_response_0().

decode_describe_quorum_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        error_code := ErrorCode,
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch,
        high_watermark := HighWatermark,
        current_voters := CurrentVoters,
        observers := Observers
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch),
    ?is_int64(HighWatermark),
    ?is_array(CurrentVoters),
    ?is_array(Observers)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_int64(HighWatermark),
        ?encode_compact_array(CurrentVoters, fun encode_replica_state_0/1),
        ?encode_compact_array(Observers, fun encode_replica_state_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        leader_id => int32,
        leader_epoch => int32,
        high_watermark => int64,
        current_voters => {array, replica_state_0},
        observers => {array, replica_state_0}
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_int64(HighWatermark, Bin4, Bin5),
    ?_decode_compact_array(CurrentVoters, Bin5, Bin6, ?_decode_element(decode_replica_state_0)),
    ?_decode_compact_array(Observers, Bin6, Bin7, ?_decode_element(decode_replica_state_0)),
    ?decode_tagged_fields(
        fun decode_partition_data_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            high_watermark => HighWatermark,
            current_voters => CurrentVoters,
            observers => Observers
        },
        Bin7
    ).

-spec decode_partition_data_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_0().

decode_partition_data_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_0(topic_data_0()) -> iodata().

encode_topic_data_0(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        partitions := Partitions
    }
) when
    ?is_string(TopicName),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_compact_array(Partitions, fun encode_partition_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_data_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partitions => {array, partition_data_0}
    }).

-spec decode_topic_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_data_0(),
    Rest :: binary().

decode_topic_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_0)),
    ?decode_tagged_fields(
        fun decode_topic_data_0_tagged_field/3,
        #{
            topic_name => TopicName,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_data_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_data_0().

decode_topic_data_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_replica_state_0(replica_state_0()) -> iodata().

encode_replica_state_0(
    _Args = #{
        replica_id := ReplicaId,
        % The last known log end offset of the follower or -1 if it is unknown
        log_end_offset := LogEndOffset
    }
) when
    ?is_int32(ReplicaId),
    ?is_int64(LogEndOffset)
->
    [
        ?encode_int32(ReplicaId),
        ?encode_int64(LogEndOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_replica_state_0(Args) ->
    ?encoder_error(Args, #{
        replica_id => int32,
        log_end_offset => int64
    }).

-spec decode_replica_state_0(binary()) -> {Decoded, Rest} when
    Decoded :: replica_state_0(),
    Rest :: binary().

decode_replica_state_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int64(LogEndOffset, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_replica_state_0_tagged_field/3,
        #{
            replica_id => ReplicaId,
            log_end_offset => LogEndOffset
        },
        Bin2
    ).

-spec decode_replica_state_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: replica_state_0().

decode_replica_state_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_quorum_response_1(describe_quorum_response_1()) -> iodata().

encode_describe_quorum_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The top level error code.
        error_code := ErrorCode,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_topic_data_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_quorum_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        topics => {array, topic_data_1}
    }).

-spec decode_describe_quorum_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_quorum_response_1(),
    Rest :: binary().

decode_describe_quorum_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_topic_data_1)),
    ?decode_tagged_fields(
        fun decode_describe_quorum_response_1_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            topics => Topics
        },
        Bin2
    ).

-spec decode_describe_quorum_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_quorum_response_1().

decode_describe_quorum_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_1(partition_data_1()) -> iodata().

encode_partition_data_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        error_code := ErrorCode,
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch,
        high_watermark := HighWatermark,
        current_voters := CurrentVoters,
        observers := Observers
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch),
    ?is_int64(HighWatermark),
    ?is_array(CurrentVoters),
    ?is_array(Observers)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_int64(HighWatermark),
        ?encode_compact_array(CurrentVoters, fun encode_replica_state_1/1),
        ?encode_compact_array(Observers, fun encode_replica_state_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        leader_id => int32,
        leader_epoch => int32,
        high_watermark => int64,
        current_voters => {array, replica_state_1},
        observers => {array, replica_state_1}
    }).

-spec decode_partition_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_1(),
    Rest :: binary().

decode_partition_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_int64(HighWatermark, Bin4, Bin5),
    ?_decode_compact_array(CurrentVoters, Bin5, Bin6, ?_decode_element(decode_replica_state_1)),
    ?_decode_compact_array(Observers, Bin6, Bin7, ?_decode_element(decode_replica_state_1)),
    ?decode_tagged_fields(
        fun decode_partition_data_1_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            high_watermark => HighWatermark,
            current_voters => CurrentVoters,
            observers => Observers
        },
        Bin7
    ).

-spec decode_partition_data_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_1().

decode_partition_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_1(topic_data_1()) -> iodata().

encode_topic_data_1(
    _Args = #{
        % The topic name.
        topic_name := TopicName,
        partitions := Partitions
    }
) when
    ?is_string(TopicName),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_compact_array(Partitions, fun encode_partition_data_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_data_1(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partitions => {array, partition_data_1}
    }).

-spec decode_topic_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: topic_data_1(),
    Rest :: binary().

decode_topic_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_1)),
    ?decode_tagged_fields(
        fun decode_topic_data_1_tagged_field/3,
        #{
            topic_name => TopicName,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_data_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_data_1().

decode_topic_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_replica_state_1(replica_state_1()) -> iodata().

encode_replica_state_1(
    _Args = #{
        replica_id := ReplicaId,
        % The last known log end offset of the follower or -1 if it is unknown
        log_end_offset := LogEndOffset,
        % The last known leader wall clock time time when a follower fetched from the leader. This is reported as -1 both for the current leader or if it is unknown for a voter
        last_fetch_timestamp := LastFetchTimestamp,
        % The leader wall clock append time of the offset for which the follower made the most recent fetch request. This is reported as the current time for the leader and -1 if unknown for a voter
        last_caught_up_timestamp := LastCaughtUpTimestamp
    }
) when
    ?is_int32(ReplicaId),
    ?is_int64(LogEndOffset),
    ?is_int64(LastFetchTimestamp),
    ?is_int64(LastCaughtUpTimestamp)
->
    [
        ?encode_int32(ReplicaId),
        ?encode_int64(LogEndOffset),
        ?encode_int64(LastFetchTimestamp),
        ?encode_int64(LastCaughtUpTimestamp),
        ?EMPTY_TAG_BUFFER
    ];
encode_replica_state_1(Args) ->
    ?encoder_error(Args, #{
        replica_id => int32,
        log_end_offset => int64,
        last_fetch_timestamp => int64,
        last_caught_up_timestamp => int64
    }).

-spec decode_replica_state_1(binary()) -> {Decoded, Rest} when
    Decoded :: replica_state_1(),
    Rest :: binary().

decode_replica_state_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int64(LogEndOffset, Bin1, Bin2),
    ?_decode_int64(LastFetchTimestamp, Bin2, Bin3),
    ?_decode_int64(LastCaughtUpTimestamp, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_replica_state_1_tagged_field/3,
        #{
            replica_id => ReplicaId,
            log_end_offset => LogEndOffset,
            last_fetch_timestamp => LastFetchTimestamp,
            last_caught_up_timestamp => LastCaughtUpTimestamp
        },
        Bin4
    ).

-spec decode_replica_state_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: replica_state_1().

decode_replica_state_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_quorum_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    topics := list(topic_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    error_code := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    high_watermark := integer(),
    current_voters := list(replica_state_0()),
    observers := list(replica_state_0())
}.
-type topic_data_0() :: #{
    topic_name := binary(),
    partitions := list(partition_data_0())
}.
-type replica_state_0() :: #{
    replica_id := integer(),
    log_end_offset := integer()
}.
-type describe_quorum_response_1() :: #{
    correlation_id => integer(),
    error_code := integer(),
    topics := list(topic_data_1())
}.
-type partition_data_1() :: #{
    partition_index := integer(),
    error_code := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    high_watermark := integer(),
    current_voters := list(replica_state_1()),
    observers := list(replica_state_1())
}.
-type topic_data_1() :: #{
    topic_name := binary(),
    partitions := list(partition_data_1())
}.
-type replica_state_1() :: #{
    replica_id := integer(),
    log_end_offset := integer(),
    last_fetch_timestamp := integer(),
    last_caught_up_timestamp := integer()
}.
