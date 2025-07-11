-module(alter_partition_response).
-export([
    encode_alter_partition_response_0/1,
    decode_alter_partition_response_0/1,
    encode_alter_partition_response_1/1,
    decode_alter_partition_response_1/1,
    encode_alter_partition_response_2/1,
    decode_alter_partition_response_2/1,
    encode_alter_partition_response_3/1,
    decode_alter_partition_response_3/1
]).
-export_type([
    alter_partition_response_0/0,
    partition_data_0/0,
    topic_data_0/0,
    alter_partition_response_1/0,
    partition_data_1/0,
    topic_data_1/0,
    alter_partition_response_2/0,
    partition_data_2/0,
    topic_data_2/0,
    alter_partition_response_3/0,
    partition_data_3/0,
    topic_data_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_alter_partition_response_0(alter_partition_response_0()) -> iodata().

encode_alter_partition_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code
        error_code := ErrorCode,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_topic_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        topics => {array, topic_data_0}
    }).

-spec decode_alter_partition_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_response_0(),
    Rest :: binary().

decode_alter_partition_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_data_0)),
    ?decode_tagged_fields(
        fun decode_alter_partition_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            topics => Topics
        },
        Bin3
    ).

-spec decode_alter_partition_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_partition_response_0().

decode_alter_partition_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The partition level error code
        error_code := ErrorCode,
        % The broker ID of the leader.
        leader_id := LeaderId,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % The current epoch for the partition for KRaft controllers. The current ZK version for the legacy controllers.
        partition_epoch := PartitionEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int32(PartitionEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        leader_id => int32,
        leader_epoch => int32,
        isr => {array, int32},
        partition_epoch => int32
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_partition_data_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            partition_epoch => PartitionEpoch
        },
        Bin6
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
        % The name of the topic
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

-spec encode_alter_partition_response_1(alter_partition_response_1()) -> iodata().

encode_alter_partition_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code
        error_code := ErrorCode,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_topic_data_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        topics => {array, topic_data_1}
    }).

-spec decode_alter_partition_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_response_1(),
    Rest :: binary().

decode_alter_partition_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_data_1)),
    ?decode_tagged_fields(
        fun decode_alter_partition_response_1_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            topics => Topics
        },
        Bin3
    ).

-spec decode_alter_partition_response_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_partition_response_1().

decode_alter_partition_response_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_1(partition_data_1()) -> iodata().

encode_partition_data_1(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The partition level error code
        error_code := ErrorCode,
        % The broker ID of the leader.
        leader_id := LeaderId,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % 1 if the partition is recovering from an unclean leader election; 0 otherwise.
        leader_recovery_state := LeaderRecoveryState,
        % The current epoch for the partition for KRaft controllers. The current ZK version for the legacy controllers.
        partition_epoch := PartitionEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int8(LeaderRecoveryState),
    ?is_int32(PartitionEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int8(LeaderRecoveryState),
        ?encode_int32(PartitionEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        leader_id => int32,
        leader_epoch => int32,
        isr => {array, int32},
        leader_recovery_state => int8,
        partition_epoch => int32
    }).

-spec decode_partition_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_1(),
    Rest :: binary().

decode_partition_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int8(LeaderRecoveryState, Bin5, Bin6),
    ?_decode_int32(PartitionEpoch, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_partition_data_1_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            leader_recovery_state => LeaderRecoveryState,
            partition_epoch => PartitionEpoch
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
        % The name of the topic
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

-spec encode_alter_partition_response_2(alter_partition_response_2()) -> iodata().

encode_alter_partition_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code
        error_code := ErrorCode,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_topic_data_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        topics => {array, topic_data_2}
    }).

-spec decode_alter_partition_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_response_2(),
    Rest :: binary().

decode_alter_partition_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_data_2)),
    ?decode_tagged_fields(
        fun decode_alter_partition_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            topics => Topics
        },
        Bin3
    ).

-spec decode_alter_partition_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_partition_response_2().

decode_alter_partition_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_2(partition_data_2()) -> iodata().

encode_partition_data_2(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The partition level error code
        error_code := ErrorCode,
        % The broker ID of the leader.
        leader_id := LeaderId,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % 1 if the partition is recovering from an unclean leader election; 0 otherwise.
        leader_recovery_state := LeaderRecoveryState,
        % The current epoch for the partition for KRaft controllers. The current ZK version for the legacy controllers.
        partition_epoch := PartitionEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int8(LeaderRecoveryState),
    ?is_int32(PartitionEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int8(LeaderRecoveryState),
        ?encode_int32(PartitionEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        leader_id => int32,
        leader_epoch => int32,
        isr => {array, int32},
        leader_recovery_state => int8,
        partition_epoch => int32
    }).

-spec decode_partition_data_2(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_2(),
    Rest :: binary().

decode_partition_data_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int8(LeaderRecoveryState, Bin5, Bin6),
    ?_decode_int32(PartitionEpoch, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_partition_data_2_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            leader_recovery_state => LeaderRecoveryState,
            partition_epoch => PartitionEpoch
        },
        Bin7
    ).

-spec decode_partition_data_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_2().

decode_partition_data_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_2(topic_data_2()) -> iodata().

encode_topic_data_2(
    _Args = #{
        % The ID of the topic
        topic_id := TopicId,
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_partition_data_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_data_2(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, partition_data_2}
    }).

-spec decode_topic_data_2(binary()) -> {Decoded, Rest} when
    Decoded :: topic_data_2(),
    Rest :: binary().

decode_topic_data_2(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_2)),
    ?decode_tagged_fields(
        fun decode_topic_data_2_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_data_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_data_2().

decode_topic_data_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_partition_response_3(alter_partition_response_3()) -> iodata().

encode_alter_partition_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code
        error_code := ErrorCode,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_topic_data_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        topics => {array, topic_data_3}
    }).

-spec decode_alter_partition_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_response_3(),
    Rest :: binary().

decode_alter_partition_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_data_3)),
    ?decode_tagged_fields(
        fun decode_alter_partition_response_3_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            topics => Topics
        },
        Bin3
    ).

-spec decode_alter_partition_response_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_partition_response_3().

decode_alter_partition_response_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_3(partition_data_3()) -> iodata().

encode_partition_data_3(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The partition level error code
        error_code := ErrorCode,
        % The broker ID of the leader.
        leader_id := LeaderId,
        % The leader epoch.
        leader_epoch := LeaderEpoch,
        % The in-sync replica IDs.
        isr := Isr,
        % 1 if the partition is recovering from an unclean leader election; 0 otherwise.
        leader_recovery_state := LeaderRecoveryState,
        % The current epoch for the partition for KRaft controllers. The current ZK version for the legacy controllers.
        partition_epoch := PartitionEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch),
    ?is_array(Isr),
    ?is_int8(LeaderRecoveryState),
    ?is_int32(PartitionEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(Isr, ?encode_int32_),
        ?encode_int8(LeaderRecoveryState),
        ?encode_int32(PartitionEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        leader_id => int32,
        leader_epoch => int32,
        isr => {array, int32},
        leader_recovery_state => int8,
        partition_epoch => int32
    }).

-spec decode_partition_data_3(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_3(),
    Rest :: binary().

decode_partition_data_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_compact_array(Isr, Bin4, Bin5, ?decode_int32_),
    ?_decode_int8(LeaderRecoveryState, Bin5, Bin6),
    ?_decode_int32(PartitionEpoch, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_partition_data_3_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            isr => Isr,
            leader_recovery_state => LeaderRecoveryState,
            partition_epoch => PartitionEpoch
        },
        Bin7
    ).

-spec decode_partition_data_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_data_3().

decode_partition_data_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_3(topic_data_3()) -> iodata().

encode_topic_data_3(
    _Args = #{
        % The ID of the topic
        topic_id := TopicId,
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_partition_data_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_data_3(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, partition_data_3}
    }).

-spec decode_topic_data_3(binary()) -> {Decoded, Rest} when
    Decoded :: topic_data_3(),
    Rest :: binary().

decode_topic_data_3(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_3)),
    ?decode_tagged_fields(
        fun decode_topic_data_3_tagged_field/3,
        #{
            topic_id => TopicId,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_data_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_data_3().

decode_topic_data_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type alter_partition_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    topics := list(topic_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    error_code := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    partition_epoch := integer()
}.
-type topic_data_0() :: #{
    topic_name := binary(),
    partitions := list(partition_data_0())
}.
-type alter_partition_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    topics := list(topic_data_1())
}.
-type partition_data_1() :: #{
    partition_index := integer(),
    error_code := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    leader_recovery_state := integer(),
    partition_epoch := integer()
}.
-type topic_data_1() :: #{
    topic_name := binary(),
    partitions := list(partition_data_1())
}.
-type alter_partition_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    topics := list(topic_data_2())
}.
-type partition_data_2() :: #{
    partition_index := integer(),
    error_code := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    leader_recovery_state := integer(),
    partition_epoch := integer()
}.
-type topic_data_2() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_2())
}.
-type alter_partition_response_3() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    topics := list(topic_data_3())
}.
-type partition_data_3() :: #{
    partition_index := integer(),
    error_code := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    isr := list(integer()),
    leader_recovery_state := integer(),
    partition_epoch := integer()
}.
-type topic_data_3() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_3())
}.
