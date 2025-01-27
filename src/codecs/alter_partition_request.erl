-module(alter_partition_request).
-export([
    encode_alter_partition_request_0/1,
    decode_alter_partition_request_0/1,
    encode_alter_partition_request_1/1,
    decode_alter_partition_request_1/1,
    encode_alter_partition_request_2/1,
    decode_alter_partition_request_2/1,
    encode_alter_partition_request_3/1,
    decode_alter_partition_request_3/1
]).
-export_type([
    alter_partition_request_0/0,
    partition_data_0/0,
    topic_data_0/0,
    alter_partition_request_1/0,
    partition_data_1/0,
    topic_data_1/0,
    alter_partition_request_2/0,
    partition_data_2/0,
    topic_data_2/0,
    alter_partition_request_3/0,
    broker_state_3/0,
    partition_data_3/0,
    topic_data_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ALTER_PARTITION_REQUEST, 56).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_alter_partition_request_0(alter_partition_request_0()) -> iodata().

encode_alter_partition_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the requesting broker
        broker_id := BrokerId,
        % The epoch of the requesting broker
        broker_epoch := BrokerEpoch,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?ALTER_PARTITION_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(Topics, fun encode_topic_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64,
        topics => {array, topic_data_0}
    }).

-spec decode_alter_partition_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_request_0(),
    Rest :: binary().

decode_alter_partition_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_data_0)),
    ?decode_tagged_fields(
        fun decode_alter_partition_request_0_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch,
            topics => Topics
        },
        Bin3
    ).

-spec decode_alter_partition_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_partition_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The leader epoch of this partition
        leader_epoch := LeaderEpoch,
        % The ISR for this partition. Deprecated since version 3.
        new_isr := NewIsr,
        % The expected epoch of the partition which is being updated. For legacy cluster this is the ZkVersion in the LeaderAndIsr request.
        partition_epoch := PartitionEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderEpoch),
    ?is_array(NewIsr),
    ?is_int32(PartitionEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(NewIsr, ?encode_int32_),
        ?encode_int32(PartitionEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        leader_epoch => int32,
        new_isr => {array, int32},
        partition_epoch => int32
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?_decode_compact_array(NewIsr, Bin2, Bin3, ?decode_int32_),
    ?_decode_int32(PartitionEpoch, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_partition_data_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            leader_epoch => LeaderEpoch,
            new_isr => NewIsr,
            partition_epoch => PartitionEpoch
        },
        Bin4
    ).

-spec decode_partition_data_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_partition_data_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_0(topic_data_0()) -> iodata().

encode_topic_data_0(
    _Args = #{
        % The name of the topic to alter ISRs for
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
    AccOut :: Acc.

decode_topic_data_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_partition_request_1(alter_partition_request_1()) -> iodata().

encode_alter_partition_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the requesting broker
        broker_id := BrokerId,
        % The epoch of the requesting broker
        broker_epoch := BrokerEpoch,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?ALTER_PARTITION_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(Topics, fun encode_topic_data_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64,
        topics => {array, topic_data_1}
    }).

-spec decode_alter_partition_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_request_1(),
    Rest :: binary().

decode_alter_partition_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_data_1)),
    ?decode_tagged_fields(
        fun decode_alter_partition_request_1_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch,
            topics => Topics
        },
        Bin3
    ).

-spec decode_alter_partition_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_partition_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_1(partition_data_1()) -> iodata().

encode_partition_data_1(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The leader epoch of this partition
        leader_epoch := LeaderEpoch,
        % The ISR for this partition. Deprecated since version 3.
        new_isr := NewIsr,
        % 1 if the partition is recovering from an unclean leader election; 0 otherwise.
        leader_recovery_state := LeaderRecoveryState,
        % The expected epoch of the partition which is being updated. For legacy cluster this is the ZkVersion in the LeaderAndIsr request.
        partition_epoch := PartitionEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderEpoch),
    ?is_array(NewIsr),
    ?is_int8(LeaderRecoveryState),
    ?is_int32(PartitionEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(NewIsr, ?encode_int32_),
        ?encode_int8(LeaderRecoveryState),
        ?encode_int32(PartitionEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        leader_epoch => int32,
        new_isr => {array, int32},
        leader_recovery_state => int8,
        partition_epoch => int32
    }).

-spec decode_partition_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_1(),
    Rest :: binary().

decode_partition_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?_decode_compact_array(NewIsr, Bin2, Bin3, ?decode_int32_),
    ?_decode_int8(LeaderRecoveryState, Bin3, Bin4),
    ?_decode_int32(PartitionEpoch, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_partition_data_1_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            leader_epoch => LeaderEpoch,
            new_isr => NewIsr,
            leader_recovery_state => LeaderRecoveryState,
            partition_epoch => PartitionEpoch
        },
        Bin5
    ).

-spec decode_partition_data_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_partition_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_1(topic_data_1()) -> iodata().

encode_topic_data_1(
    _Args = #{
        % The name of the topic to alter ISRs for
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
    AccOut :: Acc.

decode_topic_data_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_partition_request_2(alter_partition_request_2()) -> iodata().

encode_alter_partition_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the requesting broker
        broker_id := BrokerId,
        % The epoch of the requesting broker
        broker_epoch := BrokerEpoch,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?ALTER_PARTITION_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(Topics, fun encode_topic_data_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64,
        topics => {array, topic_data_2}
    }).

-spec decode_alter_partition_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_request_2(),
    Rest :: binary().

decode_alter_partition_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_data_2)),
    ?decode_tagged_fields(
        fun decode_alter_partition_request_2_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch,
            topics => Topics
        },
        Bin3
    ).

-spec decode_alter_partition_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_partition_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_2(partition_data_2()) -> iodata().

encode_partition_data_2(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The leader epoch of this partition
        leader_epoch := LeaderEpoch,
        % The ISR for this partition. Deprecated since version 3.
        new_isr := NewIsr,
        % 1 if the partition is recovering from an unclean leader election; 0 otherwise.
        leader_recovery_state := LeaderRecoveryState,
        % The expected epoch of the partition which is being updated. For legacy cluster this is the ZkVersion in the LeaderAndIsr request.
        partition_epoch := PartitionEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderEpoch),
    ?is_array(NewIsr),
    ?is_int8(LeaderRecoveryState),
    ?is_int32(PartitionEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(NewIsr, ?encode_int32_),
        ?encode_int8(LeaderRecoveryState),
        ?encode_int32(PartitionEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_2(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        leader_epoch => int32,
        new_isr => {array, int32},
        leader_recovery_state => int8,
        partition_epoch => int32
    }).

-spec decode_partition_data_2(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_2(),
    Rest :: binary().

decode_partition_data_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?_decode_compact_array(NewIsr, Bin2, Bin3, ?decode_int32_),
    ?_decode_int8(LeaderRecoveryState, Bin3, Bin4),
    ?_decode_int32(PartitionEpoch, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_partition_data_2_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            leader_epoch => LeaderEpoch,
            new_isr => NewIsr,
            leader_recovery_state => LeaderRecoveryState,
            partition_epoch => PartitionEpoch
        },
        Bin5
    ).

-spec decode_partition_data_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_partition_data_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_2(topic_data_2()) -> iodata().

encode_topic_data_2(
    _Args = #{
        % The ID of the topic to alter ISRs for
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
    AccOut :: Acc.

decode_topic_data_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_partition_request_3(alter_partition_request_3()) -> iodata().

encode_alter_partition_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the requesting broker
        broker_id := BrokerId,
        % The epoch of the requesting broker
        broker_epoch := BrokerEpoch,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?ALTER_PARTITION_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(Topics, fun encode_topic_data_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64,
        topics => {array, topic_data_3}
    }).

-spec decode_alter_partition_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_request_3(),
    Rest :: binary().

decode_alter_partition_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_data_3)),
    ?decode_tagged_fields(
        fun decode_alter_partition_request_3_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch,
            topics => Topics
        },
        Bin3
    ).

-spec decode_alter_partition_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_alter_partition_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_broker_state_3(broker_state_3()) -> iodata().

encode_broker_state_3(
    _Args = #{
        % The ID of the broker.
        broker_id := BrokerId,
        % The epoch of the broker. It will be -1 if the epoch check is not supported.
        broker_epoch := BrokerEpoch
    }
) when
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch)
->
    [
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_broker_state_3(Args) ->
    ?encoder_error(Args, #{
        broker_id => int32,
        broker_epoch => int64
    }).

-spec decode_broker_state_3(binary()) -> {Decoded, Rest} when
    Decoded :: broker_state_3(),
    Rest :: binary().

decode_broker_state_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_broker_state_3_tagged_field/3,
        #{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch
        },
        Bin2
    ).

-spec decode_broker_state_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_broker_state_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_3(partition_data_3()) -> iodata().

encode_partition_data_3(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The leader epoch of this partition
        leader_epoch := LeaderEpoch,
        new_isr_with_epochs := NewIsrWithEpochs,
        % 1 if the partition is recovering from an unclean leader election; 0 otherwise.
        leader_recovery_state := LeaderRecoveryState,
        % The expected epoch of the partition which is being updated. For legacy cluster this is the ZkVersion in the LeaderAndIsr request.
        partition_epoch := PartitionEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderEpoch),
    ?is_array(NewIsrWithEpochs),
    ?is_int8(LeaderRecoveryState),
    ?is_int32(PartitionEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderEpoch),
        ?encode_compact_array(NewIsrWithEpochs, fun encode_broker_state_3/1),
        ?encode_int8(LeaderRecoveryState),
        ?encode_int32(PartitionEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_3(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        leader_epoch => int32,
        new_isr_with_epochs => {array, broker_state_3},
        leader_recovery_state => int8,
        partition_epoch => int32
    }).

-spec decode_partition_data_3(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_3(),
    Rest :: binary().

decode_partition_data_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?_decode_compact_array(NewIsrWithEpochs, Bin2, Bin3, ?_decode_element(decode_broker_state_3)),
    ?_decode_int8(LeaderRecoveryState, Bin3, Bin4),
    ?_decode_int32(PartitionEpoch, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_partition_data_3_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            leader_epoch => LeaderEpoch,
            new_isr_with_epochs => NewIsrWithEpochs,
            leader_recovery_state => LeaderRecoveryState,
            partition_epoch => PartitionEpoch
        },
        Bin5
    ).

-spec decode_partition_data_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_partition_data_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_data_3(topic_data_3()) -> iodata().

encode_topic_data_3(
    _Args = #{
        % The ID of the topic to alter ISRs for
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
    AccOut :: Acc.

decode_topic_data_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type alter_partition_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer(),
    topics := list(topic_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    leader_epoch := integer(),
    new_isr := list(integer()),
    partition_epoch := integer()
}.
-type topic_data_0() :: #{
    topic_name := binary(),
    partitions := list(partition_data_0())
}.
-type alter_partition_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer(),
    topics := list(topic_data_1())
}.
-type partition_data_1() :: #{
    partition_index := integer(),
    leader_epoch := integer(),
    new_isr := list(integer()),
    leader_recovery_state := integer(),
    partition_epoch := integer()
}.
-type topic_data_1() :: #{
    topic_name := binary(),
    partitions := list(partition_data_1())
}.
-type alter_partition_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer(),
    topics := list(topic_data_2())
}.
-type partition_data_2() :: #{
    partition_index := integer(),
    leader_epoch := integer(),
    new_isr := list(integer()),
    leader_recovery_state := integer(),
    partition_epoch := integer()
}.
-type topic_data_2() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_2())
}.
-type alter_partition_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer(),
    topics := list(topic_data_3())
}.
-type broker_state_3() :: #{
    broker_id := integer(),
    broker_epoch := integer()
}.
-type partition_data_3() :: #{
    partition_index := integer(),
    leader_epoch := integer(),
    new_isr_with_epochs := list(broker_state_3()),
    leader_recovery_state := integer(),
    partition_epoch := integer()
}.
-type topic_data_3() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_3())
}.
