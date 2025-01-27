-module(end_quorum_epoch_response).
-export([
    encode_end_quorum_epoch_response_0/1,
    decode_end_quorum_epoch_response_0/1
]).
-export_type([
    end_quorum_epoch_response_0/0,
    partition_data_0/0,
    topic_data_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_end_quorum_epoch_response_0(end_quorum_epoch_response_0()) -> iodata().

encode_end_quorum_epoch_response_0(
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
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode),
        ?encode_array(Topics, fun encode_topic_data_0/1)
    ];
encode_end_quorum_epoch_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        topics => {array, topic_data_0}
    }).

-spec decode_end_quorum_epoch_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: end_quorum_epoch_response_0(),
    Rest :: binary().

decode_end_quorum_epoch_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_topic_data_0)),
    {
        Header#{
            error_code => ErrorCode,
            topics => Topics
        },
        Bin2
    }.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        error_code := ErrorCode,
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch)
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        leader_id => int32,
        leader_epoch => int32
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    {
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch
        },
        Bin4
    }.

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
        ?encode_string(TopicName),
        ?encode_array(Partitions, fun encode_partition_data_0/1)
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
    ?_decode_string(TopicName, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_0)),
    {
        #{
            topic_name => TopicName,
            partitions => Partitions
        },
        Bin2
    }.

-type end_quorum_epoch_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    topics := list(topic_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    error_code := integer(),
    leader_id := integer(),
    leader_epoch := integer()
}.
-type topic_data_0() :: #{
    topic_name := binary(),
    partitions := list(partition_data_0())
}.
