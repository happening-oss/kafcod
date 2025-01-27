-module(begin_quorum_epoch_request).
-export([
    encode_begin_quorum_epoch_request_0/1,
    decode_begin_quorum_epoch_request_0/1
]).
-export_type([
    begin_quorum_epoch_request_0/0,
    partition_data_0/0,
    topic_data_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(BEGIN_QUORUM_EPOCH_REQUEST, 53).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_begin_quorum_epoch_request_0(begin_quorum_epoch_request_0()) -> iodata().

encode_begin_quorum_epoch_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        cluster_id := ClusterId,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_string(ClusterId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?BEGIN_QUORUM_EPOCH_REQUEST, 0, CorrelationId, ClientId),
        ?encode_nullable_string(ClusterId),
        ?encode_array(Topics, fun encode_topic_data_0/1)
    ];
encode_begin_quorum_epoch_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        cluster_id => nullable_string,
        topics => {array, topic_data_0}
    }).

-spec decode_begin_quorum_epoch_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: begin_quorum_epoch_request_0(),
    Rest :: binary().

decode_begin_quorum_epoch_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_string(ClusterId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_topic_data_0)),
    {
        Header#{
            cluster_id => ClusterId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The ID of the newly elected leader
        leader_id := LeaderId,
        % The epoch of the newly elected leader
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch)
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        leader_id => int32,
        leader_epoch => int32
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(LeaderId, Bin1, Bin2),
    ?_decode_int32(LeaderEpoch, Bin2, Bin3),
    {
        #{
            partition_index => PartitionIndex,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch
        },
        Bin3
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

-type begin_quorum_epoch_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    cluster_id := binary() | null,
    topics := list(topic_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    leader_id := integer(),
    leader_epoch := integer()
}.
-type topic_data_0() :: #{
    topic_name := binary(),
    partitions := list(partition_data_0())
}.
