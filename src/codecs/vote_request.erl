-module(vote_request).
-export([
    encode_vote_request_0/1,
    decode_vote_request_0/1
]).
-export_type([
    vote_request_0/0,
    partition_data_0/0,
    topic_data_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(VOTE_REQUEST, 52).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_vote_request_0(vote_request_0()) -> iodata().

encode_vote_request_0(
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
        ?encode_request_header_2(?VOTE_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_nullable_string(ClusterId),
        ?encode_compact_array(Topics, fun encode_topic_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_vote_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        cluster_id => nullable_string,
        topics => {array, topic_data_0}
    }).

-spec decode_vote_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: vote_request_0(),
    Rest :: binary().

decode_vote_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_string(ClusterId, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_topic_data_0)),
    ?decode_tagged_fields(
        fun decode_vote_request_0_tagged_field/3,
        Header#{
            cluster_id => ClusterId,
            topics => Topics
        },
        Bin2
    ).

-spec decode_vote_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_vote_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The bumped epoch of the candidate sending the request
        candidate_epoch := CandidateEpoch,
        % The ID of the voter sending the request
        candidate_id := CandidateId,
        % The epoch of the last record written to the metadata log
        last_offset_epoch := LastOffsetEpoch,
        % The offset of the last record written to the metadata log
        last_offset := LastOffset
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int32(CandidateEpoch),
    ?is_int32(CandidateId),
    ?is_int32(LastOffsetEpoch),
    ?is_int64(LastOffset)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int32(CandidateEpoch),
        ?encode_int32(CandidateId),
        ?encode_int32(LastOffsetEpoch),
        ?encode_int64(LastOffset),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        candidate_epoch => int32,
        candidate_id => int32,
        last_offset_epoch => int32,
        last_offset => int64
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int32(CandidateEpoch, Bin1, Bin2),
    ?_decode_int32(CandidateId, Bin2, Bin3),
    ?_decode_int32(LastOffsetEpoch, Bin3, Bin4),
    ?_decode_int64(LastOffset, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_partition_data_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            candidate_epoch => CandidateEpoch,
            candidate_id => CandidateId,
            last_offset_epoch => LastOffsetEpoch,
            last_offset => LastOffset
        },
        Bin5
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
    AccOut :: Acc.

decode_topic_data_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type vote_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    cluster_id := binary() | null,
    topics := list(topic_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    candidate_epoch := integer(),
    candidate_id := integer(),
    last_offset_epoch := integer(),
    last_offset := integer()
}.
-type topic_data_0() :: #{
    topic_name := binary(),
    partitions := list(partition_data_0())
}.
