-module(vote_response).
-export([
    encode_vote_response_0/1,
    decode_vote_response_0/1
]).
-export_type([
    vote_response_0/0,
    partition_data_0/0,
    topic_data_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_vote_response_0(vote_response_0()) -> iodata().

encode_vote_response_0(
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
encode_vote_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16,
        topics => {array, topic_data_0}
    }).

-spec decode_vote_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: vote_response_0(),
    Rest :: binary().

decode_vote_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_topic_data_0)),
    ?decode_tagged_fields(
        fun decode_vote_response_0_tagged_field/3,
        Header#{
            error_code => ErrorCode,
            topics => Topics
        },
        Bin2
    ).

-spec decode_vote_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: vote_response_0().

decode_vote_response_0_tagged_field(_Tag, _Bin0, Acc) ->
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
        % True if the vote was granted and false otherwise
        vote_granted := VoteGranted
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch),
    ?is_bool(VoteGranted)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?encode_bool(VoteGranted),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        leader_id => int32,
        leader_epoch => int32,
        vote_granted => bool
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_int32(LeaderId, Bin2, Bin3),
    ?_decode_int32(LeaderEpoch, Bin3, Bin4),
    ?_decode_bool(VoteGranted, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_partition_data_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch,
            vote_granted => VoteGranted
        },
        Bin5
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

-type vote_response_0() :: #{
    correlation_id => integer(),
    error_code := integer(),
    topics := list(topic_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    error_code := integer(),
    leader_id := integer(),
    leader_epoch := integer(),
    vote_granted := boolean()
}.
-type topic_data_0() :: #{
    topic_name := binary(),
    partitions := list(partition_data_0())
}.
