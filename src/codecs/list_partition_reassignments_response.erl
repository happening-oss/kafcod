-module(list_partition_reassignments_response).
-export([
    encode_list_partition_reassignments_response_0/1,
    decode_list_partition_reassignments_response_0/1
]).
-export_type([
    list_partition_reassignments_response_0/0,
    ongoing_partition_reassignment_0/0,
    ongoing_topic_reassignment_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_partition_reassignments_response_0(list_partition_reassignments_response_0()) -> iodata().

encode_list_partition_reassignments_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top-level error code, or 0 if there was no error
        error_code := ErrorCode,
        % The top-level error message, or null if there was no error.
        error_message := ErrorMessage,
        % The ongoing reassignments for each topic.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(Topics, fun encode_ongoing_topic_reassignment_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_partition_reassignments_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        topics => {array, ongoing_topic_reassignment_0}
    }).

-spec decode_list_partition_reassignments_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_partition_reassignments_response_0(),
    Rest :: binary().

decode_list_partition_reassignments_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(Topics, Bin3, Bin4, ?_decode_element(decode_ongoing_topic_reassignment_0)),
    ?decode_tagged_fields(
        fun decode_list_partition_reassignments_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            topics => Topics
        },
        Bin4
    ).

-spec decode_list_partition_reassignments_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_list_partition_reassignments_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_ongoing_partition_reassignment_0(ongoing_partition_reassignment_0()) -> iodata().

encode_ongoing_partition_reassignment_0(
    _Args = #{
        % The index of the partition.
        partition_index := PartitionIndex,
        % The current replica set.
        replicas := Replicas,
        % The set of replicas we are currently adding.
        adding_replicas := AddingReplicas,
        % The set of replicas we are currently removing.
        removing_replicas := RemovingReplicas
    }
) when
    ?is_int32(PartitionIndex),
    ?is_array(Replicas),
    ?is_array(AddingReplicas),
    ?is_array(RemovingReplicas)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_compact_array(Replicas, ?encode_int32_),
        ?encode_compact_array(AddingReplicas, ?encode_int32_),
        ?encode_compact_array(RemovingReplicas, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_ongoing_partition_reassignment_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        replicas => {array, int32},
        adding_replicas => {array, int32},
        removing_replicas => {array, int32}
    }).

-spec decode_ongoing_partition_reassignment_0(binary()) -> {Decoded, Rest} when
    Decoded :: ongoing_partition_reassignment_0(),
    Rest :: binary().

decode_ongoing_partition_reassignment_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_compact_array(Replicas, Bin1, Bin2, ?decode_int32_),
    ?_decode_compact_array(AddingReplicas, Bin2, Bin3, ?decode_int32_),
    ?_decode_compact_array(RemovingReplicas, Bin3, Bin4, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_ongoing_partition_reassignment_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            replicas => Replicas,
            adding_replicas => AddingReplicas,
            removing_replicas => RemovingReplicas
        },
        Bin4
    ).

-spec decode_ongoing_partition_reassignment_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_ongoing_partition_reassignment_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_ongoing_topic_reassignment_0(ongoing_topic_reassignment_0()) -> iodata().

encode_ongoing_topic_reassignment_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The ongoing reassignments for each partition.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_ongoing_partition_reassignment_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_ongoing_topic_reassignment_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, ongoing_partition_reassignment_0}
    }).

-spec decode_ongoing_topic_reassignment_0(binary()) -> {Decoded, Rest} when
    Decoded :: ongoing_topic_reassignment_0(),
    Rest :: binary().

decode_ongoing_topic_reassignment_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_ongoing_partition_reassignment_0)),
    ?decode_tagged_fields(
        fun decode_ongoing_topic_reassignment_0_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_ongoing_topic_reassignment_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_ongoing_topic_reassignment_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_partition_reassignments_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    topics := list(ongoing_topic_reassignment_0())
}.
-type ongoing_partition_reassignment_0() :: #{
    partition_index := integer(),
    replicas := list(integer()),
    adding_replicas := list(integer()),
    removing_replicas := list(integer())
}.
-type ongoing_topic_reassignment_0() :: #{
    name := binary(),
    partitions := list(ongoing_partition_reassignment_0())
}.
