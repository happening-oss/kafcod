-module(assign_replicas_to_dirs_response).
-export([
    encode_assign_replicas_to_dirs_response_0/1,
    decode_assign_replicas_to_dirs_response_0/1
]).
-export_type([
    assign_replicas_to_dirs_response_0/0,
    partition_data_0/0,
    topic_data_0/0,
    directory_data_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_assign_replicas_to_dirs_response_0(assign_replicas_to_dirs_response_0()) -> iodata().

encode_assign_replicas_to_dirs_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code
        error_code := ErrorCode,
        directories := Directories
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Directories)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Directories, fun encode_directory_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_assign_replicas_to_dirs_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        directories => {array, directory_data_0}
    }).

-spec decode_assign_replicas_to_dirs_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: assign_replicas_to_dirs_response_0(),
    Rest :: binary().

decode_assign_replicas_to_dirs_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Directories, Bin2, Bin3, ?_decode_element(decode_directory_data_0)),
    ?decode_tagged_fields(
        fun decode_assign_replicas_to_dirs_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            directories => Directories
        },
        Bin3
    ).

-spec decode_assign_replicas_to_dirs_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_assign_replicas_to_dirs_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex,
        % The partition level error code
        error_code := ErrorCode
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_partition_data_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode
        },
        Bin2
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
        % The ID of the assigned topic
        topic_id := TopicId,
        partitions := Partitions
    }
) when
    ?is_uuid(TopicId),
    ?is_array(Partitions)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_array(Partitions, fun encode_partition_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_data_0(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        partitions => {array, partition_data_0}
    }).

-spec decode_topic_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_data_0(),
    Rest :: binary().

decode_topic_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_data_0)),
    ?decode_tagged_fields(
        fun decode_topic_data_0_tagged_field/3,
        #{
            topic_id => TopicId,
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

-spec encode_directory_data_0(directory_data_0()) -> iodata().

encode_directory_data_0(
    _Args = #{
        % The ID of the directory
        id := Id,
        topics := Topics
    }
) when
    ?is_uuid(Id),
    ?is_array(Topics)
->
    [
        ?encode_uuid(Id),
        ?encode_compact_array(Topics, fun encode_topic_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_directory_data_0(Args) ->
    ?encoder_error(Args, #{
        id => uuid,
        topics => {array, topic_data_0}
    }).

-spec decode_directory_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: directory_data_0(),
    Rest :: binary().

decode_directory_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(Id, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_topic_data_0)),
    ?decode_tagged_fields(
        fun decode_directory_data_0_tagged_field/3,
        #{
            id => Id,
            topics => Topics
        },
        Bin2
    ).

-spec decode_directory_data_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_directory_data_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type assign_replicas_to_dirs_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    directories := list(directory_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer(),
    error_code := integer()
}.
-type topic_data_0() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_0())
}.
-type directory_data_0() :: #{
    id := kafcod:uuid(),
    topics := list(topic_data_0())
}.
