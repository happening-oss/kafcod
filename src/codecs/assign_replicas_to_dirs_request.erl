-module(assign_replicas_to_dirs_request).
-export([
    encode_assign_replicas_to_dirs_request_0/1,
    decode_assign_replicas_to_dirs_request_0/1
]).
-export_type([
    assign_replicas_to_dirs_request_0/0,
    partition_data_0/0,
    topic_data_0/0,
    directory_data_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ASSIGN_REPLICAS_TO_DIRS_REQUEST, 73).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_assign_replicas_to_dirs_request_0(assign_replicas_to_dirs_request_0()) -> iodata().

encode_assign_replicas_to_dirs_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the requesting broker
        broker_id := BrokerId,
        % The epoch of the requesting broker
        broker_epoch := BrokerEpoch,
        directories := Directories
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch),
    ?is_array(Directories)
->
    [
        ?encode_request_header_2(?ASSIGN_REPLICAS_TO_DIRS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?encode_compact_array(Directories, fun encode_directory_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_assign_replicas_to_dirs_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64,
        directories => {array, directory_data_0}
    }).

-spec decode_assign_replicas_to_dirs_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: assign_replicas_to_dirs_request_0(),
    Rest :: binary().

decode_assign_replicas_to_dirs_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?_decode_compact_array(Directories, Bin2, Bin3, ?_decode_element(decode_directory_data_0)),
    ?decode_tagged_fields(
        fun decode_assign_replicas_to_dirs_request_0_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch,
            directories => Directories
        },
        Bin3
    ).

-spec decode_assign_replicas_to_dirs_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_assign_replicas_to_dirs_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index
        partition_index := PartitionIndex
    }
) when
    ?is_int32(PartitionIndex)
->
    [
        ?encode_int32(PartitionIndex),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32
    }).

-spec decode_partition_data_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_0(),
    Rest :: binary().

decode_partition_data_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_partition_data_0_tagged_field/3,
        #{
            partition_index => PartitionIndex
        },
        Bin1
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

-type assign_replicas_to_dirs_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer(),
    directories := list(directory_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer()
}.
-type topic_data_0() :: #{
    topic_id := kafcod:uuid(),
    partitions := list(partition_data_0())
}.
-type directory_data_0() :: #{
    id := kafcod:uuid(),
    topics := list(topic_data_0())
}.
