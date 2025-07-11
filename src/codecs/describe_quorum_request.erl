-module(describe_quorum_request).
-export([
    encode_describe_quorum_request_0/1,
    decode_describe_quorum_request_0/1,
    encode_describe_quorum_request_1/1,
    decode_describe_quorum_request_1/1
]).
-export_type([
    describe_quorum_request_0/0,
    partition_data_0/0,
    topic_data_0/0,
    describe_quorum_request_1/0,
    partition_data_1/0,
    topic_data_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_QUORUM_REQUEST, 55).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_quorum_request_0(describe_quorum_request_0()) -> iodata().

encode_describe_quorum_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?DESCRIBE_QUORUM_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_topic_data_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_quorum_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, topic_data_0}
    }).

-spec decode_describe_quorum_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_quorum_request_0(),
    Rest :: binary().

decode_describe_quorum_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_topic_data_0)),
    ?decode_tagged_fields(
        fun decode_describe_quorum_request_0_tagged_field/3,
        Header#{
            topics => Topics
        },
        Bin1
    ).

-spec decode_describe_quorum_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_quorum_request_0().

decode_describe_quorum_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_0(partition_data_0()) -> iodata().

encode_partition_data_0(
    _Args = #{
        % The partition index.
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

-spec encode_describe_quorum_request_1(describe_quorum_request_1()) -> iodata().

encode_describe_quorum_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?DESCRIBE_QUORUM_REQUEST, 1, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_topic_data_1/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_quorum_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, topic_data_1}
    }).

-spec decode_describe_quorum_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_quorum_request_1(),
    Rest :: binary().

decode_describe_quorum_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_topic_data_1)),
    ?decode_tagged_fields(
        fun decode_describe_quorum_request_1_tagged_field/3,
        Header#{
            topics => Topics
        },
        Bin1
    ).

-spec decode_describe_quorum_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_quorum_request_1().

decode_describe_quorum_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_data_1(partition_data_1()) -> iodata().

encode_partition_data_1(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex
    }
) when
    ?is_int32(PartitionIndex)
->
    [
        ?encode_int32(PartitionIndex),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_data_1(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32
    }).

-spec decode_partition_data_1(binary()) -> {Decoded, Rest} when
    Decoded :: partition_data_1(),
    Rest :: binary().

decode_partition_data_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_partition_data_1_tagged_field/3,
        #{
            partition_index => PartitionIndex
        },
        Bin1
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

-type describe_quorum_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(topic_data_0())
}.
-type partition_data_0() :: #{
    partition_index := integer()
}.
-type topic_data_0() :: #{
    topic_name := binary(),
    partitions := list(partition_data_0())
}.
-type describe_quorum_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(topic_data_1())
}.
-type partition_data_1() :: #{
    partition_index := integer()
}.
-type topic_data_1() :: #{
    topic_name := binary(),
    partitions := list(partition_data_1())
}.
