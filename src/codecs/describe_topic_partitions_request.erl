-module(describe_topic_partitions_request).
-export([
    encode_describe_topic_partitions_request_0/1,
    decode_describe_topic_partitions_request_0/1
]).
-export_type([
    describe_topic_partitions_request_0/0,
    topic_request_0/0,
    cursor_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_TOPIC_PARTITIONS_REQUEST, 75).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_topic_partitions_request_0(describe_topic_partitions_request_0()) -> iodata().

encode_describe_topic_partitions_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch details for.
        topics := Topics,
        % The maximum number of partitions included in the response.
        response_partition_limit := ResponsePartitionLimit,
        % The first topic and partition index to fetch details for.
        cursor := Cursor
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(ResponsePartitionLimit),
    ?is_nullable_entity(Cursor)
->
    [
        ?encode_request_header_2(?DESCRIBE_TOPIC_PARTITIONS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_topic_request_0/1),
        ?encode_int32(ResponsePartitionLimit),
        encode_cursor_0(Cursor),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_topic_partitions_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, topic_request_0},
        response_partition_limit => int32,
        cursor => nullable_Cursor
    }).

-spec decode_describe_topic_partitions_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_topic_partitions_request_0(),
    Rest :: binary().

decode_describe_topic_partitions_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_topic_request_0)),
    ?_decode_int32(ResponsePartitionLimit, Bin1, Bin2),
    ?_decode_entity(Cursor, Bin2, Bin3, decode_cursor_0),
    ?decode_tagged_fields(
        fun decode_describe_topic_partitions_request_0_tagged_field/3,
        Header#{
            topics => Topics,
            response_partition_limit => ResponsePartitionLimit,
            cursor => Cursor
        },
        Bin3
    ).

-spec decode_describe_topic_partitions_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_topic_partitions_request_0().

decode_describe_topic_partitions_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_request_0(topic_request_0()) -> iodata().

encode_topic_request_0(
    _Args = #{
        % The topic name
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_compact_string(Name),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_request_0(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_topic_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_request_0(),
    Rest :: binary().

decode_topic_request_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_topic_request_0_tagged_field/3,
        #{
            name => Name
        },
        Bin1
    ).

-spec decode_topic_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_request_0().

decode_topic_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_cursor_0(cursor_0()) -> iodata().

encode_cursor_0(
    _Args = #{
        % The name for the first topic to process
        topic_name := TopicName,
        % The partition index to start with
        partition_index := PartitionIndex
    }
) when
    ?is_string(TopicName),
    ?is_int32(PartitionIndex)
->
    [
        ?encode_compact_string(TopicName),
        ?encode_int32(PartitionIndex),
        ?EMPTY_TAG_BUFFER
    ];
encode_cursor_0(Args) ->
    ?encoder_error(Args, #{
        topic_name => string,
        partition_index => int32
    }).

-spec decode_cursor_0(binary()) -> {Decoded, Rest} when
    Decoded :: cursor_0(),
    Rest :: binary().

decode_cursor_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(TopicName, Bin0, Bin1),
    ?_decode_int32(PartitionIndex, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_cursor_0_tagged_field/3,
        #{
            topic_name => TopicName,
            partition_index => PartitionIndex
        },
        Bin2
    ).

-spec decode_cursor_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: cursor_0().

decode_cursor_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_topic_partitions_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(topic_request_0()),
    response_partition_limit := integer(),
    cursor := cursor_0() | null
}.
-type topic_request_0() :: #{
    name := binary()
}.
-type cursor_0() :: #{
    topic_name := binary(),
    partition_index := integer()
}.
