-module(elect_leaders_request).
-export([
    encode_elect_leaders_request_0/1,
    decode_elect_leaders_request_0/1,
    encode_elect_leaders_request_1/1,
    decode_elect_leaders_request_1/1,
    encode_elect_leaders_request_2/1,
    decode_elect_leaders_request_2/1
]).
-export_type([
    elect_leaders_request_0/0,
    topic_partitions_0/0,
    elect_leaders_request_1/0,
    topic_partitions_1/0,
    elect_leaders_request_2/0,
    topic_partitions_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ELECT_LEADERS_REQUEST, 43).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_elect_leaders_request_0(elect_leaders_request_0()) -> iodata().

encode_elect_leaders_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topic partitions to elect leaders.
        topic_partitions := TopicPartitions,
        % The time in ms to wait for the election to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(TopicPartitions),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?ELECT_LEADERS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_nullable_array(TopicPartitions, fun encode_topic_partitions_0/1),
        ?encode_int32(TimeoutMs)
    ];
encode_elect_leaders_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topic_partitions => {nullable_array, topic_partitions_0},
        timeout_ms => int32
    }).

-spec decode_elect_leaders_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: elect_leaders_request_0(),
    Rest :: binary().

decode_elect_leaders_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(TopicPartitions, Bin0, Bin1, ?_decode_element(decode_topic_partitions_0)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    {
        Header#{
            topic_partitions => TopicPartitions,
            timeout_ms => TimeoutMs
        },
        Bin2
    }.

-spec encode_topic_partitions_0(topic_partitions_0()) -> iodata().

encode_topic_partitions_0(
    _Args = #{
        % The name of a topic.
        topic := Topic,
        % The partitions of this topic whose leader should be elected.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_topic_partitions_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_topic_partitions_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partitions_0(),
    Rest :: binary().

decode_topic_partitions_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_elect_leaders_request_1(elect_leaders_request_1()) -> iodata().

encode_elect_leaders_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Type of elections to conduct for the partition. A value of '0' elects the preferred replica. A value of '1' elects the first live replica if there are no in-sync replica.
        election_type := ElectionType,
        % The topic partitions to elect leaders.
        topic_partitions := TopicPartitions,
        % The time in ms to wait for the election to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int8(ElectionType),
    ?is_nullable_array(TopicPartitions),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?ELECT_LEADERS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int8(ElectionType),
        ?encode_nullable_array(TopicPartitions, fun encode_topic_partitions_1/1),
        ?encode_int32(TimeoutMs)
    ];
encode_elect_leaders_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        election_type => int8,
        topic_partitions => {nullable_array, topic_partitions_1},
        timeout_ms => int32
    }).

-spec decode_elect_leaders_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: elect_leaders_request_1(),
    Rest :: binary().

decode_elect_leaders_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int8(ElectionType, Bin0, Bin1),
    ?_decode_nullable_array(TopicPartitions, Bin1, Bin2, ?_decode_element(decode_topic_partitions_1)),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    {
        Header#{
            election_type => ElectionType,
            topic_partitions => TopicPartitions,
            timeout_ms => TimeoutMs
        },
        Bin3
    }.

-spec encode_topic_partitions_1(topic_partitions_1()) -> iodata().

encode_topic_partitions_1(
    _Args = #{
        % The name of a topic.
        topic := Topic,
        % The partitions of this topic whose leader should be elected.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_topic_partitions_1(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_topic_partitions_1(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partitions_1(),
    Rest :: binary().

decode_topic_partitions_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_elect_leaders_request_2(elect_leaders_request_2()) -> iodata().

encode_elect_leaders_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Type of elections to conduct for the partition. A value of '0' elects the preferred replica. A value of '1' elects the first live replica if there are no in-sync replica.
        election_type := ElectionType,
        % The topic partitions to elect leaders.
        topic_partitions := TopicPartitions,
        % The time in ms to wait for the election to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int8(ElectionType),
    ?is_nullable_array(TopicPartitions),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_2(?ELECT_LEADERS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int8(ElectionType),
        ?encode_compact_nullable_array(TopicPartitions, fun encode_topic_partitions_2/1),
        ?encode_int32(TimeoutMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_elect_leaders_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        election_type => int8,
        topic_partitions => {nullable_array, topic_partitions_2},
        timeout_ms => int32
    }).

-spec decode_elect_leaders_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: elect_leaders_request_2(),
    Rest :: binary().

decode_elect_leaders_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int8(ElectionType, Bin0, Bin1),
    ?_decode_compact_nullable_array(TopicPartitions, Bin1, Bin2, ?_decode_element(decode_topic_partitions_2)),
    ?_decode_int32(TimeoutMs, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_elect_leaders_request_2_tagged_field/3,
        Header#{
            election_type => ElectionType,
            topic_partitions => TopicPartitions,
            timeout_ms => TimeoutMs
        },
        Bin3
    ).

-spec decode_elect_leaders_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: elect_leaders_request_2().

decode_elect_leaders_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_partitions_2(topic_partitions_2()) -> iodata().

encode_topic_partitions_2(
    _Args = #{
        % The name of a topic.
        topic := Topic,
        % The partitions of this topic whose leader should be elected.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_partitions_2(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_topic_partitions_2(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partitions_2(),
    Rest :: binary().

decode_topic_partitions_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_topic_partitions_2_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_partitions_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_partitions_2().

decode_topic_partitions_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type elect_leaders_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topic_partitions := list(topic_partitions_0()) | null,
    timeout_ms := integer()
}.
-type topic_partitions_0() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type elect_leaders_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    election_type := integer(),
    topic_partitions := list(topic_partitions_1()) | null,
    timeout_ms := integer()
}.
-type topic_partitions_1() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type elect_leaders_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    election_type := integer(),
    topic_partitions := list(topic_partitions_2()) | null,
    timeout_ms := integer()
}.
-type topic_partitions_2() :: #{
    topic := binary(),
    partitions := list(integer())
}.
