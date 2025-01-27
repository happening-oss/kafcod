-module(describe_producers_request).
-export([
    encode_describe_producers_request_0/1,
    decode_describe_producers_request_0/1
]).
-export_type([
    describe_producers_request_0/0,
    topic_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_PRODUCERS_REQUEST, 61).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_producers_request_0(describe_producers_request_0()) -> iodata().

encode_describe_producers_request_0(
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
        ?encode_request_header_2(?DESCRIBE_PRODUCERS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_topic_request_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_producers_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, topic_request_0}
    }).

-spec decode_describe_producers_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_producers_request_0(),
    Rest :: binary().

decode_describe_producers_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_topic_request_0)),
    ?decode_tagged_fields(
        fun decode_describe_producers_request_0_tagged_field/3,
        Header#{
            topics => Topics
        },
        Bin1
    ).

-spec decode_describe_producers_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_producers_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_request_0(topic_request_0()) -> iodata().

encode_topic_request_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The indexes of the partitions to list producers for.
        partition_indexes := PartitionIndexes
    }
) when
    ?is_string(Name),
    ?is_array(PartitionIndexes)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(PartitionIndexes, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_request_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_topic_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_request_0(),
    Rest :: binary().

decode_topic_request_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_topic_request_0_tagged_field/3,
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    ).

-spec decode_topic_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_topic_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_producers_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(topic_request_0())
}.
-type topic_request_0() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
