-module(offset_delete_request).
-export([
    encode_offset_delete_request_0/1,
    decode_offset_delete_request_0/1
]).
-export_type([
    offset_delete_request_0/0,
    offset_delete_request_partition_0/0,
    offset_delete_request_topic_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(OFFSET_DELETE_REQUEST, 47).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_offset_delete_request_0(offset_delete_request_0()) -> iodata().

encode_offset_delete_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The unique group identifier.
        group_id := GroupId,
        % The topics to delete offsets for
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_string(GroupId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_DELETE_REQUEST, 0, CorrelationId, ClientId),
        ?encode_string(GroupId),
        ?encode_array(Topics, fun encode_offset_delete_request_topic_0/1)
    ];
encode_offset_delete_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_id => string,
        topics => {array, offset_delete_request_topic_0}
    }).

-spec decode_offset_delete_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_delete_request_0(),
    Rest :: binary().

decode_offset_delete_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_string(GroupId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_delete_request_topic_0)),
    {
        Header#{
            group_id => GroupId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_delete_request_partition_0(offset_delete_request_partition_0()) -> iodata().

encode_offset_delete_request_partition_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex
    }
) when
    ?is_int32(PartitionIndex)
->
    [
        ?encode_int32(PartitionIndex)
    ];
encode_offset_delete_request_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32
    }).

-spec decode_offset_delete_request_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_delete_request_partition_0(),
    Rest :: binary().

decode_offset_delete_request_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    {
        #{
            partition_index => PartitionIndex
        },
        Bin1
    }.

-spec encode_offset_delete_request_topic_0(offset_delete_request_topic_0()) -> iodata().

encode_offset_delete_request_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % Each partition to delete offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, fun encode_offset_delete_request_partition_0/1)
    ];
encode_offset_delete_request_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, offset_delete_request_partition_0}
    }).

-spec decode_offset_delete_request_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_delete_request_topic_0(),
    Rest :: binary().

decode_offset_delete_request_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_delete_request_partition_0)),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-type offset_delete_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_id := binary(),
    topics := list(offset_delete_request_topic_0())
}.
-type offset_delete_request_partition_0() :: #{
    partition_index := integer()
}.
-type offset_delete_request_topic_0() :: #{
    name := binary(),
    partitions := list(offset_delete_request_partition_0())
}.
