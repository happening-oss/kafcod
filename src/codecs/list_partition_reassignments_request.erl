-module(list_partition_reassignments_request).
-export([
    encode_list_partition_reassignments_request_0/1,
    decode_list_partition_reassignments_request_0/1
]).
-export_type([
    list_partition_reassignments_request_0/0,
    list_partition_reassignments_topics_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(LIST_PARTITION_REASSIGNMENTS_REQUEST, 46).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_list_partition_reassignments_request_0(list_partition_reassignments_request_0()) -> iodata().

encode_list_partition_reassignments_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The time in ms to wait for the request to complete.
        timeout_ms := TimeoutMs,
        % The topics to list partition reassignments for, or null to list everything.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(TimeoutMs),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_2(?LIST_PARTITION_REASSIGNMENTS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(TimeoutMs),
        ?encode_compact_nullable_array(Topics, fun encode_list_partition_reassignments_topics_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_list_partition_reassignments_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        timeout_ms => int32,
        topics => {nullable_array, list_partition_reassignments_topics_0}
    }).

-spec decode_list_partition_reassignments_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_partition_reassignments_request_0(),
    Rest :: binary().

decode_list_partition_reassignments_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(TimeoutMs, Bin0, Bin1),
    ?_decode_compact_nullable_array(Topics, Bin1, Bin2, ?_decode_element(decode_list_partition_reassignments_topics_0)),
    ?decode_tagged_fields(
        fun decode_list_partition_reassignments_request_0_tagged_field/3,
        Header#{
            timeout_ms => TimeoutMs,
            topics => Topics
        },
        Bin2
    ).

-spec decode_list_partition_reassignments_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_partition_reassignments_request_0().

decode_list_partition_reassignments_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_list_partition_reassignments_topics_0(list_partition_reassignments_topics_0()) -> iodata().

encode_list_partition_reassignments_topics_0(
    _Args = #{
        % The topic name
        name := Name,
        % The partitions to list partition reassignments for.
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
encode_list_partition_reassignments_topics_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partition_indexes => {array, int32}
    }).

-spec decode_list_partition_reassignments_topics_0(binary()) -> {Decoded, Rest} when
    Decoded :: list_partition_reassignments_topics_0(),
    Rest :: binary().

decode_list_partition_reassignments_topics_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(PartitionIndexes, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_list_partition_reassignments_topics_0_tagged_field/3,
        #{
            name => Name,
            partition_indexes => PartitionIndexes
        },
        Bin2
    ).

-spec decode_list_partition_reassignments_topics_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: list_partition_reassignments_topics_0().

decode_list_partition_reassignments_topics_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type list_partition_reassignments_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    timeout_ms := integer(),
    topics := list(list_partition_reassignments_topics_0()) | null
}.
-type list_partition_reassignments_topics_0() :: #{
    name := binary(),
    partition_indexes := list(integer())
}.
