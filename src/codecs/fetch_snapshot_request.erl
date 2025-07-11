-module(fetch_snapshot_request).
-export([
    encode_fetch_snapshot_request_0/1,
    decode_fetch_snapshot_request_0/1
]).
-export_type([
    fetch_snapshot_request_0/0,
    snapshot_id_0/0,
    partition_snapshot_0/0,
    topic_snapshot_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(FETCH_SNAPSHOT_REQUEST, 59).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_fetch_snapshot_request_0(fetch_snapshot_request_0()) -> iodata().

encode_fetch_snapshot_request_0(
    Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower
        replica_id := ReplicaId,
        % The maximum bytes to fetch from all of the snapshots
        max_bytes := MaxBytes,
        % The topics to fetch
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_int32(MaxBytes),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?FETCH_SNAPSHOT_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_int32(MaxBytes),
        ?encode_compact_array(Topics, fun encode_topic_snapshot_0/1),
        ?encode_tagged_fields(
            fun encode_fetch_snapshot_request_0_tagged_field/2,
            Args
        )
    ];
encode_fetch_snapshot_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        max_bytes => int32,
        topics => {array, topic_snapshot_0}
    }).

-spec encode_fetch_snapshot_request_0_tagged_field(
    Key :: atom(), Value :: binary() | null
) -> {non_neg_integer(), iodata()} | ignore.

encode_fetch_snapshot_request_0_tagged_field(_Key = cluster_id, ClusterId) ->
    {0, ?encode_compact_nullable_string(ClusterId)};
encode_fetch_snapshot_request_0_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_fetch_snapshot_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_snapshot_request_0(),
    Rest :: binary().

decode_fetch_snapshot_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_int32(MaxBytes, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_snapshot_0)),
    ?decode_tagged_fields(
        fun decode_fetch_snapshot_request_0_tagged_field/3,
        Header#{
            replica_id => ReplicaId,
            max_bytes => MaxBytes,
            topics => Topics
        },
        Bin3
    ).

-spec decode_fetch_snapshot_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: fetch_snapshot_request_0().

%% ClusterId
%% The clusterId if known, this is used to validate metadata fetches prior to broker registration
decode_fetch_snapshot_request_0_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_compact_nullable_string(ClusterId, Bin0, Bin1),
    <<>> = Bin1,
    Acc#{cluster_id => ClusterId};
decode_fetch_snapshot_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_snapshot_id_0(snapshot_id_0()) -> iodata().

encode_snapshot_id_0(
    _Args = #{
        end_offset := EndOffset,
        epoch := Epoch
    }
) when
    ?is_int64(EndOffset),
    ?is_int32(Epoch)
->
    [
        ?encode_int64(EndOffset),
        ?encode_int32(Epoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_snapshot_id_0(Args) ->
    ?encoder_error(Args, #{
        end_offset => int64,
        epoch => int32
    }).

-spec decode_snapshot_id_0(binary()) -> {Decoded, Rest} when
    Decoded :: snapshot_id_0(),
    Rest :: binary().

decode_snapshot_id_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int64(EndOffset, Bin0, Bin1),
    ?_decode_int32(Epoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_snapshot_id_0_tagged_field/3,
        #{
            end_offset => EndOffset,
            epoch => Epoch
        },
        Bin2
    ).

-spec decode_snapshot_id_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: snapshot_id_0().

decode_snapshot_id_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_snapshot_0(partition_snapshot_0()) -> iodata().

encode_partition_snapshot_0(
    _Args = #{
        % The partition index
        partition := Partition,
        % The current leader epoch of the partition, -1 for unknown leader epoch
        current_leader_epoch := CurrentLeaderEpoch,
        % The snapshot endOffset and epoch to fetch
        snapshot_id := SnapshotId,
        % The byte position within the snapshot to start fetching from
        position := Position
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_entity(SnapshotId),
    ?is_int64(Position)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        encode_snapshot_id_0(SnapshotId),
        ?encode_int64(Position),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_snapshot_0(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        snapshot_id => map,
        position => int64
    }).

-spec decode_partition_snapshot_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_snapshot_0(),
    Rest :: binary().

decode_partition_snapshot_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_entity(SnapshotId, Bin2, Bin3, decode_snapshot_id_0),
    ?_decode_int64(Position, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_partition_snapshot_0_tagged_field/3,
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            snapshot_id => SnapshotId,
            position => Position
        },
        Bin4
    ).

-spec decode_partition_snapshot_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: partition_snapshot_0().

decode_partition_snapshot_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_snapshot_0(topic_snapshot_0()) -> iodata().

encode_topic_snapshot_0(
    _Args = #{
        % The name of the topic to fetch
        name := Name,
        % The partitions to fetch
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_partition_snapshot_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_topic_snapshot_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, partition_snapshot_0}
    }).

-spec decode_topic_snapshot_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_snapshot_0(),
    Rest :: binary().

decode_topic_snapshot_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_partition_snapshot_0)),
    ?decode_tagged_fields(
        fun decode_topic_snapshot_0_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_topic_snapshot_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: topic_snapshot_0().

decode_topic_snapshot_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type fetch_snapshot_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    cluster_id => binary() | null,
    replica_id := integer(),
    max_bytes := integer(),
    topics := list(topic_snapshot_0())
}.
-type snapshot_id_0() :: #{
    end_offset := integer(),
    epoch := integer()
}.
-type partition_snapshot_0() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    snapshot_id := snapshot_id_0(),
    position := integer()
}.
-type topic_snapshot_0() :: #{
    name := binary(),
    partitions := list(partition_snapshot_0())
}.
