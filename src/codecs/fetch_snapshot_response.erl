-module(fetch_snapshot_response).
-export([
    encode_fetch_snapshot_response_0/1,
    decode_fetch_snapshot_response_0/1
]).
-export_type([
    fetch_snapshot_response_0/0,
    snapshot_id_0/0,
    leader_id_and_epoch_0/0,
    partition_snapshot_0/0,
    topic_snapshot_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_fetch_snapshot_response_0(fetch_snapshot_response_0()) -> iodata().

encode_fetch_snapshot_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The topics to fetch.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(Topics)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(Topics, fun encode_topic_snapshot_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_fetch_snapshot_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        topics => {array, topic_snapshot_0}
    }).

-spec decode_fetch_snapshot_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: fetch_snapshot_response_0(),
    Rest :: binary().

decode_fetch_snapshot_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(Topics, Bin2, Bin3, ?_decode_element(decode_topic_snapshot_0)),
    ?decode_tagged_fields(
        fun decode_fetch_snapshot_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            topics => Topics
        },
        Bin3
    ).

-spec decode_fetch_snapshot_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_fetch_snapshot_response_0_tagged_field(_Tag, _Bin0, Acc) ->
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
    AccOut :: Acc.

decode_snapshot_id_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_leader_id_and_epoch_0(leader_id_and_epoch_0()) -> iodata().

encode_leader_id_and_epoch_0(
    _Args = #{
        % The ID of the current leader or -1 if the leader is unknown.
        leader_id := LeaderId,
        % The latest known leader epoch
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(LeaderId),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(LeaderId),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_id_and_epoch_0(Args) ->
    ?encoder_error(Args, #{
        leader_id => int32,
        leader_epoch => int32
    }).

-spec decode_leader_id_and_epoch_0(binary()) -> {Decoded, Rest} when
    Decoded :: leader_id_and_epoch_0(),
    Rest :: binary().

decode_leader_id_and_epoch_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(LeaderId, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_leader_id_and_epoch_0_tagged_field/3,
        #{
            leader_id => LeaderId,
            leader_epoch => LeaderEpoch
        },
        Bin2
    ).

-spec decode_leader_id_and_epoch_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_leader_id_and_epoch_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_snapshot_0(partition_snapshot_0()) -> iodata().

encode_partition_snapshot_0(
    Args = #{
        % The partition index.
        index := Index,
        % The error code, or 0 if there was no fetch error.
        error_code := ErrorCode,
        % The snapshot endOffset and epoch fetched
        snapshot_id := SnapshotId,
        % The total size of the snapshot.
        size := Size,
        % The starting byte position within the snapshot included in the Bytes field.
        position := Position,
        % Snapshot data in records format which may not be aligned on an offset boundary
        unaligned_records := UnalignedRecords
    }
) when
    ?is_int32(Index),
    ?is_int16(ErrorCode),
    ?is_entity(SnapshotId),
    ?is_int64(Size),
    ?is_int64(Position),
    ?is_records(UnalignedRecords)
->
    [
        ?encode_int32(Index),
        ?encode_int16(ErrorCode),
        encode_snapshot_id_0(SnapshotId),
        ?encode_int64(Size),
        ?encode_int64(Position),
        ?encode_compact_records(UnalignedRecords),
        ?encode_tagged_fields(
            fun encode_partition_snapshot_0_tagged_field/2,
            Args
        )
    ];
encode_partition_snapshot_0(Args) ->
    ?encoder_error(Args, #{
        index => int32,
        error_code => int16,
        snapshot_id => map,
        size => int64,
        position => int64,
        unaligned_records => records
    }).

-spec encode_partition_snapshot_0_tagged_field(Key :: atom(), Value :: term()) -> iodata() | ignore.

encode_partition_snapshot_0_tagged_field(_Key = current_leader, CurrentLeader) ->
    {0, encode_leader_id_and_epoch_0(CurrentLeader)};
encode_partition_snapshot_0_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_partition_snapshot_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_snapshot_0(),
    Rest :: binary().

decode_partition_snapshot_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Index, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_entity(SnapshotId, Bin2, Bin3, decode_snapshot_id_0),
    ?_decode_int64(Size, Bin3, Bin4),
    ?_decode_int64(Position, Bin4, Bin5),
    ?_decode_compact_records(UnalignedRecords, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_partition_snapshot_0_tagged_field/3,
        #{
            index => Index,
            error_code => ErrorCode,
            snapshot_id => SnapshotId,
            size => Size,
            position => Position,
            unaligned_records => UnalignedRecords
        },
        Bin6
    ).

-spec decode_partition_snapshot_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

%% CurrentLeader
decode_partition_snapshot_0_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_entity(CurrentLeader, Bin0, Bin1, decode_leader_id_and_epoch_0),
    <<>> = Bin1,
    Acc#{current_leader => CurrentLeader};
decode_partition_snapshot_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_topic_snapshot_0(topic_snapshot_0()) -> iodata().

encode_topic_snapshot_0(
    _Args = #{
        % The name of the topic to fetch.
        name := Name,
        % The partitions to fetch.
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
    AccOut :: Acc.

decode_topic_snapshot_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type fetch_snapshot_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    topics := list(topic_snapshot_0())
}.
-type snapshot_id_0() :: #{
    end_offset := integer(),
    epoch := integer()
}.
-type leader_id_and_epoch_0() :: #{
    leader_id := integer(),
    leader_epoch := integer()
}.
-type partition_snapshot_0() :: #{
    index := integer(),
    error_code := integer(),
    snapshot_id := snapshot_id_0(),
    current_leader := leader_id_and_epoch_0(),
    size := integer(),
    position := integer(),
    unaligned_records := kafcod_records:records()
}.
-type topic_snapshot_0() :: #{
    name := binary(),
    partitions := list(partition_snapshot_0())
}.
