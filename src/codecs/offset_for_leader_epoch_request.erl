-module(offset_for_leader_epoch_request).
-export([
    encode_offset_for_leader_epoch_request_0/1,
    decode_offset_for_leader_epoch_request_0/1,
    encode_offset_for_leader_epoch_request_1/1,
    decode_offset_for_leader_epoch_request_1/1,
    encode_offset_for_leader_epoch_request_2/1,
    decode_offset_for_leader_epoch_request_2/1,
    encode_offset_for_leader_epoch_request_3/1,
    decode_offset_for_leader_epoch_request_3/1,
    encode_offset_for_leader_epoch_request_4/1,
    decode_offset_for_leader_epoch_request_4/1
]).
-export_type([
    offset_for_leader_epoch_request_0/0,
    offset_for_leader_partition_0/0,
    offset_for_leader_topic_0/0,
    offset_for_leader_epoch_request_1/0,
    offset_for_leader_partition_1/0,
    offset_for_leader_topic_1/0,
    offset_for_leader_epoch_request_2/0,
    offset_for_leader_partition_2/0,
    offset_for_leader_topic_2/0,
    offset_for_leader_epoch_request_3/0,
    offset_for_leader_partition_3/0,
    offset_for_leader_topic_3/0,
    offset_for_leader_epoch_request_4/0,
    offset_for_leader_partition_4/0,
    offset_for_leader_topic_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(OFFSET_FOR_LEADER_EPOCH_REQUEST, 23).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_offset_for_leader_epoch_request_0(offset_for_leader_epoch_request_0()) -> iodata().

encode_offset_for_leader_epoch_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic to get offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FOR_LEADER_EPOCH_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_offset_for_leader_topic_0/1)
    ];
encode_offset_for_leader_epoch_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, offset_for_leader_topic_0}
    }).

-spec decode_offset_for_leader_epoch_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_request_0(),
    Rest :: binary().

decode_offset_for_leader_epoch_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_offset_for_leader_topic_0)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_offset_for_leader_partition_0(offset_for_leader_partition_0()) -> iodata().

encode_offset_for_leader_partition_0(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The epoch to look up an offset for.
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(Partition),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(LeaderEpoch)
    ];
encode_offset_for_leader_partition_0(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        leader_epoch => int32
    }).

-spec decode_offset_for_leader_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_partition_0(),
    Rest :: binary().

decode_offset_for_leader_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    {
        #{
            partition => Partition,
            leader_epoch => LeaderEpoch
        },
        Bin2
    }.

-spec encode_offset_for_leader_topic_0(offset_for_leader_topic_0()) -> iodata().

encode_offset_for_leader_topic_0(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition to get offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_offset_for_leader_partition_0/1)
    ];
encode_offset_for_leader_topic_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, offset_for_leader_partition_0}
    }).

-spec decode_offset_for_leader_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_0(),
    Rest :: binary().

decode_offset_for_leader_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_partition_0)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_for_leader_epoch_request_1(offset_for_leader_epoch_request_1()) -> iodata().

encode_offset_for_leader_epoch_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic to get offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FOR_LEADER_EPOCH_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_offset_for_leader_topic_1/1)
    ];
encode_offset_for_leader_epoch_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, offset_for_leader_topic_1}
    }).

-spec decode_offset_for_leader_epoch_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_request_1(),
    Rest :: binary().

decode_offset_for_leader_epoch_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_offset_for_leader_topic_1)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_offset_for_leader_partition_1(offset_for_leader_partition_1()) -> iodata().

encode_offset_for_leader_partition_1(
    _Args = #{
        % The partition index.
        partition := Partition,
        % The epoch to look up an offset for.
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(Partition),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(LeaderEpoch)
    ];
encode_offset_for_leader_partition_1(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        leader_epoch => int32
    }).

-spec decode_offset_for_leader_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_partition_1(),
    Rest :: binary().

decode_offset_for_leader_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(LeaderEpoch, Bin1, Bin2),
    {
        #{
            partition => Partition,
            leader_epoch => LeaderEpoch
        },
        Bin2
    }.

-spec encode_offset_for_leader_topic_1(offset_for_leader_topic_1()) -> iodata().

encode_offset_for_leader_topic_1(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition to get offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_offset_for_leader_partition_1/1)
    ];
encode_offset_for_leader_topic_1(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, offset_for_leader_partition_1}
    }).

-spec decode_offset_for_leader_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_1(),
    Rest :: binary().

decode_offset_for_leader_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_partition_1)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_for_leader_epoch_request_2(offset_for_leader_epoch_request_2()) -> iodata().

encode_offset_for_leader_epoch_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic to get offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FOR_LEADER_EPOCH_REQUEST, 2, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_offset_for_leader_topic_2/1)
    ];
encode_offset_for_leader_epoch_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, offset_for_leader_topic_2}
    }).

-spec decode_offset_for_leader_epoch_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_request_2(),
    Rest :: binary().

decode_offset_for_leader_epoch_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_offset_for_leader_topic_2)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_offset_for_leader_partition_2(offset_for_leader_partition_2()) -> iodata().

encode_offset_for_leader_partition_2(
    _Args = #{
        % The partition index.
        partition := Partition,
        % An epoch used to fence consumers/replicas with old metadata. If the epoch provided by the client is larger than the current epoch known to the broker, then the UNKNOWN_LEADER_EPOCH error code will be returned. If the provided epoch is smaller, then the FENCED_LEADER_EPOCH error code will be returned.
        current_leader_epoch := CurrentLeaderEpoch,
        % The epoch to look up an offset for.
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int32(LeaderEpoch)
    ];
encode_offset_for_leader_partition_2(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        leader_epoch => int32
    }).

-spec decode_offset_for_leader_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_partition_2(),
    Rest :: binary().

decode_offset_for_leader_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int32(LeaderEpoch, Bin2, Bin3),
    {
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            leader_epoch => LeaderEpoch
        },
        Bin3
    }.

-spec encode_offset_for_leader_topic_2(offset_for_leader_topic_2()) -> iodata().

encode_offset_for_leader_topic_2(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition to get offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_offset_for_leader_partition_2/1)
    ];
encode_offset_for_leader_topic_2(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, offset_for_leader_partition_2}
    }).

-spec decode_offset_for_leader_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_2(),
    Rest :: binary().

decode_offset_for_leader_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_partition_2)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_for_leader_epoch_request_3(offset_for_leader_epoch_request_3()) -> iodata().

encode_offset_for_leader_epoch_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % Each topic to get offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?OFFSET_FOR_LEADER_EPOCH_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_array(Topics, fun encode_offset_for_leader_topic_3/1)
    ];
encode_offset_for_leader_epoch_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        topics => {array, offset_for_leader_topic_3}
    }).

-spec decode_offset_for_leader_epoch_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_request_3(),
    Rest :: binary().

decode_offset_for_leader_epoch_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_topic_3)),
    {
        Header#{
            replica_id => ReplicaId,
            topics => Topics
        },
        Bin2
    }.

-spec encode_offset_for_leader_partition_3(offset_for_leader_partition_3()) -> iodata().

encode_offset_for_leader_partition_3(
    _Args = #{
        % The partition index.
        partition := Partition,
        % An epoch used to fence consumers/replicas with old metadata. If the epoch provided by the client is larger than the current epoch known to the broker, then the UNKNOWN_LEADER_EPOCH error code will be returned. If the provided epoch is smaller, then the FENCED_LEADER_EPOCH error code will be returned.
        current_leader_epoch := CurrentLeaderEpoch,
        % The epoch to look up an offset for.
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int32(LeaderEpoch)
    ];
encode_offset_for_leader_partition_3(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        leader_epoch => int32
    }).

-spec decode_offset_for_leader_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_partition_3(),
    Rest :: binary().

decode_offset_for_leader_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int32(LeaderEpoch, Bin2, Bin3),
    {
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            leader_epoch => LeaderEpoch
        },
        Bin3
    }.

-spec encode_offset_for_leader_topic_3(offset_for_leader_topic_3()) -> iodata().

encode_offset_for_leader_topic_3(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition to get offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, fun encode_offset_for_leader_partition_3/1)
    ];
encode_offset_for_leader_topic_3(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, offset_for_leader_partition_3}
    }).

-spec decode_offset_for_leader_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_3(),
    Rest :: binary().

decode_offset_for_leader_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_partition_3)),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_offset_for_leader_epoch_request_4(offset_for_leader_epoch_request_4()) -> iodata().

encode_offset_for_leader_epoch_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID of the follower, of -1 if this request is from a consumer.
        replica_id := ReplicaId,
        % Each topic to get offsets for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ReplicaId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_2(?OFFSET_FOR_LEADER_EPOCH_REQUEST, 4, CorrelationId, ClientId),
        ?encode_int32(ReplicaId),
        ?encode_compact_array(Topics, fun encode_offset_for_leader_topic_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_for_leader_epoch_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        replica_id => int32,
        topics => {array, offset_for_leader_topic_4}
    }).

-spec decode_offset_for_leader_epoch_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_epoch_request_4(),
    Rest :: binary().

decode_offset_for_leader_epoch_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ReplicaId, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_topic_4)),
    ?decode_tagged_fields(
        fun decode_offset_for_leader_epoch_request_4_tagged_field/3,
        Header#{
            replica_id => ReplicaId,
            topics => Topics
        },
        Bin2
    ).

-spec decode_offset_for_leader_epoch_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_for_leader_epoch_request_4().

decode_offset_for_leader_epoch_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_for_leader_partition_4(offset_for_leader_partition_4()) -> iodata().

encode_offset_for_leader_partition_4(
    _Args = #{
        % The partition index.
        partition := Partition,
        % An epoch used to fence consumers/replicas with old metadata. If the epoch provided by the client is larger than the current epoch known to the broker, then the UNKNOWN_LEADER_EPOCH error code will be returned. If the provided epoch is smaller, then the FENCED_LEADER_EPOCH error code will be returned.
        current_leader_epoch := CurrentLeaderEpoch,
        % The epoch to look up an offset for.
        leader_epoch := LeaderEpoch
    }
) when
    ?is_int32(Partition),
    ?is_int32(CurrentLeaderEpoch),
    ?is_int32(LeaderEpoch)
->
    [
        ?encode_int32(Partition),
        ?encode_int32(CurrentLeaderEpoch),
        ?encode_int32(LeaderEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_for_leader_partition_4(Args) ->
    ?encoder_error(Args, #{
        partition => int32,
        current_leader_epoch => int32,
        leader_epoch => int32
    }).

-spec decode_offset_for_leader_partition_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_partition_4(),
    Rest :: binary().

decode_offset_for_leader_partition_4(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(Partition, Bin0, Bin1),
    ?_decode_int32(CurrentLeaderEpoch, Bin1, Bin2),
    ?_decode_int32(LeaderEpoch, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_offset_for_leader_partition_4_tagged_field/3,
        #{
            partition => Partition,
            current_leader_epoch => CurrentLeaderEpoch,
            leader_epoch => LeaderEpoch
        },
        Bin3
    ).

-spec decode_offset_for_leader_partition_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_for_leader_partition_4().

decode_offset_for_leader_partition_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_offset_for_leader_topic_4(offset_for_leader_topic_4()) -> iodata().

encode_offset_for_leader_topic_4(
    _Args = #{
        % The topic name.
        topic := Topic,
        % Each partition to get offsets for.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, fun encode_offset_for_leader_partition_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_offset_for_leader_topic_4(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, offset_for_leader_partition_4}
    }).

-spec decode_offset_for_leader_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: offset_for_leader_topic_4(),
    Rest :: binary().

decode_offset_for_leader_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_offset_for_leader_partition_4)),
    ?decode_tagged_fields(
        fun decode_offset_for_leader_topic_4_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_offset_for_leader_topic_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: offset_for_leader_topic_4().

decode_offset_for_leader_topic_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type offset_for_leader_epoch_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(offset_for_leader_topic_0())
}.
-type offset_for_leader_partition_0() :: #{
    partition := integer(),
    leader_epoch := integer()
}.
-type offset_for_leader_topic_0() :: #{
    topic := binary(),
    partitions := list(offset_for_leader_partition_0())
}.
-type offset_for_leader_epoch_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(offset_for_leader_topic_1())
}.
-type offset_for_leader_partition_1() :: #{
    partition := integer(),
    leader_epoch := integer()
}.
-type offset_for_leader_topic_1() :: #{
    topic := binary(),
    partitions := list(offset_for_leader_partition_1())
}.
-type offset_for_leader_epoch_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(offset_for_leader_topic_2())
}.
-type offset_for_leader_partition_2() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    leader_epoch := integer()
}.
-type offset_for_leader_topic_2() :: #{
    topic := binary(),
    partitions := list(offset_for_leader_partition_2())
}.
-type offset_for_leader_epoch_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    topics := list(offset_for_leader_topic_3())
}.
-type offset_for_leader_partition_3() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    leader_epoch := integer()
}.
-type offset_for_leader_topic_3() :: #{
    topic := binary(),
    partitions := list(offset_for_leader_partition_3())
}.
-type offset_for_leader_epoch_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    replica_id := integer(),
    topics := list(offset_for_leader_topic_4())
}.
-type offset_for_leader_partition_4() :: #{
    partition := integer(),
    current_leader_epoch := integer(),
    leader_epoch := integer()
}.
-type offset_for_leader_topic_4() :: #{
    topic := binary(),
    partitions := list(offset_for_leader_partition_4())
}.
