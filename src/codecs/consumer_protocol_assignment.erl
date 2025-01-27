-module(consumer_protocol_assignment).
-export([
    encode_consumer_protocol_assignment_0/1,
    decode_consumer_protocol_assignment_0/1,
    encode_consumer_protocol_assignment_1/1,
    decode_consumer_protocol_assignment_1/1,
    encode_consumer_protocol_assignment_2/1,
    decode_consumer_protocol_assignment_2/1,
    encode_consumer_protocol_assignment_3/1,
    decode_consumer_protocol_assignment_3/1
]).
-export_type([
    consumer_protocol_assignment_0/0,
    topic_partition_0/0,
    consumer_protocol_assignment_1/0,
    topic_partition_1/0,
    consumer_protocol_assignment_2/0,
    topic_partition_2/0,
    consumer_protocol_assignment_3/0,
    topic_partition_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_consumer_protocol_assignment_0(consumer_protocol_assignment_0()) -> iodata().

encode_consumer_protocol_assignment_0(
    _Args = #{
        assigned_partitions := AssignedPartitions,
        user_data := UserData
    }
) when
    ?is_array(AssignedPartitions),
    ?is_nullable_bytes(UserData)
->
    [
        ?encode_array(AssignedPartitions, fun encode_topic_partition_0/1),
        ?encode_nullable_bytes(UserData)
    ];
encode_consumer_protocol_assignment_0(Args) ->
    ?encoder_error(Args, #{
        assigned_partitions => {array, topic_partition_0},
        user_data => nullable_bytes
    }).

-spec decode_consumer_protocol_assignment_0(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_protocol_assignment_0(),
    Rest :: binary().

decode_consumer_protocol_assignment_0(Bin0) when is_binary(Bin0) ->
    ?_decode_array(AssignedPartitions, Bin0, Bin1, ?_decode_element(decode_topic_partition_0)),
    ?_decode_nullable_bytes(UserData, Bin1, Bin2),
    {
        #{
            assigned_partitions => AssignedPartitions,
            user_data => UserData
        },
        Bin2
    }.

-spec encode_topic_partition_0(topic_partition_0()) -> iodata().

encode_topic_partition_0(
    _Args = #{
        topic := Topic,
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
encode_topic_partition_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_topic_partition_0(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partition_0(),
    Rest :: binary().

decode_topic_partition_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_consumer_protocol_assignment_1(consumer_protocol_assignment_1()) -> iodata().

encode_consumer_protocol_assignment_1(
    _Args = #{
        assigned_partitions := AssignedPartitions,
        user_data := UserData
    }
) when
    ?is_array(AssignedPartitions),
    ?is_nullable_bytes(UserData)
->
    [
        ?encode_array(AssignedPartitions, fun encode_topic_partition_1/1),
        ?encode_nullable_bytes(UserData)
    ];
encode_consumer_protocol_assignment_1(Args) ->
    ?encoder_error(Args, #{
        assigned_partitions => {array, topic_partition_1},
        user_data => nullable_bytes
    }).

-spec decode_consumer_protocol_assignment_1(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_protocol_assignment_1(),
    Rest :: binary().

decode_consumer_protocol_assignment_1(Bin0) when is_binary(Bin0) ->
    ?_decode_array(AssignedPartitions, Bin0, Bin1, ?_decode_element(decode_topic_partition_1)),
    ?_decode_nullable_bytes(UserData, Bin1, Bin2),
    {
        #{
            assigned_partitions => AssignedPartitions,
            user_data => UserData
        },
        Bin2
    }.

-spec encode_topic_partition_1(topic_partition_1()) -> iodata().

encode_topic_partition_1(
    _Args = #{
        topic := Topic,
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
encode_topic_partition_1(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_topic_partition_1(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partition_1(),
    Rest :: binary().

decode_topic_partition_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_consumer_protocol_assignment_2(consumer_protocol_assignment_2()) -> iodata().

encode_consumer_protocol_assignment_2(
    _Args = #{
        assigned_partitions := AssignedPartitions,
        user_data := UserData
    }
) when
    ?is_array(AssignedPartitions),
    ?is_nullable_bytes(UserData)
->
    [
        ?encode_array(AssignedPartitions, fun encode_topic_partition_2/1),
        ?encode_nullable_bytes(UserData)
    ];
encode_consumer_protocol_assignment_2(Args) ->
    ?encoder_error(Args, #{
        assigned_partitions => {array, topic_partition_2},
        user_data => nullable_bytes
    }).

-spec decode_consumer_protocol_assignment_2(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_protocol_assignment_2(),
    Rest :: binary().

decode_consumer_protocol_assignment_2(Bin0) when is_binary(Bin0) ->
    ?_decode_array(AssignedPartitions, Bin0, Bin1, ?_decode_element(decode_topic_partition_2)),
    ?_decode_nullable_bytes(UserData, Bin1, Bin2),
    {
        #{
            assigned_partitions => AssignedPartitions,
            user_data => UserData
        },
        Bin2
    }.

-spec encode_topic_partition_2(topic_partition_2()) -> iodata().

encode_topic_partition_2(
    _Args = #{
        topic := Topic,
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
encode_topic_partition_2(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_topic_partition_2(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partition_2(),
    Rest :: binary().

decode_topic_partition_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_consumer_protocol_assignment_3(consumer_protocol_assignment_3()) -> iodata().

encode_consumer_protocol_assignment_3(
    _Args = #{
        assigned_partitions := AssignedPartitions,
        user_data := UserData
    }
) when
    ?is_array(AssignedPartitions),
    ?is_nullable_bytes(UserData)
->
    [
        ?encode_array(AssignedPartitions, fun encode_topic_partition_3/1),
        ?encode_nullable_bytes(UserData)
    ];
encode_consumer_protocol_assignment_3(Args) ->
    ?encoder_error(Args, #{
        assigned_partitions => {array, topic_partition_3},
        user_data => nullable_bytes
    }).

-spec decode_consumer_protocol_assignment_3(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_protocol_assignment_3(),
    Rest :: binary().

decode_consumer_protocol_assignment_3(Bin0) when is_binary(Bin0) ->
    ?_decode_array(AssignedPartitions, Bin0, Bin1, ?_decode_element(decode_topic_partition_3)),
    ?_decode_nullable_bytes(UserData, Bin1, Bin2),
    {
        #{
            assigned_partitions => AssignedPartitions,
            user_data => UserData
        },
        Bin2
    }.

-spec encode_topic_partition_3(topic_partition_3()) -> iodata().

encode_topic_partition_3(
    _Args = #{
        topic := Topic,
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
encode_topic_partition_3(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_topic_partition_3(binary()) -> {Decoded, Rest} when
    Decoded :: topic_partition_3(),
    Rest :: binary().

decode_topic_partition_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-type consumer_protocol_assignment_0() :: #{
    assigned_partitions := list(topic_partition_0()),
    user_data := kafcod:nullable_bytes()
}.
-type topic_partition_0() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type consumer_protocol_assignment_1() :: #{
    assigned_partitions := list(topic_partition_1()),
    user_data := kafcod:nullable_bytes()
}.
-type topic_partition_1() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type consumer_protocol_assignment_2() :: #{
    assigned_partitions := list(topic_partition_2()),
    user_data := kafcod:nullable_bytes()
}.
-type topic_partition_2() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type consumer_protocol_assignment_3() :: #{
    assigned_partitions := list(topic_partition_3()),
    user_data := kafcod:nullable_bytes()
}.
-type topic_partition_3() :: #{
    topic := binary(),
    partitions := list(integer())
}.
