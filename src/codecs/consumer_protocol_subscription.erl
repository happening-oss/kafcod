-module(consumer_protocol_subscription).
-export([
    encode_consumer_protocol_subscription_0/1,
    decode_consumer_protocol_subscription_0/1,
    encode_consumer_protocol_subscription_1/1,
    decode_consumer_protocol_subscription_1/1,
    encode_consumer_protocol_subscription_2/1,
    decode_consumer_protocol_subscription_2/1,
    encode_consumer_protocol_subscription_3/1,
    decode_consumer_protocol_subscription_3/1
]).
-export_type([
    consumer_protocol_subscription_0/0,
    consumer_protocol_subscription_1/0,
    topic_partition_1/0,
    consumer_protocol_subscription_2/0,
    topic_partition_2/0,
    consumer_protocol_subscription_3/0,
    topic_partition_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_consumer_protocol_subscription_0(consumer_protocol_subscription_0()) -> iodata().

encode_consumer_protocol_subscription_0(
    _Args = #{
        topics := Topics,
        user_data := UserData
    }
) when
    ?is_array(Topics),
    ?is_nullable_bytes(UserData)
->
    [
        ?encode_array(Topics, ?encode_string_),
        ?encode_nullable_bytes(UserData)
    ];
encode_consumer_protocol_subscription_0(Args) ->
    ?encoder_error(Args, #{
        topics => {array, string},
        user_data => nullable_bytes
    }).

-spec decode_consumer_protocol_subscription_0(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_protocol_subscription_0(),
    Rest :: binary().

decode_consumer_protocol_subscription_0(Bin0) when is_binary(Bin0) ->
    ?_decode_array(Topics, Bin0, Bin1, ?decode_string_),
    ?_decode_nullable_bytes(UserData, Bin1, Bin2),
    {
        #{
            topics => Topics,
            user_data => UserData
        },
        Bin2
    }.

-spec encode_consumer_protocol_subscription_1(consumer_protocol_subscription_1()) -> iodata().

encode_consumer_protocol_subscription_1(
    _Args = #{
        topics := Topics,
        user_data := UserData,
        owned_partitions := OwnedPartitions
    }
) when
    ?is_array(Topics),
    ?is_nullable_bytes(UserData),
    ?is_array(OwnedPartitions)
->
    [
        ?encode_array(Topics, ?encode_string_),
        ?encode_nullable_bytes(UserData),
        ?encode_array(OwnedPartitions, fun encode_topic_partition_1/1)
    ];
encode_consumer_protocol_subscription_1(Args) ->
    ?encoder_error(Args, #{
        topics => {array, string},
        user_data => nullable_bytes,
        owned_partitions => {array, topic_partition_1}
    }).

-spec decode_consumer_protocol_subscription_1(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_protocol_subscription_1(),
    Rest :: binary().

decode_consumer_protocol_subscription_1(Bin0) when is_binary(Bin0) ->
    ?_decode_array(Topics, Bin0, Bin1, ?decode_string_),
    ?_decode_nullable_bytes(UserData, Bin1, Bin2),
    ?_decode_array(OwnedPartitions, Bin2, Bin3, ?_decode_element(decode_topic_partition_1)),
    {
        #{
            topics => Topics,
            user_data => UserData,
            owned_partitions => OwnedPartitions
        },
        Bin3
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

-spec encode_consumer_protocol_subscription_2(consumer_protocol_subscription_2()) -> iodata().

encode_consumer_protocol_subscription_2(
    _Args = #{
        topics := Topics,
        user_data := UserData,
        owned_partitions := OwnedPartitions,
        generation_id := GenerationId
    }
) when
    ?is_array(Topics),
    ?is_nullable_bytes(UserData),
    ?is_array(OwnedPartitions),
    ?is_int32(GenerationId)
->
    [
        ?encode_array(Topics, ?encode_string_),
        ?encode_nullable_bytes(UserData),
        ?encode_array(OwnedPartitions, fun encode_topic_partition_2/1),
        ?encode_int32(GenerationId)
    ];
encode_consumer_protocol_subscription_2(Args) ->
    ?encoder_error(Args, #{
        topics => {array, string},
        user_data => nullable_bytes,
        owned_partitions => {array, topic_partition_2},
        generation_id => int32
    }).

-spec decode_consumer_protocol_subscription_2(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_protocol_subscription_2(),
    Rest :: binary().

decode_consumer_protocol_subscription_2(Bin0) when is_binary(Bin0) ->
    ?_decode_array(Topics, Bin0, Bin1, ?decode_string_),
    ?_decode_nullable_bytes(UserData, Bin1, Bin2),
    ?_decode_array(OwnedPartitions, Bin2, Bin3, ?_decode_element(decode_topic_partition_2)),
    ?_decode_int32(GenerationId, Bin3, Bin4),
    {
        #{
            topics => Topics,
            user_data => UserData,
            owned_partitions => OwnedPartitions,
            generation_id => GenerationId
        },
        Bin4
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

-spec encode_consumer_protocol_subscription_3(consumer_protocol_subscription_3()) -> iodata().

encode_consumer_protocol_subscription_3(
    _Args = #{
        topics := Topics,
        user_data := UserData,
        owned_partitions := OwnedPartitions,
        generation_id := GenerationId,
        rack_id := RackId
    }
) when
    ?is_array(Topics),
    ?is_nullable_bytes(UserData),
    ?is_array(OwnedPartitions),
    ?is_int32(GenerationId),
    ?is_nullable_string(RackId)
->
    [
        ?encode_array(Topics, ?encode_string_),
        ?encode_nullable_bytes(UserData),
        ?encode_array(OwnedPartitions, fun encode_topic_partition_3/1),
        ?encode_int32(GenerationId),
        ?encode_nullable_string(RackId)
    ];
encode_consumer_protocol_subscription_3(Args) ->
    ?encoder_error(Args, #{
        topics => {array, string},
        user_data => nullable_bytes,
        owned_partitions => {array, topic_partition_3},
        generation_id => int32,
        rack_id => nullable_string
    }).

-spec decode_consumer_protocol_subscription_3(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_protocol_subscription_3(),
    Rest :: binary().

decode_consumer_protocol_subscription_3(Bin0) when is_binary(Bin0) ->
    ?_decode_array(Topics, Bin0, Bin1, ?decode_string_),
    ?_decode_nullable_bytes(UserData, Bin1, Bin2),
    ?_decode_array(OwnedPartitions, Bin2, Bin3, ?_decode_element(decode_topic_partition_3)),
    ?_decode_int32(GenerationId, Bin3, Bin4),
    ?_decode_nullable_string(RackId, Bin4, Bin5),
    {
        #{
            topics => Topics,
            user_data => UserData,
            owned_partitions => OwnedPartitions,
            generation_id => GenerationId,
            rack_id => RackId
        },
        Bin5
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

-type consumer_protocol_subscription_0() :: #{
    topics := list(binary()),
    user_data := kafcod:nullable_bytes()
}.
-type consumer_protocol_subscription_1() :: #{
    topics := list(binary()),
    user_data := kafcod:nullable_bytes(),
    owned_partitions := list(topic_partition_1())
}.
-type topic_partition_1() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type consumer_protocol_subscription_2() :: #{
    topics := list(binary()),
    user_data := kafcod:nullable_bytes(),
    owned_partitions := list(topic_partition_2()),
    generation_id := integer()
}.
-type topic_partition_2() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type consumer_protocol_subscription_3() :: #{
    topics := list(binary()),
    user_data := kafcod:nullable_bytes(),
    owned_partitions := list(topic_partition_3()),
    generation_id := integer(),
    rack_id := binary() | null
}.
-type topic_partition_3() :: #{
    topic := binary(),
    partitions := list(integer())
}.
