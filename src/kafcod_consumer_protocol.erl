-module(kafcod_consumer_protocol).

%%% Encoder/decoder for the Kafka client-side assignment protocol.

%%% See the Kafka documentation at
%%% https://cwiki.apache.org/confluence/display/KAFKA/Kafka+Client-side+Assignment+Proposal#KafkaClientsideAssignmentProposal-ConsumerEmbeddedProtocol

%%% In the same way that we have request/response header codecs (which are in kafcod_{request,response}_header.erl)
%%% and the request/response body codecs (which are generated, in the src/codecs directory),
%%% we have this module and src/codecs/consumer_protocol_{assignment,subscription}.erl.

%%% Following the Kafka documentation linked above, MemberMetadata is in consumer_protocol_subscription; MemberState is
%%% in consumer_protocol_assignment.

-export([
    encode_metadata/1,
    encode_metadata/2,
    decode_metadata/1,
    encode_assignments/1,
    decode_assignment/1
]).
% Used in kafine unit tests, for mocking SyncGroup responses.
-export([
    encode_assignment/2
]).

-export_type([assignments/0, member_assignment/0, assigned_partitions/0]).

-type assignments() :: #{
    member_id() => member_assignment()
}.
-type member_id() :: binary().
-type member_assignment() :: #{
    assigned_partitions := assigned_partitions(),
    user_data := kafcod:nullable_bytes()
}.
-type assigned_partitions() :: #{
    Topic :: binary() => [Partition :: non_neg_integer()]
}.

-spec encode_metadata(
    Metadata :: consumer_protocol_subscription:consumer_protocol_subscription_0()
) -> binary().

encode_metadata(Metadata) ->
    encode_metadata(Metadata, 0).

encode_metadata(Metadata, Version) when Version =:= 0 ->
    iolist_to_binary([
        <<Version:16/big-signed>>,
        consumer_protocol_subscription:encode_consumer_protocol_subscription_0(Metadata)
    ]);
encode_metadata(Metadata, Version) when Version =:= 1 ->
    iolist_to_binary([
        <<Version:16/big-signed>>,
        consumer_protocol_subscription:encode_consumer_protocol_subscription_1(Metadata)
    ]);
encode_metadata(Metadata, Version) when Version =:= 2 ->
    iolist_to_binary([
        <<Version:16/big-signed>>,
        consumer_protocol_subscription:encode_consumer_protocol_subscription_2(Metadata)
    ]);
encode_metadata(Metadata, Version) when Version =:= 3 ->
    iolist_to_binary([
        <<Version:16/big-signed>>,
        consumer_protocol_subscription:encode_consumer_protocol_subscription_3(Metadata)
    ]).

-spec decode_metadata(Metadata :: binary()) ->
    consumer_protocol_subscription:consumer_protocol_subscription_0()
    | consumer_protocol_subscription:consumer_protocol_subscription_1()
    | consumer_protocol_subscription:consumer_protocol_subscription_2()
    | consumer_protocol_subscription:consumer_protocol_subscription_3().

decode_metadata(<<Version:16/big-signed, Rest/binary>>) ->
    decode_metadata(Version, Rest).

decode_metadata(_Version = 0, Bin) ->
    % Kafire sends the assignment strategy over the wire (by mistake/misunderstanding) as a part of
    % of its member join group process. Thats why we match _ instead of <<>> here.
    {Metadata, _} = consumer_protocol_subscription:decode_consumer_protocol_subscription_0(Bin),
    Metadata;
decode_metadata(_Version = 1, Bin) ->
    {Metadata, _} = consumer_protocol_subscription:decode_consumer_protocol_subscription_1(Bin),
    Metadata;
decode_metadata(_Version = 2, Bin) ->
    {Metadata, _} = consumer_protocol_subscription:decode_consumer_protocol_subscription_2(Bin),
    Metadata;
decode_metadata(Version, Bin) when Version >= 3 ->
    % kcat uses ConsumerProtocolSubscription v3
    %
    % Per the comments in the ConsumerProtocolSubscription.json file, each version extends the previous one, so Version
    % >= 3 is the correct guard, above, and we should ignore any trailing data.
    {Metadata, _} = consumer_protocol_subscription:decode_consumer_protocol_subscription_3(Bin),
    Metadata.

-spec encode_assignments(
    Assignments :: assignments()
) -> [#{member_id := binary(), assignment := binary()}].

encode_assignments(Assignments) ->
    maps:fold(
        fun(
            MemberId, #{assigned_partitions := AssignedTopicPartitions, user_data := UserData}, Acc
        ) ->
            AssignedPartitions = convert_assignment_to_kafka(AssignedTopicPartitions),
            MemberState = encode_assignment(AssignedPartitions, UserData),
            [#{member_id => MemberId, assignment => MemberState} | Acc]
        end,
        [],
        Assignments
    ).

encode_assignment(AssignedPartitions, UserData) ->
    % Note: the version isn't in 'consumer_protocol_assignment', because that was machine-generated, and the schema
    % definition doesn't include it. It assumes that the caller will wrap the result. So we do.
    Version = 0,
    iolist_to_binary([
        <<Version:16/big-signed>>,
        consumer_protocol_assignment:encode_consumer_protocol_assignment_0(#{
            assigned_partitions => AssignedPartitions, user_data => UserData
        })
    ]).

-spec decode_assignment(binary()) -> member_assignment().

decode_assignment(<<Version:16/big-signed, Rest/binary>>) ->
    convert_assignment_from_kafka(decode_assignment(Version, Rest));
decode_assignment(<<>>) ->
    % If the leader completely omits the member from the assignments, the broker returns an empty binary. Treat that as
    % if we've been assigned nothing.
    #{assigned_partitions => #{}, user_data => <<>>}.

decode_assignment(_Version = 0, Bytes) ->
    {Assignment, <<>>} = consumer_protocol_assignment:decode_consumer_protocol_assignment_0(Bytes),
    Assignment;
decode_assignment(_Version = 1, Bytes) ->
    {Assignment, <<>>} = consumer_protocol_assignment:decode_consumer_protocol_assignment_1(Bytes),
    Assignment;
decode_assignment(_Version = 2, Bytes) ->
    {Assignment, <<>>} = consumer_protocol_assignment:decode_consumer_protocol_assignment_2(Bytes),
    Assignment;
decode_assignment(Version, Bytes) when Version >= 3 ->
    {Assignment, <<>>} = consumer_protocol_assignment:decode_consumer_protocol_assignment_3(Bytes),
    Assignment.

convert_assignment_to_kafka(AssignedTopicPartitions) ->
    % Convert from #{Topic => [Partition]} to Kafka-style #{topic => Topic, partitions => [Partition]}
    maps:fold(
        fun(Topic, Partitions, Acc2) ->
            [#{topic => Topic, partitions => Partitions} | Acc2]
        end,
        [],
        AssignedTopicPartitions
    ).

convert_assignment_from_kafka(#{assigned_partitions := AssignedPartitions0, user_data := UserData}) ->
    % Convert from Kafka-style #{topic => Topic, partitions => [Partition]} to #{Topic => [Partition]}
    AssignedTopicPartitions = lists:foldl(
        fun(#{topic := Topic, partitions := Partitions}, Acc) ->
            Acc#{Topic => Partitions}
        end,
        #{},
        AssignedPartitions0
    ),
    #{assigned_partitions => AssignedTopicPartitions, user_data => UserData}.
