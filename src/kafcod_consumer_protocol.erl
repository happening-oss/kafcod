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
    decode_metadata/1,
    encode_assignments/1,
    decode_assignment/1
]).
% Used in kafine unit tests, for mocking SyncGroup responses.
-export([
    encode_assignment/2
]).

-export_type([assignments/0, member_assignment/0, assigned_partitions/0]).

% TODO: We need some more types, for topic(), etc.
-type metadata() :: #{
    topics := [binary()],
    user_data := binary() | null
}.

-type assignments() :: #{
    member_id() := member_assignment()
}.
-type member_id() :: binary().
-type member_assignment() :: #{
    assigned_partitions := assigned_partitions(),
    user_data := binary()
}.
-type assigned_partitions() :: #{
    Topic :: binary() := [Partition :: non_neg_integer()]
}.

-spec encode_metadata(
    Metadata :: metadata()
) -> binary().

encode_metadata(Metadata) ->
    Version = 0,
    iolist_to_binary([
        <<Version:16/big-signed>>,
        consumer_protocol_subscription:encode_consumer_protocol_subscription_0(Metadata)
    ]).

-spec decode_metadata(Metadata :: binary()) -> metadata().

decode_metadata(<<Version:16/big-signed, Rest/binary>>) when Version =:= 0 ->
    % Kafire sends the assignment strategy over the wire (by mistake/misunderstanding) as a part of
    % of its member join group process. Thats why we match _ instead of <<>> here.
    {Metadata, _} = consumer_protocol_subscription:decode_consumer_protocol_subscription_0(Rest),
    Metadata;
decode_metadata(<<Version:16/big-signed, Rest/binary>>) when Version =:= 3 ->
    % kcat uses ConsumerProtocolSubscription v3
    {Metadata, <<>>} = consumer_protocol_subscription:decode_consumer_protocol_subscription_3(Rest),
    Metadata.

-spec encode_assignments(
    Assignments :: assignments()
) -> [#{member_id := binary(), assignment := binary()}].

encode_assignments(Assignments) ->
    maps:fold(
        fun(
            MemberId, #{assigned_partitions := AssignedTopicPartitions, user_data := UserData}, Acc
        ) ->
            % Convert from #{Topic => [Partition]} to Kafka-style #{topic => Topic, partitions => [Partition]}
            AssignedPartitions = maps:fold(
                fun(Topic, Partitions, Acc2) ->
                    [#{topic => Topic, partitions => Partitions} | Acc2]
                end,
                [],
                AssignedTopicPartitions
            ),

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

decode_assignment(<<Version:16/big-signed, Rest/binary>>) when Version =:= 0 ->
    {Assignment0, <<>>} = consumer_protocol_assignment:decode_consumer_protocol_assignment_0(Rest),
    #{assigned_partitions := AssignedPartitions0, user_data := UserData} = Assignment0,
    % Convert from Kafka-style #{topic => Topic, partitions => [Partition]} to #{Topic => [Partition]}
    AssignedTopicPartitions = lists:foldl(
        fun(#{topic := Topic, partitions := Partitions}, Acc) ->
            Acc#{Topic => Partitions}
        end,
        #{},
        AssignedPartitions0
    ),
    #{assigned_partitions => AssignedTopicPartitions, user_data => UserData}.
