-module(produce_many).
-export([main/1]).

-define(CLIENT_ID, list_to_binary(?MODULE_STRING)).

main(Args) ->
    argparse:run(
        Args,
        #{
            arguments => [
                #{
                    name => broker,
                    help => "Bootstrap broker (host[:port])",
                    type => {custom, fun parse_broker/1}
                },
                #{name => topic, help => "Topic to produce to", type => binary},
                #{
                    name => count,
                    type => integer,
                    default => 1,
                    help => "How many messages to produce"
                }
            ],
            handler => fun produce_many/1
        },
        #{progname => ?MODULE}
    ).

-define(DEFAULT_BROKER_PORT, 9092).

parse_broker(Arg) when is_list(Arg) ->
    case string:split(Arg, ":") of
        [Host, Port] ->
            {Host, list_to_integer(Port)};
        [Host] ->
            {Host, ?DEFAULT_BROKER_PORT};
        _ ->
            error(badarg)
    end.

produce_many(#{broker := {BootstrapHost, BootstrapPort}, topic := Topic, count := Count}) ->
    % Generate a bunch of messages, figure out which partitions they go to, and then figure out the leader for those
    % partitions.
    Messages = [
        begin
            Key = base64:encode(crypto:strong_rand_bytes(15)),
            Value = base64:encode(crypto:strong_rand_bytes(180)),
            #{key => Key, value => Value, headers => [{<<"seq">>, integer_to_binary(N)}]}
        end
     || N <- lists:seq(1, Count)
    ],

    % Connect to the bootstrap broker.
    {ok, Bootstrap} = kafka_connection:start_link(BootstrapHost, BootstrapPort, ?CLIENT_ID),

    {ok, #{
        topics := [#{name := Topic, error_code := 0, partitions := Partitions}], brokers := Brokers
    }} = kafka_connection:call(
        Bootstrap,
        fun metadata_request:encode_metadata_request_7/1,
        #{
            allow_auto_topic_creation => false,
            topics => [#{name => Topic}]
        },
        fun metadata_response:decode_metadata_response_7/1
    ),

    MessagesByLeaderAndPartition = group_messages(Messages, Partitions),

    maps:foreach(
        fun(LeaderId, MessagesByPartition) ->
            [{LeaderHost, LeaderPort}] = [
                {Host, Port}
             || #{host := Host, port := Port, node_id := NodeId} <- Brokers, NodeId =:= LeaderId
            ],
            {ok, Leader} = kafka_connection:start_link(LeaderHost, LeaderPort, ?CLIENT_ID),

            {ok, Response} = kafka_connection:call(
                Leader,
                fun produce_request:encode_produce_request_7/1,
                #{
                    transactional_id => null,
                    acks => -1,
                    timeout_ms => 5_000,
                    topic_data => [
                        #{
                            name => Topic,
                            partition_data => maps:fold(
                                fun(PartitionIndex, MessagesForPartition, Acc) ->
                                    [
                                        #{
                                            index => PartitionIndex,
                                            records => prepare_message_set(MessagesForPartition)
                                        }
                                        | Acc
                                    ]
                                end,
                                [],
                                MessagesByPartition
                            )
                        }
                    ]
                },
                fun produce_response:decode_produce_response_7/1
            ),

            % This simply fails if anything goes wrong. This is deliberate, at least for now.
            #{responses := [#{name := Topic, partition_responses := PartitionResponses}]} =
                Response,
            [] = [PR || PR = #{error_code := ErrorCode} <- PartitionResponses, ErrorCode =/= 0]
        end,
        MessagesByLeaderAndPartition
    ),
    ok.

prepare_message_set(Messages) ->
    {Records, OffsetDelta} = prepare_records(Messages),
    Timestamp = os:system_time(millisecond),
    [
        #{
            % We don't know the offset, so it's always zero.
            base_offset => 0,
            % While we _could_ get the partition leader epoch from the metadata, kafire doesn't bother. Is that a problem?
            partition_leader_epoch => 0,
            magic => 2,
            % The CRC is calculated in kafcod_record_batch:encode_record_batch; show it here for clarity.
            crc => -1,
            attributes => 0,
            last_offset_delta => OffsetDelta,
            base_timestamp => Timestamp,
            max_timestamp => Timestamp,
            producer_id => -1,
            producer_epoch => -1,
            base_sequence => -1,
            records => Records
        }
    ].

prepare_records(Messages) ->
    {Records, OffsetDelta} = lists:mapfoldl(
        fun(#{key := Key, value := Value, headers := Headers}, OffsetDelta) ->
            {
                #{
                    attributes => 0,
                    timestamp_delta => 0,
                    offset_delta => OffsetDelta,
                    key => Key,
                    value => Value,
                    headers => Headers
                },
                OffsetDelta + 1
            }
        end,
        0,
        Messages
    ),
    {Records, OffsetDelta - 1}.

partition(Key, Count) when is_binary(Key), is_integer(Count) ->
    <<Hash:128>> = crypto:hash(md5, Key),
    Hash rem Count.

%% Group a set of messages into #{Leader => #{Partition => Messages}}
group_messages(Messages, Partitions) ->
    PartitionCount = length(Partitions),

    % Convert 'Partitions' into #{P => LeaderId}; for large numbers of messages/partitions, this avoids searching the
    % list on each message.
    PartitionLeaders = lists:foldl(
        fun(#{partition_index := PartitionIndex, leader_id := LeaderId, error_code := 0}, Acc) ->
            Acc#{PartitionIndex => LeaderId}
        end,
        #{},
        Partitions
    ),

    % Using lists:foldr/3 preserves the message ordering within each partition.
    MessagesByLeaderAndPartition = lists:foldr(
        fun(Message = #{key := Key}, Acc) ->
            PartitionIndex = partition(Key, PartitionCount),
            LeaderId = maps:get(PartitionIndex, PartitionLeaders),

            case Acc of
                % We already have the leader and the partition; add the message.
                #{LeaderId := #{PartitionIndex := PartitionMessages} = Other} ->
                    Acc#{LeaderId := Other#{PartitionIndex := [Message | PartitionMessages]}};
                % We have the leader, but not the partition; add the partition and the message.
                #{LeaderId := #{} = Other} ->
                    Acc#{LeaderId := Other#{PartitionIndex => [Message]}};
                % We don't have the leader; add everything.
                #{} ->
                    Acc#{LeaderId => #{PartitionIndex => [Message]}}
            end
        end,
        #{},
        Messages
    ),
    MessagesByLeaderAndPartition.
