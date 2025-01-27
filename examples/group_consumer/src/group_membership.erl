-module(group_membership).
-export([start_link/4]).
-behaviour(gen_statem).
-export([
    callback_mode/0,
    init/1,
    handle_event/4,
    terminate/3
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kafcod/include/error_code.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link(ConsumerSup, {BootstrapHost, BootstrapPort}, ClientId, {GroupId, Topics}) when
    is_list(BootstrapHost),
    is_integer(BootstrapPort),
    is_binary(ClientId),
    is_binary(GroupId),
    is_list(Topics)
->
    gen_statem:start_link(
        ?MODULE,
        {ConsumerSup, {BootstrapHost, BootstrapPort}, ClientId, {GroupId, Topics}},
        start_options()
    ).

start_options() -> [].

callback_mode() ->
    [handle_event_function].

-record(state, {
    consumer_sup :: pid(),
    % TODO: type aliases for these.
    broker :: pid() | undefined,
    client_id :: binary(),
    group_id :: binary(),
    member_id :: binary() | undefined,
    generation_id :: integer() | undefined,
    topics :: [binary()]
}).

% Arbitrary number; same as kcat default.
-define(HEARTBEAT_INTERVAL_MS, 3_000).

init({ConsumerSup, {BootstrapHost, BootstrapPort}, ClientId, {GroupId, Topics}}) ->
    process_flag(trap_exit, true),

    % Connect to the bootstrap server in 'init', and then do the rest of the FindCoordinator/JoinGroup/SyncGroup state
    % machine later.
    % This is kind of arbitrary at this point. It might make sense to move things around once we've worked out what
    % our guarantees are.
    % For example: if we lose the coordinator, is that another trip round the state machine back to the bootstrap,
    % or do we just die?
    % Note: In each of the failure cases, there's a difference between "lost the connection", "broker died" and
    % "broker is no longer coordinator". Maybe more.
    {ok, Bootstrap} = kafka_connection:start_link(BootstrapHost, BootstrapPort, ClientId),

    % TODO: Can we find some guidance about how much to put in the state, and how much in the event?
    StateData = #state{
        consumer_sup = ConsumerSup,
        broker = Bootstrap,
        client_id = ClientId,
        group_id = GroupId,
        topics = Topics
    },

    {ok, no_state, StateData, [{next_event, internal, find_coordinator}]}.

handle_event(
    internal,
    find_coordinator,
    _,
    StateData = #state{broker = Bootstrap, client_id = ClientId, group_id = GroupId}
) ->
    % A note on API versions: We need to decide whether we're: (a) using the latest version supported by the broker
    % that we have a codec for, whether we need the extra features or not; or (b) the latest version we _need_.
    % On the one hand, using only what we need means broader broker version support. On the other hand, using the
    % latest available means we're safe from deprecated API versions for longer. Does Kafka deprecate that often?
    case find_coordinator(Bootstrap, GroupId) of
        {ok, #{error_code := ?NONE, host := Host, port := Port, node_id := NodeId}} ->
            ?LOG_DEBUG("Coordinator is node ~B, at ~s:~B", [NodeId, Host, Port]),

            {ok, Coordinator} = kafka_connection:start_link(Host, Port, ClientId),

            unlink(Bootstrap),
            kafka_connection:stop(Bootstrap),

            StateData2 = StateData#state{broker = Coordinator},

            {keep_state, StateData2, [{next_event, internal, join_group}]};
        {ok, #{error_code := ?COORDINATOR_NOT_AVAILABLE}} ->
            % TODO: Do we want any delay/backoff here?
            ?LOG_WARNING("Coordinator not available; retrying"),
            {keep_state_and_data, [{next_event, internal, find_coordinator}]}
    end;
handle_event(
    internal,
    join_group,
    _,
    StateData = #state{
        broker = Coordinator, group_id = GroupId, member_id = undefined, topics = Topics
    }
) when is_pid(Coordinator) ->
    % We've got the coordinator; let's join the group.
    Protocols = [
        #{
            name => range_assignor:name(),
            metadata => consumer_protocol:encode_metadata(range_assignor:metadata(Topics))
        }
    ],

    % Attempt to join w/o member ID. Per KIP-394, it will fail.
    {ok, #{error_code := ?MEMBER_ID_REQUIRED, member_id := MemberId}} = join_group(
        Coordinator, GroupId, <<>>, Protocols
    ),

    % We've got a member ID, try again.
    ?LOG_DEBUG("MemberId = ~p", [MemberId]),
    {keep_state, StateData#state{member_id = MemberId}, [{next_event, internal, join_group}]};
handle_event(
    internal,
    join_group,
    _,
    StateData = #state{
        broker = Coordinator,
        group_id = GroupId,
        member_id = MemberId,
        topics = Topics
    }
) when is_pid(Coordinator), MemberId /= undefined ->
    % We've got the coordinator; let's join the group.
    % TODO: Does 'Protocols' live in the state? At least for DRY reasons (see above).
    % TODO: Other assignors.
    Protocols = [
        #{
            name => range_assignor:name(),
            metadata => consumer_protocol:encode_metadata(range_assignor:metadata(Topics))
        }
    ],

    % TODO: How could this fail? What should we do if it does? I suspect that the most common cause would
    % be ?NOT_COORDINATOR, in which case we should go back to find_coordinator. Does it make sense to be stuck in a loop
    % doing that?

    % TODO: Does that mean we need some retry/delay/timeout logic? I'd prefer not to, or at least to put it somewhere
    % else -- supervisor2/3, maybe.

    % TODO: We need some metrics for how long this takes, since it's a major chunk of the time taken to scale-up/recover.
    % (the coordinator waits to give previous group members a chance to join).
    {ok, #{
        error_code := ?NONE, leader := Leader, members := Members0, generation_id := GenerationId
    }} = join_group(Coordinator, GroupId, MemberId, Protocols),

    AssignedPartitions =
        case Leader of
            MemberId ->
                ?LOG_DEBUG("We are the leader"),

                % This is the only really different bit between leader and followers.
                Assignments = assign_members(Coordinator, Members0),
                ?LOG_DEBUG("Assignments = ~p", [Assignments]),

                % TODO: The code from this point is more or less the same for the leader, non-leaders.
                {ok, #{error_code := ?NONE, assignment := Assignment}} = sync_group(
                    Coordinator, GroupId, GenerationId, MemberId, consumer_protocol:encode_assignments(Assignments)
                ),

                #{assigned_partitions := AssignedPartitions0, user_data := <<>>} = kafcod_consumer_protocol:decode_assignment(
                    Assignment
                ),

                AssignedPartitions0;
            _ ->
                ?LOG_DEBUG("We are a follower"),

                % TODO: The code from this point is more or less the same for the leader, non-leaders.
                % BUG: This can fail with ?REBALANCE_IN_PROGRESS. I'm not sure what causes it. It can be triggered by moving the coordinator.
                % But all of the members see the same error. Is it because the group's in a weird state in the new coordinator? What's the solution? How do we test it?
                % I guess this is where we invent the fake broker.
                % kafire goes back to join_group. If it gets ?COORDINATOR_LOAD_IN_PROGRESS, ?NOT_COORDINATOR or ?UNKNOWN_MEMBER_ID at that point, it goes back to find_coordinator.

                {ok, #{error_code := ?NONE, assignment := Assignment}} = sync_group(
                    Coordinator, GroupId, GenerationId, MemberId, []
                ),

                % I'm NOT gonna deal with the duplication just yet, because I don't feel like encode/decode should leak out
                % of the case clause (in case it becomes a separate function later -- I'd like to keep that concern in
                % there).
                #{assigned_partitions := AssignedPartitions0, user_data := <<>>} = kafcod_consumer_protocol:decode_assignment(
                    Assignment
                ),

                AssignedPartitions0
        end,

    ?LOG_DEBUG("AssignedPartitions = ~p", [AssignedPartitions]),

    % Based on the assigned topics/partitions, group them by leader and figure out the offsets.
    AssignedTopics = [T || #{topic := T} <- AssignedPartitions],
    % TODO: This is _another_ call to the broker for metadata; kafire tries hard to avoid that; we should probably do the same.
    {ok, Metadata} = metadata(Coordinator, AssignedTopics),
    {ok, Offsets} = offset_fetch(Coordinator, GroupId, [
        #{name => Topic, partition_indexes => PartitionIndexes}
     || #{topic := Topic, partitions := PartitionIndexes} <- AssignedPartitions
    ]),

    start_consumers(
        StateData#state.consumer_sup,
        StateData#state.client_id,
        AssignedPartitions,
        Metadata,
        Offsets
    ),

    % TODO: Do we need a separate process for this? Probably not; maybe not. Do we need a supervisor with more intelligence?
    % When the topic consumer dies, or the leader moves, we need to restart it/reassign the partition.
    % Hmmm. We don't want the entire process to die, no? So if the partition moves, we need to tell someone. Maybe we _do_ die?

    {keep_state, StateData#state{generation_id = GenerationId}, [
        {state_timeout, ?HEARTBEAT_INTERVAL_MS, heartbeat}
    ]};
handle_event(
    state_timeout,
    heartbeat,
    _,
    StateData = #state{
        broker = Coordinator,
        group_id = GroupId,
        generation_id = GenerationId,
        member_id = MemberId
    }
) ->
    % TODO: This logging might be better in the actual encoder.
    ?LOG_DEBUG("Sending heartbeat. GroupId = ~p, GenerationId = ~p, MemberId = ~p", [
        GroupId, GenerationId, MemberId
    ]),
    case heartbeat(Coordinator, GroupId, GenerationId, MemberId) of
        {ok, #{error_code := ?NONE}} ->
            {keep_state_and_data, [{state_timeout, ?HEARTBEAT_INTERVAL_MS, heartbeat}]};
        {ok, #{error_code := ?REBALANCE_IN_PROGRESS}} ->
            ?LOG_DEBUG("group rebalance in progress"),
            {keep_state_and_data, [{next_event, internal, join_group}]};
        {ok, #{error_code := ?NOT_COORDINATOR}} ->
            ?LOG_DEBUG("coordinator has moved"),
            % Note that we don't have to clear the member ID; we can reuse it.
            {keep_state, StateData, [{next_event, internal, find_coordinator}]}
    end.

start_consumers(ConsumerSup, ClientId, AssignedPartitions, Metadata, Offsets) ->
    Specs = build_consumer_specs(AssignedPartitions, Metadata, Offsets),
    #{brokers := Brokers0} = Metadata,
    Brokers = lists:foldl(
        fun(Broker = #{node_id := NodeId}, Acc) ->
            Acc#{NodeId => Broker}
        end,
        #{},
        Brokers0
    ),
    maps:foreach(
        fun(BrokerId, TopicPartitionOffsets) ->
            Broker = maps:get(BrokerId, Brokers),
            % TODO: Do I want to convert the map into the Kafka stuff here? It saves doing it every time.
            % BUT: it might be easier to update the offsets after each fetch if we leave it as a map.
            % Keep it as a map for now; the conversion code is the same wherever we do it, so changing our minds later is easy.
            {ok, _} = consumer_sup:start_child(ConsumerSup, ClientId, Broker, TopicPartitionOffsets)
        end,
        Specs
    ).

convert_assigned_partitions(AssignedPartitions0) ->
    lists:foldl(
        fun(#{topic := Topic, partitions := PartitionIndexes}, Acc) ->
            Acc#{Topic => PartitionIndexes}
        end,
        #{},
        AssignedPartitions0
    ).

convert_offsets(Offsets0) ->
    lists:foldl(
        fun(#{name := Topic, partitions := Partitions}, Acc1) ->
            lists:foldl(
                fun(
                    #{
                        partition_index := PartitionIndex,
                        error_code := ?NONE,
                        committed_offset := Offset
                    },
                    Acc2
                ) ->
                    case Acc2 of
                        #{Topic := PartitionOffsets} ->
                            Acc2#{Topic := PartitionOffsets#{PartitionIndex => Offset}};
                        #{} ->
                            Acc2#{Topic => #{PartitionIndex => Offset}}
                    end
                end,
                Acc1,
                Partitions
            )
        end,
        #{},
        Offsets0
    ).

-spec build_consumer_specs(AssigedPartitions :: _, Metadata :: _, Offsets :: _) ->
    [{Broker, TopicPartitionOffsets}]
when
    Broker :: #{host := binary(), port := non_neg_integer()},
    TopicPartitionOffsets :: consumer:topic_partition_offsets().
build_consumer_specs(AssignedPartitions0, Metadata, Offsets0) ->
    % Convert to something more useful.
    AssignedPartitions = convert_assigned_partitions(AssignedPartitions0),
    ?LOG_DEBUG("AssignedPartitions = ~p", [AssignedPartitions]),

    #{error_code := ?NONE, topics := TopicOffsets} = Offsets0,
    Offsets = convert_offsets(TopicOffsets),
    ?LOG_DEBUG("Offsets = ~p", [Offsets]),

    % TODO: If the offset is -1, go and get the current offset. That's what kcat does, anyway.
    % ? kafire might have something different, in terms of earliest, latest, last, etc.

    ?LOG_DEBUG("Metadata = ~p", [Metadata]),
    #{topics := TopicMetadata} = Metadata,

    % Group the topics and partitions by leader. Only include topics and partitions assigned to us. Associate the commit
    % offsets as we go along.
    lists:foldl(
        fun(#{name := Topic, error_code := ?NONE, partitions := Partitions}, Acc1) ->
            case AssignedPartitions of
                #{Topic := AssignedPartitionIndexes} ->
                    PartitionOffsets = maps:get(Topic, Offsets),
                    lists:foldl(
                        fun(
                            #{
                                partition_index := PartitionIndex,
                                error_code := ?NONE,
                                leader_id := LeaderId
                            },
                            Acc2
                        ) ->
                            case lists:member(PartitionIndex, AssignedPartitionIndexes) of
                                true ->
                                    CommittedOffset = maps:get(PartitionIndex, PartitionOffsets),

                                    case Acc2 of
                                        #{LeaderId := #{Topic := PartitionIndexes} = Topics} ->
                                            Acc2#{
                                                LeaderId := Topics#{
                                                    Topic := [
                                                        {PartitionIndex, CommittedOffset}
                                                        | PartitionIndexes
                                                    ]
                                                }
                                            };
                                        #{LeaderId := #{} = Topics} ->
                                            Acc2#{
                                                LeaderId := Topics#{
                                                    Topic => [{PartitionIndex, CommittedOffset}]
                                                }
                                            };
                                        #{} ->
                                            Acc2#{
                                                LeaderId => #{
                                                    Topic => [{PartitionIndex, CommittedOffset}]
                                                }
                                            }
                                    end;
                                false ->
                                    Acc2
                            end
                        end,

                        Acc1,
                        Partitions
                    );
                #{} ->
                    Acc1
            end
        end,
        #{},
        TopicMetadata
    ).

-ifdef(TEST).
% TODO: Move this (and the consumer spec stuff) to a separate module?
build_consumer_specs_test() ->
    % We've been assigned partitions 0-2.
    AssignedPartitions = [
        #{partitions => [0, 1, 2], topic => <<"cars">>}
    ],
    % There are partitions 0-5.
    Metadata = #{
        topics => [
            #{
                name => <<"cars">>,
                error_code => 0,
                partitions => [
                    #{
                        error_code => 0,
                        partition_index => 0,
                        leader_id => 103,
                        leader_epoch => 0
                    },
                    #{
                        error_code => 0,
                        partition_index => 2,
                        leader_id => 102,
                        leader_epoch => 0
                    },
                    #{
                        error_code => 0,
                        partition_index => 5,
                        leader_id => 102,
                        leader_epoch => 0
                    },
                    #{
                        error_code => 0,
                        partition_index => 1,
                        leader_id => 101,
                        leader_epoch => 0
                    },
                    #{
                        error_code => 0,
                        partition_index => 3,
                        leader_id => 103,
                        leader_epoch => 0
                    },
                    #{
                        error_code => 0,
                        partition_index => 4,
                        leader_id => 103,
                        leader_epoch => 0
                    }
                ]
            }
        ],
        throttle_time_ms => 0,
        brokers => [
            #{port => 9092, host => <<"172.17.92.39">>, node_id => 101},
            #{port => 9094, host => <<"172.17.92.39">>, node_id => 103},
            #{port => 9093, host => <<"172.17.92.39">>, node_id => 102}
        ]
    },
    Offsets = #{
        error_code => 0,
        topics => [
            #{
                name => <<"cars">>,
                partitions => [
                    #{
                        error_code => 0,
                        partition_index => 3,
                        committed_offset => -1
                    },
                    #{
                        error_code => 0,
                        partition_index => 2,
                        committed_offset => -1
                    },
                    #{
                        error_code => 0,
                        partition_index => 1,
                        committed_offset => -1
                    },
                    #{
                        error_code => 0,
                        partition_index => 0,
                        committed_offset => -1
                    }
                ]
            }
        ]
    },
    ?assertEqual(moo, build_consumer_specs(AssignedPartitions, Metadata, Offsets)),
    ok.
-endif.

terminate(
    _Reason,
    _,
    _StateData = #state{broker = Coordinator, group_id = GroupId, member_id = MemberId}
) when is_pid(Coordinator), is_binary(MemberId) ->
    % TODO: Do we really want to guard on MemberId, or should we actually start using 'State'?
    ?LOG_DEBUG("Leaving group"),
    leave_group(Coordinator, GroupId, MemberId),
    ok;
terminate(_Reason, _, _StateData) ->
    ok.

assign_members(Coordinator, Members0) ->
    Members = lists:map(
        fun(M = #{metadata := Metadata}) -> M#{metadata := consumer_protocol:decode_metadata(Metadata)} end,
        Members0
    ),
    ?LOG_DEBUG("Members = ~p", [Members]),

    Topics = lists:uniq(
        lists:foldl(
            fun(#{metadata := #{topics := Topics}}, Acc) ->
                Acc ++ Topics
            end,
            [],
            Members
        )
    ),
    ?LOG_DEBUG("Topics = ~p", [Topics]),

    % TODO: What to do if a given topic doesn't exist?

    % Get the topic metadata.
    % TODO: It's entirely possible that this fails somehow. What should we do about it?
    % Note that Kafire caches the information, but the refresh logic leaks into the callers. We should avoid that.
    {ok, #{topics := TopicPartitions0}} = metadata(Coordinator, Topics),

    % TODO: My insistence on matching error_code := ?NONE is safe (reduced data loss), but not particularly resilient.
    % TODO: We need some error-injection to figure out the recovery paths.

    % We want [#{name := topic(), partitions := [non_neg_integer()]}]
    TopicPartitions = lists:map(
        fun(#{name := T, partitions := Ps0, error_code := ?NONE}) ->
            % We don't need to sort the partitions. It makes debugging easier, though.
            Ps = lists:sort(
                lists:map(fun(#{error_code := ?NONE, partition_index := P}) -> P end, Ps0)
            ),
            #{name => T, partitions => Ps}
        end,
        TopicPartitions0
    ),
    ?LOG_DEBUG("TopicPartitions = ~p", [TopicPartitions]),

    % TODO: We've assumed a particular assignor here; we only have one, but we might have more later...
    range_assignor:assign(Members, TopicPartitions).

% TODO: Move these to a separate file?
metadata(Connection, Topics) when is_pid(Connection), is_list(Topics) ->
    MetadataRequest = #{
        topics => [#{name => T} || T <- Topics],
        allow_auto_topic_creation => false,
        include_cluster_authorized_operations => false,
        include_topic_authorized_operations => false
    },
    kafka_connection:call(
        Connection,
        fun metadata_request:encode_metadata_request_9/1,
        MetadataRequest,
        fun metadata_response:decode_metadata_response_9/1
    ).

find_coordinator(Connection, GroupId) when is_pid(Connection), is_binary(GroupId) ->
    FindCoordinatorRequest = #{
        % TODO: macro for key type.
        key_type => 0,
        key => GroupId
    },
    kafka_connection:call(
        Connection,
        fun find_coordinator_request:encode_find_coordinator_request_3/1,
        FindCoordinatorRequest,
        fun find_coordinator_response:decode_find_coordinator_response_3/1
    ).

join_group(Connection, GroupId, MemberId, Protocols) when
    is_pid(Connection), is_binary(GroupId), is_binary(MemberId), is_list(Protocols)
->
    JoinGroupRequest = #{
        session_timeout_ms => 45_000,
        rebalance_timeout_ms => 90_000,
        protocol_type => <<"consumer">>,
        protocols => Protocols,
        member_id => MemberId,
        group_instance_id => null,
        group_id => GroupId
    },
    kafka_connection:call(
        Connection,
        fun join_group_request:encode_join_group_request_7/1,
        JoinGroupRequest,
        fun join_group_response:decode_join_group_response_7/1
    ).

sync_group(Connection, GroupId, GenerationId, MemberId, Assignments) when
    is_pid(Connection),
    is_binary(GroupId),
    is_integer(GenerationId),
    is_binary(MemberId),
    is_list(Assignments)
->
    SyncGroupRequest = #{
        group_id => GroupId,
        generation_id => GenerationId,
        member_id => MemberId,
        group_instance_id => null,
        assignments => Assignments
    },
    kafka_connection:call(
        Connection,
        fun sync_group_request:encode_sync_group_request_3/1,
        SyncGroupRequest,
        fun sync_group_response:decode_sync_group_response_3/1
    ).

leave_group(Connection, GroupId, MemberId) when
    is_pid(Connection),
    is_binary(GroupId),
    is_binary(MemberId)
->
    LeaveGroupRequest = #{
        group_id => GroupId,
        member_id => MemberId
    },
    kafka_connection:call(
        Connection,
        fun leave_group_request:encode_leave_group_request_0/1,
        LeaveGroupRequest,
        fun leave_group_response:decode_leave_group_response_0/1
    ).

heartbeat(Connection, GroupId, GenerationId, MemberId) when
    is_pid(Connection),
    is_binary(GroupId),
    is_integer(GenerationId),
    is_binary(MemberId)
->
    HeartbeatRequest = #{
        group_id => GroupId,
        generation_id => GenerationId,
        member_id => MemberId,
        group_instance_id => null
    },
    kafka_connection:call(
        Connection,
        fun heartbeat_request:encode_heartbeat_request_3/1,
        HeartbeatRequest,
        fun heartbeat_response:decode_heartbeat_response_3/1
    ).

% TODO: spec for the return type.
-spec offset_fetch(
    Connection :: pid(),
    GroupId :: binary(),
    Topics :: [#{name := binary(), partition_indexes := [non_neg_integer()]}]
) -> {ok, map()}.

offset_fetch(Connection, GroupId, Topics) when
    is_pid(Connection), is_binary(GroupId), is_list(Topics)
->
    kafka_connection:call(
        Connection,
        fun offset_fetch_request:encode_offset_fetch_request_4/1,
        #{
            group_id => GroupId,
            topics => Topics
        },
        fun offset_fetch_response:decode_offset_fetch_response_4/1
    ).
