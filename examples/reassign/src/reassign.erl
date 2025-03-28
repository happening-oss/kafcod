-module(reassign).
-export([main/1]).
-export([
    reassign_partitions/1,
    list_partition_reassignments/1,
    elect_leaders/1,
    validate_replication/1
]).

-include_lib("kafcod/include/error_code.hrl").

main(Args) ->
    argparse:run(Args, reassign_cli:cli(), #{progname => ?MODULE}).

reassign_partitions(
    Args = #{
        bootstrap := Bootstrap,
        topic := Topic,
        to := ToNodes,
        pick := Pick,
        shuffle := Shuffle,
        keep_leader := KeepLeader,
        rack_aware := RackAware
    }
) ->
    % Connect to the bootstrap broker.
    {ok, Bo} = kafcod_connection:start_link(Bootstrap),
    Controller = find_controller(Bo),
    kafcod_connection:stop(Bo),

    {ok, Co} = kafcod_connection:start_link(Controller),
    {ok, #{brokers := BrokersMetadata, topics := TopicsMetadata}} = kafcod_connection:call(
        Co,
        fun metadata_request:encode_metadata_request_7/1,
        #{allow_auto_topic_creation => false, topics => [#{name => Topic}]},
        fun metadata_response:decode_metadata_response_7/1
    ),

    [#{name := Topic, error_code := ?NONE, partitions := PartitionsMetadata}] = TopicsMetadata,

    % undefined means all partitions.
    PartitionsToMove = maps:get(partition, Args, undefined),

    CandidateNodes = get_candidate_nodes(BrokersMetadata, ToNodes),
    Options = #{
        keep_leader => KeepLeader, rack_aware => RackAware, pick => Pick, shuffle => Shuffle
    },
    Partitions2 = assign_partitions(PartitionsToMove, PartitionsMetadata, CandidateNodes, Options),

    AlterPartitionReassignmentsRequest = #{
        timeout_ms => 5_000,
        topics => [
            #{name => Topic, partitions => Partitions2}
        ]
    },
    {ok, AlterPartitionReassignmentsResponse} = kafcod_connection:call(
        Co,
        fun alter_partition_reassignments_request:encode_alter_partition_reassignments_request_0/1,
        AlterPartitionReassignmentsRequest,
        fun alter_partition_reassignments_response:decode_alter_partition_reassignments_response_0/1
    ),
    assert_alter_partition_reassignments_response(AlterPartitionReassignmentsResponse),

    kafcod_connection:stop(Co),
    ok.

assert_alter_partition_reassignments_response(#{error_code := ?NONE, responses := Responses}) ->
    lists:foreach(
        fun(#{name := Topic, partitions := Partitions}) ->
            lists:foreach(
                fun
                    (
                        #{
                            partition_index := PartitionIndex,
                            error_code := ErrorCode,
                            error_message := ErrorMessage
                        }
                    ) when ErrorCode /= ?NONE ->
                        io:format("Topic ~s, partition ~B: error ~B: ~s~n", [
                            Topic, PartitionIndex, ErrorCode, ErrorMessage
                        ]);
                    (_) ->
                        ok
                end,
                Partitions
            )
        end,
        Responses
    ).

%% Return the broker objects from BrokersMetadata where the node ID is in NodeIds, in the same order as NodeIds.
get_candidate_nodes(BrokersMetadata, NodeIds) ->
    lists:foldr(
        fun(NodeId, Acc) ->
            case
                lists:search(
                    fun(#{node_id := Id}) -> Id =:= NodeId end, BrokersMetadata
                )
            of
                {value, Broker} -> [Broker | Acc];
                false -> Acc
            end
        end,
        [],
        NodeIds
    ).

assign_partitions(PartitionsToMove, PartitionsMetadata, CandidateNodes, Options) ->
    lists:map(
        fun(#{partition_index := P, replica_nodes := CurrentReplicas}) ->
            NewReplicas = assign_replicas(
                CurrentReplicas, CandidateNodes, Options
            ),
            #{partition_index => P, replicas => NewReplicas}
        end,
        lists:filter(
            fun(Partition) -> want_partition(Partition, PartitionsToMove) end,
            PartitionsMetadata
        )
    ).

% Build a list of replicas. If we're doing sticky leader, put the existing leader in there. As we put a replica in the list, remove its rack-mates from the candidate replicas. Then: take a random replica from the list until we either have enough replicas, or we run out of candidates.
assign_replicas(
    _CurrentReplicas,
    CandidateNodes,
    _Options = #{
        keep_leader := false, rack_aware := RackAware, pick := Pick, shuffle := Shuffle
    }
) ->
    do_assign_replicas([], maybe_shuffle(Shuffle, CandidateNodes), Pick, RackAware);
assign_replicas(
    _CurrentReplicas = [LeaderId | _],
    CandidateNodes,
    _Options = #{
        keep_leader := true, rack_aware := RackAware, pick := Pick, shuffle := Shuffle
    }
) ->
    LeaderRack =
        case {lists:search(fun(#{node_id := N}) -> N == LeaderId end, CandidateNodes), RackAware} of
            {_, false} -> undefined;
            {{value, #{rack := Rack}}, _} -> Rack;
            _ -> undefined
        end,
    do_assign_replicas(
        [LeaderId],
        remove_rack(LeaderRack, remove_leader(LeaderId, maybe_shuffle(Shuffle, CandidateNodes))),
        Pick - 1,
        RackAware
    ).

maybe_shuffle(false, CandidateNodes) ->
    ByNodeId = fun(#{node_id := X}, #{node_id := Y}) -> X =< Y end,
    lists:sort(ByNodeId, CandidateNodes);
maybe_shuffle(true, CandidateNodes) ->
    shuffle(CandidateNodes).

do_assign_replicas(Replicas, [], _Pick, _RackAware) ->
    lists:reverse(Replicas);
do_assign_replicas(Replicas, _Candidates, _Pick = 0, _RackAware) ->
    lists:reverse(Replicas);
do_assign_replicas(Replicas, [#{node_id := NodeId} | Candidates], Pick, RackAware = false) ->
    do_assign_replicas([NodeId | Replicas], Candidates, Pick - 1, RackAware);
do_assign_replicas(
    Replicas, [#{node_id := NodeId, rack := Rack} | Candidates], Pick, RackAware = true
) ->
    do_assign_replicas([NodeId | Replicas], remove_rack(Rack, Candidates), Pick - 1, RackAware).

remove_leader(NodeId, Candidates) ->
    lists:filter(fun(#{node_id := N}) -> N /= NodeId end, Candidates).

remove_rack(Rack, Candidates) ->
    lists:filter(fun(#{rack := R}) -> R /= Rack end, Candidates).

want_partition(_, _PartitionsToMove = undefined) ->
    true;
want_partition(#{partition_index := P}, PartitionsToMove) ->
    lists:member(P, PartitionsToMove).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

brokers_metadata() ->
    brokers_metadata(3).

brokers_metadata(Count) ->
    lists:map(
        fun(N) ->
            #{
                port => 9092 + N,
                host => broker_host(N),
                rack => broker_rack(N rem 3),
                node_id => 101 + N
            }
        end,
        lists:seq(0, Count - 1)
    ).

brokers_metadata_test() ->
    ?assertEqual(
        [
            #{port => 9092, host => <<"172.17.11.0">>, node_id => 101, rack => <<"eu-west-1a">>},
            #{port => 9093, host => <<"172.17.11.1">>, node_id => 102, rack => <<"eu-west-1b">>},
            #{port => 9094, host => <<"172.17.11.2">>, node_id => 103, rack => <<"eu-west-1c">>},
            #{port => 9095, host => <<"172.17.11.3">>, node_id => 104, rack => <<"eu-west-1a">>}
        ],
        brokers_metadata(4)
    ).

broker_host(N) ->
    iolist_to_binary(io_lib:format("172.17.11.~B", [N])).

broker_rack(0) ->
    <<"eu-west-1a">>;
broker_rack(1) ->
    <<"eu-west-1b">>;
broker_rack(2) ->
    <<"eu-west-1c">>.

get_candidate_nodes_test_() ->
    BrokersMetadata = brokers_metadata(),
    [
        ?_assertMatch(
            [#{node_id := 102}, #{node_id := 101}, #{node_id := 103}],
            get_candidate_nodes(BrokersMetadata, [102, 101, 103])
        ),
        ?_assertMatch(
            [#{node_id := 103}, #{node_id := 101}],
            get_candidate_nodes(BrokersMetadata, [103, 101])
        ),
        ?_assertMatch(
            [#{node_id := 103}, #{node_id := 101}],
            get_candidate_nodes(BrokersMetadata, [103, 101, 901])
        )
    ].

assign_replicas_basic_test_() ->
    BrokersMetadata = brokers_metadata(),
    BrokersMetadata2 = brokers_metadata(6),
    [
        ?_assertEqual(
            [101, 102, 103],
            assign_replicas([101, 102, 103], BrokersMetadata, #{
                keep_leader => false, rack_aware => false, pick => 3, shuffle => false
            })
        ),
        % Reducing the number of replicas
        ?_assertEqual(
            [101, 102],
            assign_replicas([101, 102, 103], BrokersMetadata, #{
                keep_leader => false, rack_aware => false, pick => 2, shuffle => false
            })
        ),
        % Increasing the number of replicas
        ?_assertEqual(
            [101, 102],
            assign_replicas([101], BrokersMetadata, #{
                keep_leader => false, rack_aware => false, pick => 2, shuffle => false
            })
        ),
        % More brokers than we need
        ?_assertEqual(
            [101, 102],
            assign_replicas([101, 102, 103], BrokersMetadata2, #{
                keep_leader => false, rack_aware => false, pick => 2, shuffle => false
            })
        ),
        % Fewer brokers than we need
        ?_assertEqual(
            [101, 102, 103],
            assign_replicas([101, 102, 103], BrokersMetadata, #{
                keep_leader => false, rack_aware => false, pick => 5, shuffle => false
            })
        )
    ].

assign_replicas_rack_aware_test_() ->
    BrokersMetadata = brokers_metadata(),
    BrokersMetadata2 = brokers_metadata(6),
    [
        ?_assertEqual(
            [101, 102, 103],
            assign_replicas([101], BrokersMetadata, #{
                keep_leader => false, rack_aware => true, pick => 3, shuffle => false
            })
        ),
        ?_assertEqual(
            [101, 102, 103, 104, 105, 106],
            assign_replicas([101], BrokersMetadata2, #{
                keep_leader => false, rack_aware => false, pick => 6, shuffle => false
            })
        ),
        ?_assertEqual(
            [101, 102, 103],
            assign_replicas([101], BrokersMetadata2, #{
                keep_leader => false, rack_aware => true, pick => 6, shuffle => false
            })
        )
    ].

assign_replicas_keep_leader_test_() ->
    BrokersMetadata = brokers_metadata(),
    BrokersMetadata2 = brokers_metadata(6),
    [
        ?_assertEqual(
            [103, 101, 102],
            assign_replicas([103], BrokersMetadata, #{
                keep_leader => true, rack_aware => true, pick => 3, shuffle => false
            })
        ),
        ?_assertEqual(
            [103, 101, 102, 104, 105, 106],
            assign_replicas([103], BrokersMetadata2, #{
                keep_leader => true, rack_aware => false, pick => 6, shuffle => false
            })
        ),
        ?_assertEqual(
            [103, 101, 102],
            assign_replicas([103], BrokersMetadata2, #{
                keep_leader => true, rack_aware => true, pick => 6, shuffle => false
            })
        ),
        ?_assertEqual(
            [105, 101, 103],
            assign_replicas([105], BrokersMetadata2, #{
                keep_leader => true, rack_aware => true, pick => 3, shuffle => false
            })
        )
    ].

assign_replicas_keep_leader_shuffle_test() ->
    BrokersMetadata = brokers_metadata(),
    case
        assign_replicas([103], BrokersMetadata, #{
            keep_leader => true, rack_aware => true, pick => 3, shuffle => true
        })
    of
        [103, 101, 102] -> ok;
        [103, 102, 101] -> ok;
        _ -> ?assert(false)
    end.

assign_replicas_real_test() ->
    BrokersMetadata2 = brokers_metadata(6),
    case
        assign_replicas([101, 104, 102], BrokersMetadata2, #{
            keep_leader => true, rack_aware => true, pick => 3, shuffle => true
        })
    of
        [101, X, Y] when X rem 3 /= Y rem 3, X rem 3 /= 2, Y rem 3 /= 2 -> ok;
        _ -> ?assert(false)
    end.

-endif.

elect_leaders(
    Args = #{
        bootstrap := Bootstrap,
        topic := Topic
    }
) ->
    % Connect to the bootstrap broker.
    {ok, Bo} = kafcod_connection:start_link(Bootstrap),
    Controller = find_controller(Bo),
    kafcod_connection:stop(Bo),

    {ok, Co} = kafcod_connection:start_link(Controller),

    {ok, #{topics := [#{name := Topic, error_code := ?NONE, partitions := Partitions}]}} = kafcod_connection:call(
        Co,
        fun metadata_request:encode_metadata_request_7/1,
        #{allow_auto_topic_creation => false, topics => [#{name => Topic}]},
        fun metadata_response:decode_metadata_response_7/1
    ),

    % If no partitions are specified, default to all of them.
    ExistingPartitions = [P || #{partition_index := P} <- Partitions],
    PartitionsToMove = maps:get(partition, Args, ExistingPartitions),

    {ok, _} = kafcod_connection:call(
        Controller,
        fun elect_leaders_request:encode_elect_leaders_request_0/1,
        #{
            timeout_ms => 5_000,
            topic_partitions => [
                #{
                    topic => Topic,
                    partitions => PartitionsToMove
                }
            ]
        },
        fun elect_leaders_response:decode_elect_leaders_response_0/1
    ),

    kafcod_connection:stop(Co),
    ok.

list_partition_reassignments(#{bootstrap := Bootstrap}) ->
    {ok, Bo} = kafcod_connection:start_link(Bootstrap),
    Controller = find_controller(Bo),
    kafcod_connection:stop(Bo),

    {ok, Co} = kafcod_connection:start_link(Controller),

    {ok, #{error_code := ?NONE, topics := Topics}} = kafcod_connection:call(
        Co,
        fun list_partition_reassignments_request:encode_list_partition_reassignments_request_0/1,
        #{timeout_ms => 5_000, topics => null},
        fun list_partition_reassignments_response:decode_list_partition_reassignments_response_0/1
    ),

    kafcod_connection:stop(Co),
    io:format("~p~n", [Topics]),
    ok.

validate_replication(#{bootstrap := Bootstrap, pick := Pick, rack_aware := RackAware}) ->
    {ok, Bo} = kafcod_connection:start_link(Bootstrap),
    Controller = find_controller(Bo),
    kafcod_connection:stop(Bo),

    {ok, Co} = kafcod_connection:start_link(Controller),

    {ok, #{topics := TopicsMetadata0, brokers := BrokersMetadata}} = kafcod_connection:call(
        Co,
        fun metadata_request:encode_metadata_request_7/1,
        #{allow_auto_topic_creation => false, topics => null},
        fun metadata_response:decode_metadata_response_7/1
    ),

    TopicsMetadata = sort_topics_metadata(TopicsMetadata0),

    lists:foreach(
        fun
            (#{name := Topic, error_code := ?NONE, is_internal := false, partitions := Partitions}) ->
                lists:foreach(
                    fun(#{partition_index := P, error_code := ?NONE, replica_nodes := Replicas}) ->
                        case length(Replicas) of
                            Pick ->
                                ok;
                            Count ->
                                io:format("topic ~s partition ~B has ~B replicas~n", [
                                    Topic, P, Count
                                ]),
                                ok
                        end,
                        validate_rack_allocation(Topic, P, Replicas, BrokersMetadata, RackAware)
                    end,
                    Partitions
                );
            (#{name := Topic}) ->
                io:format("skipping topic ~s~n", [Topic])
        end,
        TopicsMetadata
    ),
    ok.

validate_rack_allocation(Topic, P, Replicas, BrokersMetadata, _RackAware = true) ->
    % Are the replicas in different racks?
    lists:foldl(
        fun(NodeId, Set) ->
            {value, Broker} = lists:search(
                fun(#{node_id := N}) -> N == NodeId end, BrokersMetadata
            ),
            #{rack := Rack} = Broker,
            case sets:is_element(Rack, Set) of
                true ->
                    io:format("topic ~s partition ~B has multiple partitions in rack ~s~n", [
                        Topic, P, Rack
                    ]),
                    Set;
                false ->
                    sets:add_element(Rack, Set)
            end
        end,
        sets:new(),
        Replicas
    );
validate_rack_allocation(_T, _P, _R, _B, _RackAware = false) ->
    ok.

sort_topics_metadata(TopicsMetadata) ->
    lists:sort(fun(#{name := X}, #{name := Y}) -> X =< Y end, TopicsMetadata).

shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

find_controller(Broker) when is_pid(Broker) ->
    % Find the controller for the cluster.
    {ok, #{error_code := ?NONE, controller_id := ControllerId, brokers := Brokers}} = kafcod_connection:call(
        Broker,
        fun describe_cluster_request:encode_describe_cluster_request_0/1,
        #{include_cluster_authorized_operations => false},
        fun describe_cluster_response:decode_describe_cluster_response_0/1
    ),
    {value, #{host := ControllerHost, port := ControllerPort}} = lists:search(
        fun(#{broker_id := BrokerId}) -> BrokerId =:= ControllerId end, Brokers
    ),
    #{host => ControllerHost, port => ControllerPort}.
