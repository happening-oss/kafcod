-module(reassign).
-export([main/1]).
-export([
    reassign_partitions/1,
    list_partition_reassignments/1
]).

-define(CLIENT_ID, list_to_binary(?MODULE_STRING)).

main(Args) ->
    argparse:run(Args, reassign_cli:cli(), #{progname => ?MODULE}).

reassign_partitions(
    Args = #{
        bootstrap := [{BootstrapHost, BootstrapPort}],
        topic := Topic,
        to := ToNodes,
        pick := Pick,
        shuffle := Shuffle
    }
) ->
    % Connect to the bootstrap broker.
    {ok, Bootstrap} = kafka_connection:start_link(BootstrapHost, BootstrapPort, ?CLIENT_ID),

    #{host := ControllerHost, port := ControllerPort} = find_controller(Bootstrap),
    {ok, Controller} = kafka_connection:start_link(ControllerHost, ControllerPort, ?CLIENT_ID),

    {ok, #{topics := [#{name := Topic, error_code := 0, partitions := Partitions}]}} = kafka_connection:call(
        Controller,
        fun metadata_request:encode_metadata_request_7/1,
        #{allow_auto_topic_creation => false, topics => [#{name => Topic}]},
        fun metadata_response:decode_metadata_response_7/1
    ),

    % If no partitions are specified, default to all of them.
    PartitionsToMove = maps:get(partition, Args, [P || #{partition_index := P} <- Partitions]),

    Partitions2 = lists:map(
        fun(#{partition_index := P, replica_nodes := _}) ->
            Replicas2 =
                case Shuffle of
                    true -> shuffle(ToNodes);
                    _ -> ToNodes
                end,
            #{partition_index => P, replicas => lists:sublist(Replicas2, Pick)}
        end,
        lists:filter(
            fun(#{partition_index := P}) -> lists:member(P, PartitionsToMove) end, Partitions
        )
    ),

    {ok, #{error_code := 0}} = kafka_connection:call(
        Controller,
        fun alter_partition_reassignments_request:encode_alter_partition_reassignments_request_0/1,
        #{
            timeout_ms => 5_000,
            topics => [
                #{
                    name => Topic,
                    partitions => Partitions2
                }
            ]
        },
        fun alter_partition_reassignments_response:decode_alter_partition_reassignments_response_0/1
    ),
    ok.

list_partition_reassignments(#{bootstrap := [{BootstrapHost, BootstrapPort}]}) ->
    {ok, Bootstrap} = kafka_connection:start_link(BootstrapHost, BootstrapPort, ?CLIENT_ID),

    #{host := ControllerHost, port := ControllerPort} = find_controller(Bootstrap),
    {ok, Controller} = kafka_connection:start_link(ControllerHost, ControllerPort, ?CLIENT_ID),

    {ok, #{error_code := 0, topics := Topics}} = kafka_connection:call(
        Controller,
        fun list_partition_reassignments_request:encode_list_partition_reassignments_request_0/1,
        #{timeout_ms => 5_000, topics => null},
        fun list_partition_reassignments_response:decode_list_partition_reassignments_response_0/1
    ),

    io:format("~p~n", [Topics]),
    ok.

shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

find_controller(Broker) when is_pid(Broker) ->
    % Find the controller for the cluster.
    {ok, #{error_code := 0, controller_id := ControllerId, brokers := Brokers}} = kafka_connection:call(
        Broker,
        fun describe_cluster_request:encode_describe_cluster_request_0/1,
        #{include_cluster_authorized_operations => false},
        fun describe_cluster_response:decode_describe_cluster_response_0/1
    ),
    {value, #{host := ControllerHost, port := ControllerPort}} = lists:search(
        fun(#{broker_id := BrokerId}) -> BrokerId =:= ControllerId end, Brokers
    ),
    #{host => ControllerHost, port => ControllerPort}.
