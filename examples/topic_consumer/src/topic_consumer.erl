-module(topic_consumer).
-export([main/1]).

-define(CLIENT_ID, list_to_binary(?MODULE_STRING)).

main(Args) ->
    argparse:run(
        Args,
        #{
            arguments => [
                #{name => topic, help => "Topic to consume from", type => binary}
            ],
            handler => fun topic_consumer/1
        },
        #{progname => ?MODULE}
    ).

topic_consumer(#{topic := TopicName}) ->
    {ok, K} = kafka_connection:start_link("localhost", 9092, ?CLIENT_ID),

    % Find out how many partitions the given topic has.
    {ok, Metadata} = kafka_connection:call(
        K,
        fun metadata_request:encode_metadata_request_7/1,
        #{topics => [#{name => TopicName}], allow_auto_topic_creation => false},
        fun metadata_response:decode_metadata_response_7/1
    ),

    kafka_connection:stop(K),

    #{brokers := Brokers0, topics := [Topic]} = Metadata,
    Brokers = lists:foldl(
        fun(Broker = #{node_id := NodeId}, Acc) -> Acc#{NodeId => Broker} end, #{}, Brokers0
    ),
    #{error_code := 0} = Topic,
    #{partitions := Partitions} = Topic,

    % Group partitions by leader.
    PartitionsByLeader = group_by(
        fun(#{leader_id := LeaderId}) -> LeaderId end,
        fun(#{partition_index := PartitionIndex}) -> PartitionIndex end,
        Partitions
    ),

    % Start the consumers: one per broker.
    _Pids = start_consumers(Brokers, TopicName, PartitionsByLeader),
    receive
        stop ->
            ok
    end,
    ok.

start_consumers(Brokers, Topic, PartitionsByLeader) ->
    maps:map(
        fun(LeaderId, Partitions) ->
            Broker = maps:get(LeaderId, Brokers),
            #{host := Host, port := Port} = Broker,
            io:format("Starting consumer for broker ~B, ~s:~B~n", [LeaderId, Host, Port]),
            {ok, Pid} = simple_topic_consumer:start_link(
                binary_to_list(Host), Port, Topic, Partitions
            ),
            Pid
        end,
        PartitionsByLeader
    ).

%% Splits a list into groups based on KeyFun.
%%
%% Returns a map where each key is given by KeyFun and each value is a list of the elements with that key.
%% The order of elements within each list is undefined (actually reversed, but that's an implementation detail).
%%
%% Similar to Elixir's Enum.group_by/3.
group_by(KeyFun, List) ->
    group_by(KeyFun, fun(Elem) -> Elem end, List).

%% Splits a list into groups based on KeyFun.
%%
%% Returns a map where each key is given by KeyFun and each value is a list of elements given by ValueFun.
%% The order of elements within each list is undefined (actually reversed, but that's an implementation detail).
%%
%% Similar to Elixir's Enum.group_by/3.
group_by(KeyFun, ValueFun, List) ->
    lists:foldl(
        fun(Elem, Acc) ->
            Key = KeyFun(Elem),
            Value = ValueFun(Elem),
            case Acc of
                #{Key := Curr} ->
                    Acc#{Key := [Value | Curr]};
                #{} ->
                    Acc#{Key => [Value]}
            end
        end,
        #{},
        List
    ).
