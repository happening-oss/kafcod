-module(consumer_sup).
-export([
    start_link/0,
    start_child/4
]).
-behaviour(supervisor).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, {}).

-spec start_child(Sup :: pid(), ClientId :: binary(), Broker :: #{host := binary(), port := non_neg_integer()},
    TopicPartitionOffsets :: consumer:topic_partition_offsets()) -> {ok, pid()}.
start_child(Sup, ClientId, Broker = #{host := Host, port := Port}, TopicPartitionOffsets) when
    is_pid(Sup),
    is_binary(Host),
    is_integer(Port),
    is_map(TopicPartitionOffsets)
->
    % ClientId could be in the static part of simple_one_for_one.
    ChildSpec = [ClientId, Broker, TopicPartitionOffsets],
    supervisor:start_child(Sup, ChildSpec).

init({}) ->
    % TODO: Do we really want simple_one_for_one here? Maybe having id = NodeId would be better?
    % TODO: restart strategy. Hmmm. Defaults to permanent, and yet the consumers are dying...?
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec = #{id => undefined, start => {consumer, start_link, []}},
    {ok, {SupFlags, [ChildSpec]}}.
