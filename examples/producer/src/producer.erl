-module(producer).
-export([main/1]).

main(_Args) ->
    Host = "localhost",
    Port = 9092,
    Topic = <<"cars">>,
    Partition = 1,
    Key = <<"key">>,
    Value = <<"value">>,
    Headers = [],
    {ok, Pid} = simple_producer:start_link(Host, Port),
    simple_producer:produce(Pid, Topic, Partition, Key, Value, Headers),
    MRef = monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ok
    end.
