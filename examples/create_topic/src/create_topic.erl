-module(create_topic).
-export([main/1]).

-record(state, {
    client_id,
    socket,
    topic,
    num_partitions,
    replication_factor
}).

main([Topic]) ->
    % Connect to any broker.
    Host = "localhost",
    Port = 9092,
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, {mode, binary}, {packet, 4}]),

    ClientId = list_to_binary(?MODULE_STRING),

    StateData = #state{
        client_id = ClientId,
        socket = Socket,
        topic = list_to_binary(Topic),
        num_partitions = 10,
        replication_factor = 3
    },

    % We skip sending the ApiVersionsRequest, since we know the versions supported by the broker we're using (see
    % docker-compose.yml at the top level).

    % Poor man's gen_statem.
    enter_state(metadata_request, StateData).

enter_state(metadata_request, StateData = #state{socket = Socket, client_id = ClientId}) ->
    % Send a MetadataRequest v7 to find the controller.
    Request = metadata_request:encode_metadata_request_7(#{
        correlation_id => 1,
        client_id => ClientId,
        allow_auto_topic_creation => false,
        topics => null
    }),
    gen_tcp:send(Socket, Request),
    loop_state(metadata_response, StateData);
enter_state(create_topics_request, StateData = #state{socket = Socket, client_id = ClientId}) ->
    Request = create_topics_request:encode_create_topics_request_2(#{
        correlation_id => 2,
        client_id => ClientId,
        topics => [
            #{
                name => StateData#state.topic,
                num_partitions => StateData#state.num_partitions,
                replication_factor => StateData#state.replication_factor,
                configs => [],
                assignments => []
            }
        ],
        timeout_ms => 3_000,
        validate_only => false
    }),
    gen_tcp:send(Socket, Request),
    loop_state(create_topics_response, StateData).

loop_state(State, StateData) ->
    receive
        {tcp, _, Bytes} ->
            % io:format("~p~n", [Bytes]),
            next_state(Bytes, State, StateData);
        M ->
            error(M)
    end.

next_state(Buffer, metadata_response, StateData = #state{socket = Socket0}) ->
    {Response, <<>>} = metadata_response:decode_metadata_response_7(Buffer),
    % io:format("~p~n", [Response]),

    % Connect to the controller.
    #{controller_id := ControllerId, brokers := Brokers} = Response,
    [Controller] = lists:filter(fun(#{node_id := NodeId}) -> NodeId =:= ControllerId end, Brokers),
    #{host := Host, port := Port} = Controller,

    gen_tcp:close(Socket0),

    {ok, Socket} = gen_tcp:connect(binary_to_list(Host), Port, [
        {active, true}, {mode, binary}, {packet, 4}
    ]),
    enter_state(create_topics_request, StateData#state{socket = Socket});
next_state(Buffer, create_topics_response, StateData) ->
    {Response, <<>>} = create_topics_response:decode_create_topics_response_2(Buffer),
    #{topics := [#{error_code := 0}]} = Response,
    terminate(StateData).

terminate(_StateData) ->
    erlang:halt(0).
