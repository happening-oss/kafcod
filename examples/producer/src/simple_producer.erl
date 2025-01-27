-module(simple_producer).
-export([
    start_link/2,
    produce/6
]).
-behaviour(gen_statem).
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).
start_link(Host, Port) when
    is_list(Host), is_integer(Port)
->
    gen_statem:start_link(?MODULE, [Host, Port], []).

produce(Pid, Topic, Partition, Key, Value, Headers) ->
    gen_statem:call(
        Pid, {produce, {Topic, Partition}, #{key => Key, value => Value, headers => Headers}}
    ).

callback_mode() ->
    handle_event_function.

-record(state, {
    client_id,
    socket,
    correlation_id
}).

init([Host, Port]) ->
    ClientId = list_to_binary(?MODULE_STRING),

    CorrelationId = 1,

    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, {mode, binary}, {packet, 4}]),
    {ok, connected, #state{
        client_id = ClientId,
        socket = Socket,
        correlation_id = CorrelationId
    }}.

handle_event(
    {call, From},
    {produce, {Topic, Partition}, Message = #{key := _Key, value := _Value, headers := _Headers}},
    connected,
    StateData = #state{client_id = ClientId, socket = Socket, correlation_id = CorrelationId}
) ->
    % Given the (topic, partition), find the leader. That requires a metadata call:
    MetadataRequest = metadata_request:encode_metadata_request_7(
        #{
            correlation_id => CorrelationId,
            client_id => ClientId,
            allow_auto_topic_creation => false,
            topics => [#{name => Topic}]
        }
    ),
    gen_tcp:send(Socket, MetadataRequest),
    {next_state, {metadata_response, From, Topic, Partition, Message}, StateData#state{
        client_id = ClientId, socket = Socket, correlation_id = CorrelationId + 1
    }};
handle_event(
    info,
    {tcp, Socket, Buffer},
    {metadata_response, From, Topic, Partition, Message},
    StateData = #state{
        client_id = ClientId,
        socket = Socket,
        correlation_id = CorrelationId
    }
) ->
    {MetadataResponse, <<>>} = metadata_response:decode_metadata_response_7(Buffer),
    io:format("~p~n", [MetadataResponse]),
    % Find the partition we're interested in; get the leader.
    #{topics := [#{name := Topic, error_code := 0, partitions := PartitionDetails}]} =
        MetadataResponse,

    [#{leader_id := LeaderId, leader_epoch := LeaderEpoch}] = [
        PD
     || PD = #{error_code := 0, partition_index := P} <- PartitionDetails, P =:= Partition
    ],

    % Get the details for that broker; reconnect.
    #{brokers := Brokers} = MetadataResponse,
    [#{host := Host, port := Port}] = [
        Broker
     || Broker = #{node_id := NodeId} <- Brokers, NodeId =:= LeaderId
    ],
    gen_tcp:close(Socket),
    {ok, Socket2} = gen_tcp:connect(binary_to_list(Host), Port, [
        {active, true}, {mode, binary}, {packet, 4}
    ]),
    Records = prepare_message_set(LeaderEpoch, [Message]),
    io:format("~p~n", [Records]),
    % Send the message.
    ProduceRequest = produce_request:encode_produce_request_7(#{
        client_id => ClientId,
        correlation_id => CorrelationId,
        transactional_id => null,
        acks => -1,
        timeout_ms => 5_000,
        topic_data => [
            #{
                name => Topic,
                partition_data => [
                    #{index => Partition, records => Records}
                ]
            }
        ]
    }),
    gen_tcp:send(Socket2, ProduceRequest),
    {next_state, {produce_response, From}, StateData#state{
        socket = Socket2, correlation_id = CorrelationId + 1
    }};
handle_event(
    info, {tcp, Socket, Buffer}, {produce_response, From}, StateData = #state{socket = Socket}
) ->
    {ProduceResponse, <<>>} = produce_response:decode_produce_response_7(Buffer),
    #{responses := [#{partition_responses := [#{error_code := ErrorCode}]}]} = ProduceResponse,
    Reply =
        case ErrorCode of
            0 -> ok;
            E -> {error, E}
        end,
    {next_state, connected, StateData, {reply, From, Reply}}.

prepare_message_set(PartitionLeaderEpoch, Messages) ->
    [
        #{
            % We don't know the offset, so it's always zero.
            base_offset => 0,
            partition_leader_epoch => PartitionLeaderEpoch,
            magic => 2,
            % The CRC is calculated in kafcod_record_batch:encode_record_batch; show it here for clarity.
            crc => -1,
            attributes => 0,
            % TODO: kcat sends zero here for single records. For multiple records, it must be, well, the last offset delta.
            last_offset_delta => 0,
            % TODO: What should the timestamps be set to?
            base_timestamp => 0,
            max_timestamp => 0,
            producer_id => -1,
            producer_epoch => -1,
            base_sequence => -1,
            records => prepare_records(Messages)
        }
    ].

prepare_records(Messages) ->
    lists:map(
        fun(#{key := Key, value := Value, headers := Headers}) ->
            #{
                attributes => 0,
                timestamp_delta => 0,
                offset_delta => 0,
                key => Key,
                value => Value,
                headers => Headers
            }
        end,
        Messages
    ).
