-module(simple_topic_consumer).
-export([
    start_link/4
]).
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

-include_lib("kafcod/include/isolation_level.hrl").

start_link(Host, Port, Topic, Partitions) when
    is_list(Host), is_integer(Port), is_binary(Topic), is_list(Partitions)
->
    gen_statem:start_link(?MODULE, [Host, Port, Topic, Partitions], []).

-record(state, {
    client_id,
    topic,
    partition_offsets,
    connection
}).

callback_mode() ->
    [handle_event_function].

init([Host, Port, Topic, Partitions0]) ->
    ClientId = list_to_binary(?MODULE_STRING),

    {ok, Conn} = kafka_connection:start_link(Host, Port, ClientId),

    StateData = #state{
        client_id = ClientId,
        topic = Topic,
        connection = Conn
    },

    {ok, connected, StateData, {next_event, internal, {list_offsets, Partitions0}}}.


handle_event(internal, {list_offsets, Partitions}, connected, StateData = #state{connection = Conn, topic = Topic}) ->
    PartitionOffsets = list_offsets(Conn, Topic, Partitions),
    StateData2 = StateData#state{partition_offsets = PartitionOffsets},
    {keep_state, StateData2, [{next_event, internal, fetch}]};
handle_event(internal, fetch, connected, StateData) ->
    StateData2 = do_fetch(StateData),
    {keep_state, StateData2, [{next_event, internal, fetch}]}.

list_offsets(Conn, Topic, PartitionIndexes) ->
    {ok, OffsetsResponse} = kafka_connection:call(
        Conn,
        fun list_offsets_request:encode_list_offsets_request_7/1,
        #{
            % we're a consumer, -1 is correct.
            replica_id => -1,
            isolation_level => ?READ_COMMITTED,
            topics => [
                #{
                    name => Topic,
                    partitions => [
                        #{
                            partition_index => Partition,
                            current_leader_epoch => -1,
                            timestamp => -2
                        }
                     || Partition <- PartitionIndexes
                    ]
                }
            ]
        },
        fun list_offsets_response:decode_list_offsets_response_7/1
    ),

    #{topics := [#{name := Topic, partitions := Partitions}]} = OffsetsResponse,
    lists:map(
        fun(#{partition_index := Partition, offset := Offset, error_code := 0}) ->
            {Partition, Offset}
        end,
        Partitions
    ).

do_fetch(StateData = #state{client_id = ClientId, connection = Conn, topic = Topic, partition_offsets = PartitionOffsets}) ->
    io:format("Fetching from ~s, ~p~n", [Topic, PartitionOffsets]),
    MaxWaitMs = 5_000,
    MaxBytes = 1_024,
    {ok, FetchResponse} = kafka_connection:call(
        Conn,
        fun fetch_request:encode_fetch_request_12/1,
        #{
            client_id => ClientId,
            replica_id => -1,
            max_wait_ms => MaxWaitMs,
            min_bytes => 1,
            max_bytes => MaxBytes,
            isolation_level => 1,
            topics => [
                #{
                    topic => Topic,
                    partitions => [
                        #{
                            partition => Partition,
                            fetch_offset => FetchOffset,
                            partition_max_bytes => MaxBytes,
                            log_start_offset => -1,
                            current_leader_epoch => -1,
                            last_fetched_epoch => -1
                        }
                     || {Partition, FetchOffset} <- PartitionOffsets
                    ]
                }
            ],
            session_id => 0,
            session_epoch => -1,
            rack_id => <<"undefined">>,
            forgotten_topics_data => []
        },
        fun fetch_response:decode_fetch_response_12/1
    ),
    % io:format("~p~n", [FetchResponse]),
    handle_fetch_response(FetchResponse, StateData).

handle_fetch_response(
    _FetchResponse = #{error_code := 0, responses := Responses},
    StateData = #state{partition_offsets = PreviousOffsets}
) ->
    % Most clients just want a flattened list of records per topic, per partition. They don't want the whole mess of
    % nested maps.
    % However, because we want to fetch from where we left off, we need to keep track of the final offset, which
    % needs a little extra.

    % We're only dealing with a single topic.
    [#{topic := Topic, partitions := Partitions}] = Responses,

    % Map over the partitions, because we want (partition, offset) for the next fetch.
    PartitionOffsets = lists:map(
        fun(
            _PartitionData = #{
                partition_index := PartitionIndex, error_code := 0, records := MessageSet
            }
        ) ->
            {_, PreviousOffset} = lists:keyfind(PartitionIndex, 1, PreviousOffsets),
            % The partition data contains a message set, which is a list of record batches. We want to flatten that,
            % and we need the offsets.
            NextOffset = lists:foldl(
                fun(
                    _RecordBatch = #{
                        base_offset := BaseOffset,
                        base_timestamp := BaseTimestamp,
                        last_offset_delta := LastOffsetDelta,
                        records := Records
                    },
                    _Acc
                ) ->
                    lists:foreach(
                        fun(
                            _Record = #{
                                key := Key,
                                value := Value,
                                headers := Headers,
                                offset_delta := OffsetDelta,
                                timestamp_delta := TimestampDelta
                            }
                        ) ->
                            io:format("~s#~B: ~B@~s: ~s = ~s (~p)~n", [
                                Topic,
                                PartitionIndex,
                                BaseOffset + OffsetDelta,
                                calendar:system_time_to_rfc3339(
                                    BaseTimestamp + TimestampDelta, [{unit, millisecond}]
                                ),
                                Key,
                                Value,
                                Headers
                            ])
                        end,
                        Records
                    ),

                    % The record batches are in ascending offset order, so we only need to remember the last one.
                    BaseOffset + LastOffsetDelta + 1
                end,
                % Pass PreviousOffset as the accumulator, so that if there aren't any records, it's returned as the NextOffset.
                PreviousOffset,
                MessageSet
            ),

            {PartitionIndex, NextOffset}
        end,
        Partitions
    ),

    StateData#state{partition_offsets = PartitionOffsets}.
