-module(consumer).
-export([
    start_link/3
]).
-behaviour(gen_statem).
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kafcod/include/error_code.hrl").
-include_lib("kafcod/include/isolation_level.hrl").
-include_lib("kafcod/include/timestamp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% TODO: Make this configurable.
-define(MAX_WAIT_MS, 5_000).

% TODO: This is deliberately low, to exercise the chunking behaviour. It needs to be increased for release.
-define(MAX_BYTES, 1_024).

% TODO: Consistency with arg order -- does ClientId go first/last, e.g.?
% TODO: If it goes first, then we can put it in the simple_one_for_one spec. BUT: we might not want simple_one_for_one.
-export_type([topic_partition_offsets/0]).
-type topic_partition_offsets() :: #{
    Topic :: binary() := #{PartitionIndex :: non_neg_integer() := CommitOffset :: integer()}}.
-spec start_link(
    ClientId :: binary(),
    Broker :: #{host := binary(), port := non_neg_integer()},
    TopicPartitionOffsets :: topic_partition_offsets()
) -> {ok, pid()}.
start_link(ClientId, Broker = #{host := Host, port := Port}, TopicPartitionOffsets) when
    is_binary(Host), is_integer(Port), is_map(TopicPartitionOffsets)
->
    gen_statem:start_link(?MODULE, {ClientId, Broker, TopicPartitionOffsets}, start_options()).

% start_options() -> [].
start_options() -> [{debug, [trace]}].

-record(state, {
    connection,
    offsets :: topic_partition_offsets()
}).

callback_mode() ->
    [handle_event_function].

init({ClientId, _Broker = #{host := Host, port := Port}, Offsets}) ->
    {ok, Conn} = kafka_connection:start_link(Host, Port, ClientId),
    ?LOG_DEBUG("~p connected to ~s:~B", [Conn, Host, Port]),
    StateData = #state{
        connection = Conn,
        offsets = Offsets
    },
    {ok, connected, StateData, {next_event, internal, fetch}}.

handle_event(internal, fetch, connected, StateData) ->
    StateData2 = do_fetch(StateData),
    {keep_state, StateData2, [{next_event, internal, fetch}]}.

do_fetch(
    StateData = #state{connection = Conn, offsets = Offsets}
) ->
    ?LOG_DEBUG("Fetching from ~p~n", [Offsets]),
    FetchRequest = build_fetch_request(Offsets),
    {ok, FetchResponse} = kafka_connection:call(
        Conn,
        fun fetch_request:encode_fetch_request_11/1,
        FetchRequest,
        fun fetch_response:decode_fetch_response_11/1
    ),
    handle_fetch_response(FetchResponse, StateData).

build_fetch_request(Offsets) ->
    #{
        replica_id => -1,
        max_wait_ms => ?MAX_WAIT_MS,
        min_bytes => 1,
        max_bytes => ?MAX_BYTES,
        isolation_level => 1,
        topics => build_fetch_topics(Offsets),
        session_id => 0,
        session_epoch => -1,
        rack_id => <<"undefined">>,
        forgotten_topics_data => []
    }.

build_fetch_topics(Offsets) ->
    [
        #{
            topic => Topic,
            partitions => [
                #{
                    partition => PartitionIndex,
                    fetch_offset => FetchOffset,

                    partition_max_bytes => ?MAX_BYTES,
                    log_start_offset => -1,
                    current_leader_epoch => -1
                }
             || {PartitionIndex, FetchOffset} <- PartitionOffsets
            ]
        }
     || Topic := PartitionOffsets <- Offsets
    ].

% TODO: Fetch response with offset out of range => ListOffsets. Which then gets awkward, 'cos what scope is the error
% reported at? It's reported at the partition scope. Which means we may have a mix of successful and failed responses.
% Ideally we'd dispatch the OK ones while we go and get the offsets for the failed ones. This is where the crazy NEI
% stuff could be useful. OTOH, we can probably get away with partitioning the results, and issuing [{next_event,
% internal, list_offsets}, {next_event, internal, fetch}] -- as long as the list_offsets happens first, we're golden,
% because the state will be correct when we do the fetch.

% TODO: Almost all of this is identical (including stuff above) with a simple (non-group) consumer.
% TODO: Can we easily share them? It might make testing easier.
handle_fetch_response(
    _FetchResponse = #{error_code := 0, responses := Responses},
    StateData = #state{connection = Connection, offsets = Offsets}
) ->
    % Most clients just want a flattened list of records per topic, per partition. They don't want the whole mess of
    % nested maps.
    % However, because we want to fetch from where we left off, we need to keep track of the final offset, which
    % needs a little extra.
    #{offsets := Offsets2, records := Records, errors := Errors} = lists:foldl(
        fun handle_fetchable_topic_response/2, new_fetch_acc(Offsets), Responses
    ),
    Offsets3 = handle_offsets_out_of_range(Errors, Connection, Offsets2),

    % TODO: Update the offsets from the records (maybe do it in handle_fetchable_topic_response?).
    StateData#state{offsets = Offsets3}.

handle_fetchable_topic_response(#{topic := Topic, partitions := Partitions}, Acc) ->
    lists:foldl(fun handle_partition_data/2, set_topic(Topic, Acc), Partitions).

handle_partition_data(
    #{partition_index := PartitionIndex, error_code := ?NONE, records := Records}, Acc
) ->
    lists:foldl(fun handle_record_batch/2, set_partition_index(PartitionIndex, Acc), Records);
handle_partition_data(#{partition_index := PartitionIndex, error_code := ErrorCode}, Acc) ->
    add_error(PartitionIndex, ErrorCode, Acc).

handle_record_batch(
    _RecordBatch = #{
        base_offset := BaseOffset,
        base_timestamp := BaseTimestamp,
        last_offset_delta := LastOffsetDelta,
        records := Records
    },
    Acc
) ->
    % TODO: If there are no records, do we get called?
    % TODO: On balance, I'm not sure I'm cool with this set_base_offset thing; if I need to set more than one thing, it
    % gets awkward. Could use a pipe operator; could update the map in one go. Not sure.
    % The ambiguity about what's actually in the accumulator is also bothering me. e.g. did we set partition_offset yet?
    Acc2 = set_base_timestamp(BaseTimestamp, set_base_offset(BaseOffset, Acc)),
    Acc3 = lists:foldl(fun handle_record/2, Acc2, Records),
    update_offset(BaseOffset, LastOffsetDelta, Acc3).

handle_record(
    _Record = #{
        key := Key,
        value := Value,
        headers := Headers,
        offset_delta := OffsetDelta,
        timestamp_delta := TimestampDelta
    },
    Acc = #{
        topic := Topic,
        partition_index := PartitionIndex,
        base_offset := BaseOffset,
        base_timestamp := BaseTimestamp
    }
) ->
    ?LOG_INFO("~s#~B: ~B@~s: ~s = ~s (~p)", [
        Topic,
        PartitionIndex,
        BaseOffset + OffsetDelta,
        calendar:system_time_to_rfc3339(
            BaseTimestamp + TimestampDelta, [{unit, millisecond}]
        ),
        Key,
        Value,
        Headers
    ]),
    Acc.

new_fetch_acc(Offsets) ->
    #{topic => undefined, offsets => Offsets, records => [], errors => []}.

set_topic(Topic, Acc) ->
    Acc#{topic => Topic}.

set_partition_index(PartitionIndex, Acc) ->
    Acc#{partition_index => PartitionIndex}.

set_base_offset(BaseOffset, Acc) ->
    Acc#{base_offset => BaseOffset}.

set_base_timestamp(BaseTimestamp, Acc) ->
    Acc#{base_timestamp => BaseTimestamp}.

update_offset(
    BaseOffset,
    LastOffsetDelta,
    Acc = #{topic := Topic, partition_index := PartitionIndex, offsets := Offsets}
) ->
    % TODO: Not happy with the separation between this and the record batch. It's not necessarily clear that the batches
    % are processed in order and that this is OK.
    Offset = BaseOffset + LastOffsetDelta + 1,
    Offsets2 = maps:update_with(
        Topic,
        fun(PartitionOffsets) ->
            lists:keyreplace(
                PartitionIndex,
                1,
                PartitionOffsets,
                {PartitionIndex, Offset}
            )
        end,
        Offsets
    ),
    Acc#{offsets := Offsets2}.

% add_records(PartitionIndex, Records, Acc = #{topic := Topic, records := Records0}) ->
%     Acc#{records := [{Topic, PartitionIndex, Records} | Records0]}.

add_error(PartitionIndex, Error, Acc = #{topic := Topic, errors := Errors0}) ->
    Acc#{errors := [{Topic, PartitionIndex, Error} | Errors0]}.

% TODO: Errors is [{Topic, Partition, Error}]; Offsets is #{Topic => [{Partition, Offset}]}; that makes them hard to deal with.
handle_offsets_out_of_range(_Errors = [], _Connection, Offsets) ->
    Offsets;
handle_offsets_out_of_range(Errors, Connection, Offsets) ->
    % TODO: We've got three different shapes for this data; that's annoying.
    case
        lists:foldl(
            fun({Topic, PartitionIndex, ?OFFSET_OUT_OF_RANGE}, Acc) ->
                maps:update_with(Topic, fun(Ps) -> [PartitionIndex | Ps] end, [PartitionIndex], Acc)
            end,
            #{},
            Errors
        )
    of
        [] ->
            Offsets;
        OffsetsOutOfRange ->
            % OffsetsOutOfRange is #{Topic => [PartitionIndex]}
            ListOffsetsRequest = build_list_offsets_request(OffsetsOutOfRange),
            % TODO: Use ListOffsetsResponse to set the offsets in StateData, and trigger another fetch.
            % TODO: If the ListOffsets fails, log an error and continue? We'll try again on the next go-round.
            {ok, #{topics := TopicOffsets}} = kafka_connection:call(
                Connection,
                fun list_offsets_request:encode_list_offsets_request_6/1,
                ListOffsetsRequest,
                fun list_offsets_response:decode_list_offsets_response_6/1
            ),

            % ListOffsetsResponse looks like #{topics => [#{name => T, partitions => [#{partition_index => P, offset => O, error_code => ?NONE}]}]}.
            lists:foldl(
                fun(#{name := Topic, partitions := Partitions}, Acc1) ->
                    lists:foldl(
                        fun(
                            #{
                                partition_index := PartitionIndex,
                                offset := Offset,
                                error_code := ?NONE
                            },
                            Acc2
                        ) ->
                            maps:update_with(
                                Topic,
                                fun(PartitionOffsets) ->
                                    lists:keyreplace(
                                        PartitionIndex,
                                        1,
                                        PartitionOffsets,
                                        {PartitionIndex, Offset}
                                    )
                                end,
                                Acc2
                            )
                        end,
                        Acc1,
                        Partitions
                    )
                end,
                Offsets,
                TopicOffsets
            )
    end.

build_list_offsets_request(OffsetsOutOfRange) ->
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
                        % TODO: Is -1 okay here?
                        current_leader_epoch => -1,
                        timestamp => ?EARLIEST_TIMESTAMP
                    }
                 || Partition <- PartitionIndexes
                ]
            }
         || Topic := PartitionIndexes <- OffsetsOutOfRange
        ]
    }.
