-module(kafcod_message_set).
-export([prepare_message_set/2]).

% Per KIP-82, "duplicate headers with the same key must be supported.", so it's a list of KV.
-type headers() :: [{Key :: binary(), Value :: binary() | null}].
-type message() :: #{
    key := binary() | null,
    value := binary() | null,
    headers := headers()
}.

-spec prepare_message_set(kafcod_record_batch:batch_attributes(), [message(), ...]) ->
    [kafcod_record_batch:record_batch()].

prepare_message_set(BatchAttributes, Messages = [_ | _]) when
    is_map(BatchAttributes), is_list(Messages)
->
    DefaultBatchAttributes = #{compression => none},
    BatchAttributes1 = maps:merge(DefaultBatchAttributes, BatchAttributes),
    [
        % TODO: Are there any reasons why we'd *produce* a message set with multiple batches in it?
        prepare_record_batch(BatchAttributes1, Messages)
    ];
prepare_message_set(BatchAttributes, Messages) ->
    error(badarg, [BatchAttributes, Messages]).

-spec prepare_record_batch(kafcod_record_batch:batch_attributes(), [message(), ...]) ->
    kafcod_record_batch:record_batch().

prepare_record_batch(BatchAttributes, Messages = [_ | _]) when is_list(Messages) ->
    % TODO: Records (i.e. the message set) is nullable.
    % TODO: What does the broker actually do with that, and should we support it?
    {Records, LastOffsetDelta} = prepare_records(Messages),
    Timestamp = os:system_time(millisecond),
    #{
        % When producing, we don't know the offset, so it's always zero.
        base_offset => 0,
        % While we _could_ get the partition leader epoch from the metadata, kafire doesn't bother. Is that a problem?
        partition_leader_epoch => 0,
        magic => 2,
        attributes => BatchAttributes,
        last_offset_delta => LastOffsetDelta,
        base_timestamp => Timestamp,
        max_timestamp => Timestamp,
        producer_id => -1,
        producer_epoch => -1,
        base_sequence => -1,
        records => Records
    }.

-spec prepare_records(Messages :: nonempty_list(message())) ->
    {Records :: [kafcod_record:record()], LastOffsetDelta :: non_neg_integer()}.

prepare_records(Messages = [_ | _]) when is_list(Messages) ->
    % When producing, we don't know the offset, so it's always zero.
    BaseOffset = 0,
    {Records, OffsetDelta} = lists:mapfoldl(
        fun prepare_record/2,
        BaseOffset,
        Messages
    ),
    % OffsetDelta is the offset of the *next* record; we want the most-recent, so decrement it.
    {Records, OffsetDelta - 1}.

%% Convert a message -- Key, Value, Headers -- into a record. Called from lists:mapfoldl. Returns the record (for the
%% map) and the next offset delta (for the fold).

-spec prepare_record(Message :: message(), OffsetDelta :: non_neg_integer()) ->
    {Record :: kafcod_record:record(), NextOffsetDelta :: non_neg_integer()}.

prepare_record(_Message = #{key := Key, value := Value, headers := Headers}, OffsetDelta) when
    is_binary(Key) orelse Key =:= null,
    is_binary(Value) orelse Value =:= null,
    is_list(Headers)
->
    {
        #{
            % Record attributes are always zero.
            attributes => 0,
            timestamp_delta => 0,
            offset_delta => OffsetDelta,
            key => Key,
            value => Value,
            headers => Headers
        },
        OffsetDelta + 1
    };
prepare_record(Message, OffsetDelta) ->
    error(badarg, [Message, OffsetDelta]).
