-module(kafcod_record_batch_tests).
-include_lib("eunit/include/eunit.hrl").

% Tests for non-null keys and for headers are in fetch_response_tests.

decode_record_batch_test() ->
    % The real capture is a single binary; I've broken it into an iolist in the test so that it can be line-wrapped and
    % nested and commented for readability without reformatting the code putting it all back into one blob.
    CapturedRecordBatch = iolist_to_binary([
        % baseOffset: int64
        <<0, 0, 0, 0, 0, 0, 0, 0>>,
        % batchLength: int32, in bytes, from immediately after batchLength to the end of the records.
        <<0, 0, 0, 61>>,
        % partitionLeaderEpoch: int32
        <<0, 0, 0, 0>>,
        % magic: int8 (current magic value is 2)
        <<2>>,
        % crc: int32, CRC-32C (Castagnoli)
        <<83, 104, 36, 131>>,
        % attributes: int16
        <<0, 0>>,
        % lastOffsetDelta: int32
        <<0, 0, 0, 0>>,
        % baseTimestamp, maxTimestamp: int64, Thu Jun 15 2023 12:33:52 GMT+0000
        <<0, 0, 1, 136, 191, 12, 70, 86>>,
        <<0, 0, 1, 136, 191, 12, 70, 86>>,
        % producerId: int64, -1
        <<255, 255, 255, 255, 255, 255, 255, 255>>,
        % producerEpoch: int16, -1
        <<255, 255>>,
        % baseSequence: int32, -1
        <<255, 255, 255, 255>>,
        % records: [Record]
        [
            % count: int32
            <<0, 0, 0, 1>>,
            [
                % record: Record
                <<22, 0, 0, 0, 1, 10, 104, 101, 108, 108, 111, 0>>
            ]
        ]
    ]),
    ?assertEqual(
        {
            #{
                attributes => #{compression => none},
                base_offset => 0,
                base_sequence => -1,
                base_timestamp => 1686832432726,
                crc => 1399334019,
                last_offset_delta => 0,
                magic => 2,
                max_timestamp => 1686832432726,
                partition_leader_epoch => 0,
                producer_epoch => -1,
                producer_id => -1,
                records => [
                    #{
                        attributes => 0,
                        headers => [],
                        key => null,
                        offset_delta => 0,
                        timestamp_delta => 0,
                        value => <<"hello">>
                    }
                ]
            },
            <<>>
        },
        kafcod_record_batch:decode_record_batch(CapturedRecordBatch)
    ).

records_multiple_test_() ->
    Encoded = iolist_to_binary(
        [
            [
                <<22, 0, 0, 0, 1, 10, 104, 101, 108, 108, 111, 0>>,
                <<22, 0, 0, 2, 1, 10, 106, 101, 108, 108, 111, 0>>,
                <<22, 0, 0, 4, 1, 10, 121, 101, 108, 108, 111, 0>>
            ]
        ]
    ),
    Decoded = [
        #{
            attributes => 0,
            headers => [],
            key => null,
            offset_delta => 0,
            timestamp_delta => 0,
            value => <<"hello">>
        },
        #{
            attributes => 0,
            headers => [],
            key => null,
            offset_delta => 1,
            timestamp_delta => 0,
            value => <<"jello">>
        },
        #{
            attributes => 0,
            headers => [],
            key => null,
            offset_delta => 2,
            timestamp_delta => 0,
            value => <<"yello">>
        }
    ],
    [
        % Slightly confusing: we need to call decompress_records/2, rather than decode_records/2, because the record
        % count comes before the compressed records. This makes it hard to test directly.
        ?_assertEqual(Decoded, kafcod_record_batch:decode_records(3, Encoded)),

        % We _can_ call encode_records/1 directly, though for symmetry, we should probably call compress_records/2.
        ?_assertEqual(Encoded, iolist_to_binary(kafcod_record_batch:encode_records(Decoded)))
    ].

encode_record_batch_error_test() ->
    % We're missing 'producer_id', so we expect that to be reported.
    %
    % But we were also seeing the following:
    %   "expected 'attributes' to be of type 'map', but has type 'map', value #{compression => none}"
    % ...which doesn't make any sense.
    %
    % This test guards against that.
    Catch =
        catch kafcod_record_batch:encode_record_batch(#{
            attributes => #{compression => none},
            % missing producer_id
            producer_epoch => -1,
            max_timestamp => 0,
            records =>
                [
                    #{
                        attributes => 0,
                        value => <<"value-1">>,
                        key => <<"key-1">>,
                        headers => [],
                        offset_delta => 0,
                        timestamp_delta => 0
                    },
                    #{
                        attributes => 0,
                        value => <<"value-2">>,
                        key => <<"key-2">>,
                        headers => [],
                        offset_delta => 1,
                        timestamp_delta => 0
                    }
                ],
            base_offset => 0,
            base_sequence => -1,
            base_timestamp => 0,
            crc => -1,
            last_offset_delta => 2,
            magic => 2,
            partition_leader_epoch => 0
        }),
    {'EXIT', {Reason = badarg, StackTrace}} = Catch,
    ?assertEqual(
        #{1 => "missing 'producer_id' (int64)"}, kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.

crc_nif_enabled_test() ->
    % This checks that you didn't accidentally leave the Erlang CRC-32 enabled.
    {module, _} = code:load_file(kafcod_crc32c),
    {module, _} = code:load_file(kafcod_crc32c_erl),

    % And we do that by tracing calls to the two modules.
    erlang:trace(all, true, [call]),
    erlang:trace_pattern({kafcod_crc32c_erl, '_', '_'}, true),
    erlang:trace_pattern({kafcod_crc32c, '_', '_'}, true),

    % We need to do the calls in a separate process.
    Self = self(),
    spawn(fun() ->
        _ = kafcod_record_batch:encode_record_batch(#{
            attributes => #{compression => none},
            base_offset => 0,
            base_sequence => -1,
            base_timestamp => 0,
            last_offset_delta => 0,
            magic => 2,
            max_timestamp => 0,
            partition_leader_epoch => -1,
            producer_epoch => -1,
            producer_id => -1,
            records => []
        }),
        Self ! done
    end),
    receive
        done -> ok
    end,

    % Turn tracing off; gather the trace messages.
    erlang:trace(all, false, [call]),
    Trace = flush(),

    % If either of these assertions fails, you've probably got the CRC macro (in kafcod_record_batch.erl) pointing to
    % the wrong implementation. Make sure that it's pointing to the NIF implementation.
    ?assertNotMatch(
        [{trace, _, call, {kafcod_crc32c_erl, value, _}}],
        Trace,
        "unexpected call to kafcod_crc32_erl:value/1"
    ),
    ?assertMatch(
        [{trace, _, call, {kafcod_crc32c, value, _}}],
        Trace,
        "expected call to kafcod_crc32:value/1"
    ),
    ok.

flush() ->
    flush([]).

flush(Acc) ->
    receive
        M ->
            flush([M | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.
