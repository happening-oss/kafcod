-module(kafcod_primitives_tagged_field_tests).
-include_lib("eunit/include/eunit.hrl").

-define(unexpected_encode, fun(_, _) -> error(unexpected) end).
-define(ignore_encode, fun(_, _) -> ignore end).

encode_none_test() ->
    ?assertEqual(
        <<0>>,
        iolist_to_binary(
            kafcod_primitives:encode_tagged_fields(?unexpected_encode, #{})
        )
    ).

encode_ignored_test() ->
    ?assertEqual(
        <<0>>,
        iolist_to_binary(
            kafcod_primitives:encode_tagged_fields(?ignore_encode, #{})
        )
    ).

encode_multiple_test() ->
    ?assertEqual(
        iolist_to_binary([
            % Count
            <<2>>,
            % Tag, Length, Value
            [<<1>>, <<8>>, <<0, 0, 0, 0, 0, 0, 0, 1>>],
            [<<2>>, <<8>>, <<0, 0, 0, 0, 0, 0, 0, 2>>]
        ]),
        iolist_to_binary(
            kafcod_primitives:encode_tagged_fields(
                fun
                    (t, T) -> {1, <<T:64/big-signed>>};
                    (u, U) -> {2, <<U:64/big-signed>>};
                    (_, _) -> ignore
                end,
                #{
                    % 'required' in the sense that it's not a tagged field.
                    r => <<"required">>,
                    t => 1,
                    u => 2
                }
            )
        )
    ).

-define(unexpected_decode, fun(_, _, _) -> error(unexpected) end).

decode_none_test() ->
    % Things we've decoded already.
    Acc0 = #{error_code => 0, throttle_time_ms => 0},
    ?assertEqual(
        {Acc0, <<>>}, kafcod_primitives:decode_tagged_fields(?unexpected_decode, Acc0, <<0>>)
    ).

decode_int64_test() ->
    % Things we've decoded already.
    Acc0 = #{error_code => 0, throttle_time_ms => 0},
    TagBuffer = iolist_to_binary([
        % Count
        <<1>>,
        % Tag, Length, Value
        [<<1>>, <<8>>, <<0, 0, 0, 0, 0, 0, 0, 1>>]
    ]),
    ?assertEqual(
        {#{throttle_time_ms => 0, error_code => 0, finalized_features_epoch => 1}, <<>>},
        kafcod_primitives:decode_tagged_fields(
            fun(_Tag = 1, <<Value:64/big>>, Acc) ->
                Acc#{finalized_features_epoch => Value}
            end,
            Acc0,
            TagBuffer
        )
    ).
