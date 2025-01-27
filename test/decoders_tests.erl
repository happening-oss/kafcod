-module(decoders_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/decoders.hrl").

decode_bool(Input) ->
    % Because _decode_bool is an unhygienic macro, we can't easily use it in the assert macros, so we have to wrap it in
    % a function.
    ?_decode_bool(V, Input, R),
    {V, R}.

decode_array(Input, Fun) ->
    % Ditto, but for arrays.
    ?_decode_array(V, Input, R, Fun),
    {V, R}.

bool_test_() ->
    [
        ?_assertEqual({true, <<>>}, decode_bool(<<1:8/big>>)),
        ?_assertEqual({false, <<>>}, decode_bool(<<0:8/big>>)),
        ?_assertEqual({true, <<"abc">>}, decode_bool(<<1:8/big, $a, $b, $c>>)),
        ?_assertEqual({false, <<"abc">>}, decode_bool(<<0:8/big, $a, $b, $c>>))
    ].

int32_array_test_() ->
    [
        ?_assertEqual(
            {[200, 300, 100], <<>>},
            decode_array(<<0, 0, 0, 3, 0, 0, 0, 200, 0, 0, 1, 44, 0, 0, 0, 100>>, ?decode_int32_)
        )
    ].
