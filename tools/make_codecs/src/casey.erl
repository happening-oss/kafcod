-module(casey).
-export([underscore/1, title/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% More-or-less directly translated from Elixir's Macro.underscore. Didn't bother with the . -> / transformation, though.
underscore(<<H, T/binary>>) ->
    iolist_to_binary([to_lower_char(H), do_underscore(T, H)]).

do_underscore(<<H, T, Rest/binary>>, _) when H >= $A andalso H =< $Z andalso not (T >= $A andalso T =< $Z) andalso not (T >= $0 andalso T =< $9) andalso T =/= $_ ->
    [$_, to_lower_char(H), T, do_underscore(Rest, T)];
do_underscore(<<H, T/binary>>, Prev) when H >= $A andalso H =< $Z andalso not (Prev >= $A andalso Prev =< $Z) andalso Prev =/= $_ ->
    [$_, to_lower_char(H), do_underscore(T, H)];
do_underscore(<<H, T/binary>>, _) ->
    [to_lower_char(H), do_underscore(T, H)];
do_underscore(<<>>, _) ->
    <<>>.

 %title(""), do: ""
title(<<$_, T/binary>>) -> title(T);
title(<<H, T/binary>>) -> iolist_to_binary([to_upper_char(H), do_title(T)]).

do_title(<<$_, $_, T/binary>>) -> do_title(<<$_, T/binary>>);

do_title(<<$_, H, T/binary>>) when H >= $a andalso H =< $z -> [to_upper_char(H), do_title(T)];

do_title(<<$_, H, T/binary>>) when H >= $0 andalso H =< $9 -> [H, do_title(T)];
do_title(<<$_>>) -> <<>>;
do_title(<<H, T/binary>>) -> [H, do_title(T)];
do_title(<<>>) -> <<>>.

to_lower_char(Ch) when Ch >= $A andalso Ch =< $Z -> Ch + 32;
to_lower_char(Ch) -> Ch.

to_upper_char(Ch) when Ch >= $a andalso Ch =< $z -> Ch - 32;
to_upper_char(Ch) -> Ch.

-ifdef(TEST).
underscore_test_() ->
    [
        ?_assertEqual(<<"single">>, underscore(<<"Single">>)),
        ?_assertEqual(<<"lower">>, underscore(<<"lower">>)),
        ?_assertEqual(<<"upper_camel">>, underscore(<<"UpperCamel">>)),
        ?_assertEqual(<<"lower_camel">>, underscore(<<"lowerCamel">>)),
        ?_assertEqual(<<"upper_bactrian_camel">>, underscore(<<"UpperBactrianCamel">>))
    ].

title_test_() ->
    [
        ?_assertEqual(<<"UpperCamel">>, title(<<"UpperCamel">>)),
        ?_assertEqual(<<"LowerCamel">>, title(<<"lowerCamel">>))
    ].
-endif.
