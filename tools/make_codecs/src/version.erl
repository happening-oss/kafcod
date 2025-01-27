-module(version).
-export([version_seq/1, fields_for_version/2, parse_version_range/1, is_version_in_range/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

version_seq({From, To}) when is_integer(From), is_integer(To), From =< To ->
    lists:seq(From, To).

fields_for_version(Version, Fields) ->
    lists:filter(
        fun(Field) ->
            FieldVersions = parse_version_range(maps:get(<<"versions">>, Field)),
            is_version_in_range(Version, FieldVersions)
        end,
        Fields
    ).

%% Convert <<"3+">> to {3, infinity} and <<"8-10">> to {8, 10} and so on.
parse_version_range(Range) ->
    % Start of line, then one of ("none" | number only | number range | number+), followed by end of line
    RE = <<"^((none)|(\\d+)|(\\d+)-(\\d+)|(\\d+)\\+)$">>,
    case re:run(Range, RE, [{capture, all, binary}]) of
        _None = {match, [_, _, <<"none">>]} ->
            none;
        _Only = {match, [_, _, <<>>, Only]} ->
            {binary_to_integer(Only), binary_to_integer(Only)};
        _Range = {match, [_, _, <<>>, <<>>, From, To]} ->
            {binary_to_integer(From), binary_to_integer(To)};
        _From = {match, [_, _, <<>>, <<>>, <<>>, <<>>, From]} ->
            {binary_to_integer(From), infinity};
        nomatch ->
            error({nomatch, Range})
    end.

% integers are always =< atoms, so we don't _need_ the two clauses, but we'll be explicit about it anyway.
is_version_in_range(Version, {From, infinity}) -> Version >= From;
is_version_in_range(Version, {From, To}) -> Version >= From andalso Version =< To;
is_version_in_range(_Version, none) -> false.

-ifdef(TEST).
parse_version_range_test_() ->
    [
        ?_assertEqual({1, 1}, parse_version_range(<<"1">>)),
        ?_assertEqual({0, infinity}, parse_version_range(<<"0+">>)),
        ?_assertEqual({2, 4}, parse_version_range(<<"2-4">>)),
        ?_assertEqual(none, parse_version_range(<<"none">>))
    ].

is_version_in_range_bounded_test_() ->
    [
        ?_assertNot(is_version_in_range(0, {1, 3})),
        ?_assert(is_version_in_range(1, {1, 3})),
        ?_assert(is_version_in_range(2, {1, 3})),
        ?_assert(is_version_in_range(2, {1, 3})),
        ?_assertNot(is_version_in_range(4, {1, 3}))
    ].

is_version_in_range_open_test_() ->
    [
        ?_assertNot(is_version_in_range(0, {1, infinity})),
        ?_assert(is_version_in_range(1, {1, infinity})),
        ?_assert(is_version_in_range(2, {1, infinity}))
    ].

is_version_in_range_none_test_() ->
    [
        ?_assertNot(is_version_in_range(0, none)),
        ?_assertNot(is_version_in_range(1, none))
    ].
-endif.
