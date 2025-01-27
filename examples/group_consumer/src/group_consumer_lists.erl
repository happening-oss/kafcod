-module(group_consumer_lists).
-export([
    share/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Given a list of items, divide it into N lists of items. The resulting lists should try to be the same length.
% Extra items are given to the first lists in the result.
% Note that this is NOT equivalent to Elixir's Enum.chunk_every; see the comment below.
share(List, N) when is_list(List), N > 0 ->
    L = length(List),
    % How many items in each list, minimum?
    D = L div N,
    % How many left over?
    R = L rem N,

    lists:reverse(share(List, N, D, R, [])).

share([], _N = 0, _D, _R, Acc) ->
    Acc;
share(List, N, D, R, Acc) when R > 0 ->
    % While we have spares, take an extra one.
    {Take, Rest} = lists:split(D + 1, List),
    share(Rest, N - 1, D, R - 1, [Take | Acc]);
share(List, N, D, R = 0, Acc) ->
    % No spares; take exact amount.
    {Take, Rest} = lists:split(D, List),
    share(Rest, N - 1, D, R, [Take | Acc]).

-ifdef(TEST).
share_test() ->
    % With only one member, it should get everything.
    ?assertEqual([[1, 2]], share([1, 2], 1)),
    ?assertEqual([[1, 2, 3]], share([1, 2, 3], 1)),

    % Spare items are allocated to the earlier lists first.
    ?assertEqual([[1], [2]], share([1, 2], 2)),
    ?assertEqual([[1, 2], [3]], share([1, 2, 3], 2)),

    % If there are not enough items, we expect some empty lists.
    ?assertEqual([[1], [2], []], share([1, 2], 3)),
    ?assertEqual([[1], [2], [], []], share([1, 2], 4)),
    ?assertEqual([[1], [2], [3], []], share([1, 2, 3], 4)),

    ?assertEqual([[1], [2], [3]], share([1, 2, 3], 3)),

    % More examples of too many items; those should be given (one at a time) to the earlier lists first.
    ?assertEqual([[1, 2], [3, 4], [5, 6]], share(lists:seq(1, 6), 3)),
    ?assertEqual([[1, 2, 3], [4, 5], [6, 7]], share(lists:seq(1, 7), 3)),
    ?assertEqual([[1, 2, 3], [4, 5, 6], [7, 8]], share(lists:seq(1, 8), 3)),
    ?assertEqual([[1, 2, 3], [4, 5, 6], [7, 8, 9]], share(lists:seq(1, 9), 3)),

    % Note that Elixir's Enum.chunk_every/2 would do the following, which is not what we want:
    % assert Enum.chunk_every(1..7, div(7, 3) + 1) == [[1, 2, 3], [4, 5, 6], [7]]
    ok.

-endif.
