-module(kafcod_errors).
-export([create_error_info/2]).
-export([format_error/2]).
-include("guards.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec create_error_info(Arg :: map(), Expected :: #{Key := Type}) ->
    {error_info, #{module => module(), function => atom(), cause => term()}}
when
    Key :: atom(), Type :: expected_type().

create_error_info(Arg, Expected) ->
    ErrorInfo = #{module => ?MODULE, cause => get_errors(Arg, Expected)},
    {error_info, ErrorInfo}.

-type expected_type() ::
    int8
    | int16
    | int32
    | int64
    | varint
    | bool
    | string
    | nullable_string
    | {array, any()}
    | {nullable_array, any()}
    | map
    | bytes
    | nullable_bytes
    | records
    | nullable_records
    | uuid.

% Erlang types (vaguely).
-type guessed_type() ::
    array
    | atom
    | bool
    | charlist
    | integer
    | map
    | null
    | tuple
    | todo.

-type error() ::
    expected_map
    | {missing, Key :: atom(), ExpectedType :: expected_type()}
    | {wrong_type, Key :: atom(), {expected, ExpectedType :: expected_type()}, {value, term()}}.

-spec get_errors(Arg :: map(), Expected :: #{Key := Type}) -> [error()] when
    Key :: atom(), Type :: expected_type().

get_errors(Arg, Expected) when is_map(Arg) ->
    Errors = maps:fold(
        fun(Key, ExpectedType, Acc) ->
            case maps:find(Key, Arg) of
                error ->
                    [{missing, Key, ExpectedType} | Acc];
                {ok, Value} ->
                    case value_conforms_to_type(Value, ExpectedType) of
                        true ->
                            Acc;
                        false ->
                            [
                                {wrong_type, Key, {expected, ExpectedType}, {value, Value}}
                                | Acc
                            ]
                    end
            end
        end,
        [],
        Expected
    ),
    sort_errors(Errors);
get_errors(_Arg, _Expected) ->
    [expected_map].

-spec sort_errors([error()]) -> [error()].

sort_errors(Errors) ->
    % Order alphabetically by field name, regardless of error class.
    Order = fun
        ({wrong_type, A, _, _}, {wrong_type, B, _, _}) -> A =< B;
        ({wrong_type, A, _, _}, {missing, B, _}) -> A =< B;
        ({missing, A, _}, {wrong_type, B, _, _}) -> A =< B;
        ({missing, A, _}, {missing, B, _}) -> A =< B;
        (A, B) -> A =< B
    end,
    lists:sort(Order, Errors).

-spec format_error(Reason, StackTrace) -> ErrorMap when
    Reason :: term(),
    StackTrace :: erlang:stacktrace(),
    ErrorMap :: #{pos_integer() => unicode:chardata()}.

format_error(_Reason, _StackTrace = [{_M, _F, [_Arg], Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    Errors = maps:get(cause, ErrorInfo, #{}),
    #{
        1 => format_errors(Errors)
    }.

-spec format_errors([error()]) -> unicode:chardata().

format_errors(Errors) ->
    lists:flatten(
        lists:join(
            "\n",
            lists:map(fun format_problem/1, Errors)
        )
    ).

-spec format_problem(error()) -> iodata().

format_problem(expected_map) ->
    "expected a map";
format_problem({missing, Key, {array, ElementType}}) ->
    io_lib:format("missing '~s' (array of ~s)", [Key, ElementType]);
format_problem({missing, Key, {nullable_array, ElementType}}) ->
    io_lib:format("missing '~s' (nullable_array of ~s)", [Key, ElementType]);
format_problem({missing, Key, ExpectedType}) ->
    io_lib:format("missing '~s' (~s)", [Key, ExpectedType]);
format_problem({wrong_type, Key, {expected, {array, ExpectedType}}, {value, Value}}) ->
    io_lib:format(
        "expected '~s' to be an array of '~s', but has type '~s', value ~p", [
            Key, ExpectedType, guess_actual_type(ExpectedType, Value), Value
        ]
    );
format_problem({wrong_type, Key, {expected, {nullable_array, ExpectedType}}, {value, Value}}) ->
    io_lib:format(
        "expected '~s' to be a nullable_array of '~s', but has type '~s', value ~p", [
            Key, ExpectedType, guess_actual_type(ExpectedType, Value), Value
        ]
    );
format_problem({wrong_type, Key, {expected, ExpectedType}, {value, Value}}) ->
    io_lib:format(
        "expected '~s' to be of type '~s', but has type '~s', value ~p", [
            Key, ExpectedType, guess_actual_type(ExpectedType, Value), Value
        ]
    ).

% guess_actual_type also takes the expected type, so that we can tell the difference between a list intended as an
% array, and a charlist intended as a string.
% TODO: This is not exhaustive.
-spec guess_actual_type(ExpectedType, Value) -> guessed_type() when
    ExpectedType :: expected_type(), Value :: term().

guess_actual_type(_, true) -> bool;
guess_actual_type(_, false) -> bool;
guess_actual_type(_, null) -> null;
guess_actual_type(string, Value) when is_list(Value) -> charlist;
guess_actual_type(_, Value) when is_list(Value) -> array;
guess_actual_type(_, Value) when is_map(Value) -> map;
guess_actual_type(_, Value) when is_atom(Value) -> atom;
guess_actual_type(_, Value) when is_tuple(Value) -> tuple;
% TODO: guess at the intended int size?
guess_actual_type(_, Value) when is_integer(Value) -> integer;
guess_actual_type(_, _) -> todo.

% TODO: Improve error reporting for arrays of complex types, similar to the above. See create_topics_request_N (which also has the timeoutMs thing going on)
% TODO: Improve error reporting for arrays of primitives if you use the wrong type. See encode_delete_groups_request_0

% TODO: Add types; check ranges.
-spec value_conforms_to_type(Value, ExpectedType) -> boolean() when
    Value :: term(), ExpectedType :: expected_type().

value_conforms_to_type(Value, varint) when is_integer(Value) -> true;
value_conforms_to_type(Value, int8) when ?is_int8(Value) -> true;
value_conforms_to_type(Value, int16) when ?is_int16(Value) -> true;
value_conforms_to_type(Value, int32) when ?is_int32(Value) -> true;
value_conforms_to_type(Value, int64) when ?is_int64(Value) -> true;
value_conforms_to_type(Value, string) when ?is_string(Value) -> true;
value_conforms_to_type(Value, nullable_string) when ?is_nullable_string(Value) -> true;
value_conforms_to_type(Value, bytes) when ?is_bytes(Value) -> true;
value_conforms_to_type(Value, nullable_bytes) when ?is_nullable_bytes(Value) -> true;
value_conforms_to_type(Value, {array, _}) when ?is_array(Value) -> true;
value_conforms_to_type(Value, {nullable_array, _}) when ?is_nullable_array(Value) -> true;
value_conforms_to_type(Value, map) when is_map(Value) -> true;
value_conforms_to_type(false, bool) -> true;
value_conforms_to_type(true, bool) -> true;
value_conforms_to_type(_Value, _Type) -> false.

-ifdef(TEST).
get_errors_missing_test_() ->
    [
        ?_assertEqual(
            [{missing, a, int32}], get_errors(#{b => <<"x">>}, #{a => int32, b => string})
        ),
        ?_assertEqual([{missing, b, string}], get_errors(#{a => 12}, #{a => int32, b => string}))
    ].

get_errors_type_test_() ->
    [
        ?_assertEqual(
            [{wrong_type, b, {expected, string}, {value, false}}],
            get_errors(#{a => 1, b => false}, #{a => int32, b => string})
        ),
        ?_assertEqual(
            [],
            get_errors(
                #{max_timestamp => 1687522502868, attributes => 0},
                #{max_timestamp => int64, attributes => int16}
            )
        )
    ].

get_errors_both_test_() ->
    [
        ?_assertEqual(
            [
                {wrong_type, client_id, {expected, string}, {value, 43}},
                {missing, coordinator_keys, {array, string}},
                {missing, key_type, int8}
            ],
            get_errors(#{client_id => 43, correlation_id => 12}, #{
                correlation_id => int32,
                client_id => string,
                key_type => int8,
                coordinator_keys => {array, string}
            })
        )
    ].

format_errors_test_() ->
    [
        ?_assertEqual(
            "expected 'assignments' to be of type 'array', but has type 'null', value null\n"
            "expected 'configs' to be of type 'array', but has type 'null', value null",
            format_errors([
                {wrong_type, assignments, {expected, array}, {value, null}},
                {wrong_type, configs, {expected, array}, {value, null}}
            ])
        ),
        ?_assertEqual("expected a map", format_errors([expected_map])),
        ?_assertEqual(
            "missing 'assignments' (array)\n"
            "missing 'configs' (array)\n"
            "missing 'name' (string)\n"
            "missing 'num_partitions' (int32)\n"
            "missing 'replication_factor' (int16)",
            format_errors([
                {missing, assignments, array},
                {missing, configs, array},
                {missing, name, string},
                {missing, num_partitions, int32},
                {missing, replication_factor, int16}
            ])
        ),
        ?_assertEqual(
            "missing 'key_type' (int8)\n"
            "missing 'key' (string)",
            format_errors([{missing, key_type, int8}, {missing, key, string}])
        ),
        ?_assertEqual(
            "expected 'key' to be of type 'string', but has type 'integer', value 1234",
            format_errors([{wrong_type, key, {expected, string}, {value, 1234}}])
        ),
        ?_assertEqual(
            "expected 'key_type' to be of type 'int8', but has type 'integer', value 128",
            format_errors([{wrong_type, key_type, {expected, int8}, {value, 128}}])
        ),
        ?_assertEqual(
            "missing 'key_type' (int8)\n"
            "missing 'coordinator_keys' (array of string)",
            format_errors([{missing, key_type, int8}, {missing, coordinator_keys, {array, string}}])
        ),
        ?_assertEqual(
            "expected 'client_id' to be of type 'nullable_string', but has type 'atom', value aaa",
            format_errors([{wrong_type, client_id, {expected, nullable_string}, {value, aaa}}])
        ),
        ?_assertEqual(
            "missing 'include_cluster_authorized_operations' (bool)",
            format_errors([{missing, include_cluster_authorized_operations, bool}])
        )
    ].
-endif.
