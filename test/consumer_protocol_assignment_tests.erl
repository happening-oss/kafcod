-module(consumer_protocol_assignment_tests).
-include_lib("eunit/include/eunit.hrl").

% See also kafcod_consumer_protocol_assignment_tests.erl

v0_assignment_encode_test() ->
    Version = 0,
    ?assertEqual(
        iolist_to_binary([
            <<0, 0>>,
            [
                <<0, 0, 0, 2>>,
                [
                    [
                        [<<0, 7>>, <<"topic-a">>],
                        [
                            <<0, 0, 0, 4>>,
                            [<<0, 0, 0, 0>>, <<0, 0, 0, 1>>, <<0, 0, 0, 2>>, <<0, 0, 0, 3>>]
                        ]
                    ],
                    [
                        [<<0, 7>>, <<"topic-b">>],
                        [
                            <<0, 0, 0, 4>>,
                            [<<0, 0, 0, 0>>, <<0, 0, 0, 1>>, <<0, 0, 0, 2>>, <<0, 0, 0, 3>>]
                        ]
                    ]
                ]
            ],
            [<<0, 0, 0, 5>>, <<"hello">>]
        ]),
        iolist_to_binary([
            <<Version:16/big-signed>>,
            consumer_protocol_assignment:encode_consumer_protocol_assignment_0(#{
                assigned_partitions => [
                    #{topic => <<"topic-a">>, partitions => [0, 1, 2, 3]},
                    #{topic => <<"topic-b">>, partitions => [0, 1, 2, 3]}
                ],
                user_data => <<"hello">>
            })
        ])
    ).
