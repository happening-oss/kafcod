-module(kafcod_consumer_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

encode_decode_assignments_test_() ->
    Assignment1 = #{
        assigned_partitions => #{<<"a">> => [0, 1], <<"b">> => [0, 1]}, user_data => <<"userdata1">>
    },
    Assignment2 = #{
        assigned_partitions => #{<<"a">> => [2, 3], <<"b">> => [2, 3]}, user_data => <<"userdata2">>
    },
    Assignments = #{
        <<"m1">> => Assignment1,
        <<"m2">> => Assignment2
    },
    EncodedAssignment2 = iolist_to_binary([
        % Version
        <<0, 0>>,
        [
            % 2 topics
            <<0, 0, 0, 2>>,
            [
                % Note: encoded in reverse; implementation detail.
                [
                    % "b": [2, 3]
                    [<<0, 1>>, <<"b">>],
                    <<0, 0, 0, 2>>,
                    [
                        <<0, 0, 0, 2>>,
                        <<0, 0, 0, 3>>
                    ]
                ],
                [
                    % "a": [2, 3]
                    [<<0, 1>>, <<"a">>],
                    <<0, 0, 0, 2>>,
                    [
                        <<0, 0, 0, 2>>,
                        <<0, 0, 0, 3>>
                    ]
                ]
            ],
            % "userdata2"
            [<<0, 0, 0, 9>>, <<"userdata2">>]
        ]
    ]),
    Encoded2 = #{
        assignment => EncodedAssignment2,
        member_id => <<"m2">>
    },
    Encoded1 = #{
        assignment =>
            iolist_to_binary([
                <<0, 0, 0, 0, 0, 2, 0, 1, 98, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 97, 0, 0, 0,
                    2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 9, 117, 115, 101, 114, 100, 97, 116, 97,
                    49>>
            ]),
        member_id => <<"m1">>
    },

    % kafcod_consumer_protocol:encode_assignments/1 reverses the order because it uses maps:fold(); this is an
    % implementation detail and doesn't matter to the broker.
    EncodedAssignments = [Encoded2, Encoded1],

    % There's an asymmetry:
    % - The leader encodes multiple assignments and sends the result to the broker.
    % - The broker sends each single assignment to the followers.
    [
        {"encode multiple assignments",
            ?_assertEqual(
                EncodedAssignments, kafcod_consumer_protocol:encode_assignments(Assignments)
            )},
        {"decode single assignment",
            ?_assertEqual(
                Assignment2, kafcod_consumer_protocol:decode_assignment(EncodedAssignment2)
            )}
    ].
