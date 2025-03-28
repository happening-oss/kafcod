-module(reassign_cli).
-export([cli/0]).

cli() ->
    #{
        arguments => [
            #{
                name => bootstrap,
                help => "Bootstrap broker(s) (host[:port])",
                long => "-bootstrap",
                required => true,
                type => {custom, fun parse_broker/1}
            }
        ],
        commands => #{
            "list-partition-reassignments" => #{
                handler => fun reassign:list_partition_reassignments/1
            },
            "reassign-partitions" => #{
                handler => fun reassign:reassign_partitions/1,
                arguments => [
                    #{
                        name => topic,
                        long => "-topic",
                        required => true,
                        type => binary
                    },

                    #{
                        name => partition,
                        long => "-partition",
                        required => false,
                        nargs => nonempty_list,
                        action => extend,
                        type => {integer, [{min, 0}]}
                    },

                    #{
                        name => to,
                        help => "Broker(s) to move to (node ID)",
                        long => "-to",
                        required => true,
                        nargs => nonempty_list,
                        action => extend,
                        type => {integer, [{min, 0}]}
                    },

                    #{
                        name => pick,
                        help => "Pick N of the given brokers",
                        long => "-pick",
                        required => false,
                        type => {integer, [{min, 0}]}
                    },

                    #{
                        name => shuffle,
                        help => "Shuffle the destination brokers?",
                        long => "-shuffle",
                        required => false,
                        type => boolean,
                        default => false
                    },

                    #{
                        name => keep_leader,
                        help => "Keep the leader",
                        long => "-keep-leader",
                        required => false,
                        type => boolean,
                        default => false
                    },

                    #{
                        name => rack_aware,
                        help => "Rack aware",
                        long => "-rack-aware",
                        required => false,
                        type => boolean,
                        default => false
                    }
                ]
            },
            "elect-leaders" => #{
                handler => fun reassign:elect_leaders/1,
                arguments => [
                    #{
                        name => topic,
                        long => "-topic",
                        required => true,
                        type => binary
                    },

                    #{
                        name => partition,
                        long => "-partition",
                        required => false,
                        nargs => nonempty_list,
                        action => extend,
                        type => {integer, [{min, 0}]}
                    }
                ]
            },
            "validate-replication" => #{
                handler => fun reassign:validate_replication/1,
                arguments => [
                    #{
                        name => pick,
                        help => "Pick N of the given brokers",
                        long => "-pick",
                        required => true,
                        type => {integer, [{min, 0}]}
                    },

                    #{
                        name => rack_aware,
                        help => "Rack aware",
                        long => "-rack-aware",
                        required => false,
                        type => boolean,
                        default => false
                    }
                ]
            }
        }
    }.

-define(DEFAULT_BROKER_PORT, 9092).

parse_broker(Arg) when is_list(Arg) ->
    case string:split(Arg, ":") of
        [Host, Port] ->
            #{host => list_to_binary(Host), port => list_to_integer(Port)};
        [Host] ->
            #{host => list_to_binary(Host), port => ?DEFAULT_BROKER_PORT};
        _ ->
            error(badarg)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
args_from(Command) ->
    [_ | Args] = string:split(Command, " ", all),
    Args.

cli_test_() ->
    [
        {"bootstrap broker",
            ?_assertMatch(
                {ok, #{bootstrap := #{host := <<"localhost">>, port := 9093}}, _, _},
                argparse:parse(
                    args_from(
                        "./reassign list-partition-reassignments --bootstrap localhost:9093"
                    ),
                    cli(),
                    #{progname => ?MODULE}
                )
            )},

        {"bootstrap broker default port",
            ?_assertMatch(
                {ok, #{bootstrap := #{host := <<"localhost">>, port := 9092}}, _, _},
                argparse:parse(
                    args_from(
                        "./reassign list-partition-reassignments --bootstrap localhost"
                    ),
                    cli(),
                    #{progname => ?MODULE}
                )
            )},

        {"move partitions (when replacing a broker)",
            ?_assertMatch(
                {ok,
                    #{
                        bootstrap := #{host := <<"localhost">>, port := 9092},
                        topic := <<"topic">>,
                        partition := [0, 1, 2],
                        to := [104],
                        shuffle := false
                    },
                    _, _},
                argparse:parse(
                    args_from(
                        "./reassign reassign-partitions --bootstrap localhost --topic topic --partition 0 1 2 --to 104"
                    ),
                    cli(),
                    #{progname => ?MODULE}
                )
            )}
    ].
-endif.
