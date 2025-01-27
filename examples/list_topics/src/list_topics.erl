-module(list_topics).
-export([main/1]).

-define(CLIENT_ID, atom_to_binary(?MODULE)).

main(Args) ->
    argparse:run(
        Args,
        #{
            arguments => [
                #{
                    name => broker,
                    help => "Bootstrap broker (host[:port])",
                    type => {custom, fun parse_broker/1}
                }
            ],
            handler => fun list_topics/1
        },
        #{progname => ?MODULE}
    ).

-define(DEFAULT_BROKER_PORT, 9092).

parse_broker(Arg) when is_list(Arg) ->
    case string:split(Arg, ":") of
        [Host, Port] ->
            {Host, list_to_integer(Port)};
        [Host] ->
            {Host, ?DEFAULT_BROKER_PORT};
        _ ->
            error(badarg)
    end.

list_topics(#{broker := {Host, Port}}) ->
    process_flag(trap_exit, true),
    {ok, Connection} = kafka_connection:start_link(Host, Port, ?CLIENT_ID),
    {ok, Metadata} = kafka_connection:call(
        Connection,
        fun metadata_request:encode_metadata_request_9/1,
        #{
            topics => null,
            include_topic_authorized_operations => false,
            include_cluster_authorized_operations => false,
            allow_auto_topic_creation => false
        },
        fun metadata_response:decode_metadata_response_9/1
    ),
    #{topics := Topics} = Metadata,
    TopicNames = [Name || #{error_code := 0, name := Name} <- Topics],

    {ok, #{results := Results}} = kafka_connection:call(
        Connection,
        fun describe_configs_request:encode_describe_configs_request_0/1,
        #{
            resources => [
                #{
                    resource_name => TopicName,
                    resource_type => 2,
                    configuration_keys => null
                }
             || TopicName <- TopicNames
            ]
        },
        fun describe_configs_response:decode_describe_configs_response_0/1
    ),

    lists:foreach(
        fun(#{resource_name := Topic, configs := Configs, error_code := 0}) ->
            io:format("~s~n", [Topic]),
            lists:foreach(
                fun(#{name := Name, value := Value}) ->
                    io:format("  ~s = ~s~n", [Name, Value])
                end,
                lists:sort(fun(#{name := A}, #{name := B}) -> A =< B end, Configs)
            )
        end,
        lists:sort(fun(#{resource_name := A}, #{resource_name := B}) -> A =< B end, Results)
    ),
    ok.
