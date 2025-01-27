-module(send_message).
-export([main/1]).

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
            handler => fun send_message/1
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

send_message(#{broker := {Host, Port}}) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, {mode, binary}, {packet, 4}]),

    ClientId = list_to_binary(?MODULE_STRING),

    CorrelationId = 1,

    % We deliberately use an invalid broker ID, so that we _don't_ actually shut down the broker.
    % Request = controlled_shutdown_request:encode_controlled_shutdown_request_0(#{
    %     correlation_id => CorrelationId, client_id => ClientId, broker_id => 900
    % }),

    Request = api_versions_request:encode_api_versions_request_2(#{
        correlation_id => CorrelationId, client_id => ClientId
    }),
    Decoder = fun api_versions_response:decode_api_versions_response_2/1,

    % Request = metadata_request:encode_metadata_request_1(#{
    %     correlation_id => CorrelationId,
    %     client_id => ClientId,
    %     topics => null
    % }),
    % Decoder = fun metadata_response:decode_metadata_response_1/1,

    % Request = metadata_request:encode_metadata_request_9(#{
    %     correlation_id => CorrelationId,
    %     client_id => ClientId,
    %     topics => [],
    %     allow_auto_topic_creation => false,
    %     include_cluster_authorized_operations => true,
    %     include_topic_authorized_operations => true
    % }),
    % Decoder = fun metadata_response:decode_metadata_response_9/1,

    % Request = metadata_request:encode_metadata_request_12(#{
    %     correlation_id => CorrelationId,
    %     client_id => ClientId,
    %     topics => null,
    %     allow_auto_topic_creation => false,
    %     include_topic_authorized_operations => true
    % }),
    % Decoder = fun metadata_response:decode_metadata_response_12/1,

    % Request = find_coordinator_request:encode_find_coordinator_request_3(#{
    %     correlation_id => CorrelationId,
    %     client_id => ClientId,
    %     key_type => 0,
    %     key => <<"a">>
    % }),
    % Decoder = fun find_coordinator_response:decode_find_coordinator_response_3/1,

    % v4 doesn't work: we get error results; also Wireshark doesn't understand it.
    % Request = find_coordinator_request:encode_find_coordinator_request_4(#{
    %     correlation_id => CorrelationId,
    %     client_id => ClientId,
    %     key_type => 0,
    %     coordinator_keys => [<<"group-a">>, <<"group-b">>]
    % }),
    % Decoder = fun find_coordinator_response:decode_find_coordinator_response_4/1,

    % Request = create_topics_request:encode_create_topics_request_2(#{
    %     correlation_id => CorrelationId,
    %     client_id => ClientId,
    %     validate_only => false,
    %     topics => [
    %         #{
    %             name => <<"teams">>,
    %             replication_factor => 3,
    %             num_partitions => 4,
    %             % empty array for automatic assignment
    %             assignments => [],
    %             % also empty, although that's not mentioned in the docs/schema anywhere.
    %             configs => []
    %         }
    %     ],
    %     timeout_ms => 5_000
    % }),
    % Decoder = fun create_topics_response:decode_create_topics_response_2/1,

    gen_tcp:send(Socket, Request),

    loop(Socket, Decoder, <<>>),
    erlang:halt(0).

loop(Socket, Decoder, Buffer) ->
    receive
        {tcp, _, Bytes} ->
            parse_buffer(Decoder, <<Buffer/binary, Bytes/binary>>),
            loop(Socket, Decoder, <<>>);
        M ->
            error(M)
    end.

parse_buffer(Decoder, Buffer) ->
    io:format("~p~n", [Buffer]),
    io:format("~p~n", [Decoder(Buffer)]).
