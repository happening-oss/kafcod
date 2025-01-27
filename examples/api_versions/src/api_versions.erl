-module(api_versions).
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
            handler => fun api_versions/1
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

api_versions(#{broker := {Host, Port}}) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, {mode, binary}, {packet, 4}]),

    ClientId = list_to_binary(?MODULE_STRING),

    CorrelationId = 1,

    Request = api_versions_request:encode_api_versions_request_3(#{
        correlation_id => CorrelationId, client_id => ClientId,
        client_software_name => <<"kafcod">>, client_software_version => <<"1.2.3">>
    }),
    Decoder = fun api_versions_response:decode_api_versions_response_3/1,

    gen_tcp:send(Socket, Request),

    receive
        {tcp, _, Reply} ->
            parse_buffer(Decoder, Reply);
        M ->
            error(M)
    end,
    erlang:halt(0).

parse_buffer(Decoder, Buffer) ->
    io:format("~p~n", [Buffer]),
    io:format("~p~n", [Decoder(Buffer)]).
