-module(get_metadata).
-export([main/1]).

main(_Args) ->
    Host = "localhost",
    Port = 9092,
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, {mode, binary}, {packet, 4}]),

    ClientId = list_to_binary(?MODULE_STRING),

    CorrelationId = 1,

    Request = metadata_request:encode_metadata_request_12(#{
        correlation_id => CorrelationId,
        client_id => ClientId,
        topics => null,
        allow_auto_topic_creation => false,
        include_topic_authorized_operations => true
    }),
    Decoder = fun metadata_response:decode_metadata_response_12/1,

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
    io:format("~p~n", [Decoder(Buffer)]).
