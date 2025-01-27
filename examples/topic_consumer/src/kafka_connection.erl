-module(kafka_connection).
-export([
    start_link/3,
    stop/1,
    call/4
]).
-behaviour(gen_statem).
-export([
    callback_mode/0,
    init/1,
    handle_event/4
]).

start_link(Host, Port, ClientId) when is_binary(Host), is_integer(Port), is_binary(ClientId) ->
    start_link(binary_to_list(Host), Port, ClientId);
start_link(Host, Port, ClientId) when is_list(Host), is_integer(Port), is_binary(ClientId) ->
    gen_statem:start_link(?MODULE, [Host, Port, ClientId], []).

stop(Pid) ->
    gen_statem:stop(Pid).

-type correlation_id() :: integer().

-record(state, {
    client_id :: binary(),
    socket :: inet:socket(),
    correlation_id :: correlation_id(),
    pending :: #{correlation_id() => gen_statem:from()}
}).

call(Pid, Encoder, Args, Decoder) ->
    _Try = Encoder(Args#{client_id => <<>>, correlation_id => 0}),
    {ok, Reply} = gen_statem:call(Pid, {call, Encoder, Args}),
    {Response, <<>>} = Decoder(Reply),
    {ok, maps:without([correlation_id], Response)}.

init([Host, Port, ClientId]) ->
    % Connect asynchronously.
    {ok, init, no_state, [{next_event, internal, {connect, Host, Port, ClientId}}]}.

callback_mode() ->
    handle_event_function.

handle_event(internal, {connect, Host, Port, ClientId}, init, _) ->
    CorrelationId = 1,

    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, {mode, binary}, {packet, 4}]),

    {next_state, connected, #state{
        client_id = ClientId, correlation_id = CorrelationId, socket = Socket, pending = #{}
    }};
handle_event(
    {call, From},
    _Req = {call, Encoder, Args},
    _State = connected,
    StateData = #state{
        client_id = ClientId, correlation_id = CorrelationId, socket = Socket, pending = Pending
    }
) ->
    Request = Encoder(Args#{client_id => ClientId, correlation_id => CorrelationId}),
    ok = gen_tcp:send(Socket, Request),
    {next_state, connected, StateData#state{
        correlation_id = CorrelationId + 1,
        pending = Pending#{
            CorrelationId => From
        }
    }};
handle_event(
    info, {tcp, Socket, Buffer}, connected, StateData = #state{socket = Socket, pending = Pending}
) ->
    <<CorrelationId:32/big-signed, _/binary>> = Buffer,
    From = maps:get(CorrelationId, Pending),
    {next_state, connected, StateData#state{pending = maps:remove(CorrelationId, Pending)},
        {reply, From, {ok, Buffer}}};
handle_event(info, {tcp_closed, Socket}, _State, _StateData = #state{socket = Socket}) ->
    {stop, closed}.
