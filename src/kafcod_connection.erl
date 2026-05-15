-module(kafcod_connection).

%% Simple implementation of a client connection. Useful for basic clients, integration tests, etc.
%%
%% For more advanced uses, consider using kafine's `kafine_connection` instead.

-export([
    start_link/1,
    start_link/2,
    stop/1,

    call/4,
    send_request/3,
    check_response/3
]).
-behaviour(gen_statem).
-export([
    callback_mode/0,
    init/1,
    handle_event/4
]).

-define(DEFAULT_CLIENT_ID, <<"kafcod">>).

-type start_ret() :: gen_statem:start_ret().
-type broker() :: #{host := binary() | list(), port := non_neg_integer(), _ => _}.
-type options() :: #{client_id => binary()}.

-spec start_link(Broker :: broker()) -> start_ret().
start_link(Broker = #{host := _, port := _}) ->
    start_link(Broker, #{}).

-spec start_link(Broker :: broker(), Options :: options()) -> start_ret().
start_link(Broker = #{host := _, port := _}, Options) ->
    gen_statem:start_link(?MODULE, [Broker, Options], []).

stop(Pid) when is_pid(Pid) ->
    gen_statem:stop(Pid).

-type correlation_id() :: integer().

-record(state, {
    client_id :: binary(),
    socket :: inet:socket(),
    correlation_id :: correlation_id(),
    pending :: #{correlation_id() => gen_statem:from()}
}).

call(Pid, Encoder, Args, Decoder) ->
    % If we encode it here, we get better error reporting (at the expense of some performance).
    _Try = Encoder(Args#{client_id => <<>>, correlation_id => 0}),
    {ok, Reply} = gen_statem:call(Pid, {call, Encoder, Args}),
    {Response, <<>>} = Decoder(Reply),
    {ok, maps:without([correlation_id], Response)}.

send_request(Pid, Encoder, Args) ->
    % If we encode it here, we get better error reporting (at the expense of some performance).
    _Try = Encoder(Args#{client_id => <<>>, correlation_id => 0}),
    gen_statem:send_request(Pid, {call, Encoder, Args}).

check_response(Msg, ReqId, Decoder) ->
    {reply, {ok, Encoded}} = gen_statem:check_response(Msg, ReqId),
    {Response, <<>>} = Decoder(Encoded),
    {ok, Response}.

init([Broker, Options]) ->
    % Connect asynchronously.
    {ok, init, no_state, [{next_event, internal, {connect, Broker, Options}}]}.

callback_mode() ->
    handle_event_function.

handle_event(internal, {connect, _Broker = #{host := Host, port := Port}, Options}, init, _) ->
    ClientId = maps:get(client_id, Options, ?DEFAULT_CLIENT_ID),
    CorrelationId = 1,

    {ok, Socket} = gen_tcp:connect(to_hostname(Host), Port, [
        {active, true}, {mode, binary}, {packet, 4}
    ]),

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
    stop.

to_hostname(Host) when is_binary(Host) ->
    binary_to_list(Host);
to_hostname(Host) when is_list(Host) ->
    Host.
