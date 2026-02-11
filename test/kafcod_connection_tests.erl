-module(kafcod_connection_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kafcod/include/error_code.hrl").

-define(WAIT_TIMEOUT_MS, 5_000).
-define(SOCKET, 'socket').

all_test_() ->
    {foreach, spawn, fun setup/0, fun cleanup/1, [
        fun call/0,
        fun send_request/0
    ]}.

setup() ->
    meck:new(gen_tcp, [unstick]),
    meck:expect(gen_tcp, connect, fun(_Host, _Port, _Opts) -> {ok, ?SOCKET} end),
    meck:expect(gen_tcp, connect, fun(_Host, _Port, _Opts, _Timeout) -> {ok, ?SOCKET} end),
    meck:expect(gen_tcp, close, fun(?SOCKET) -> ok end),
    meck:expect(gen_tcp, send, fun(_Socket, _Request) -> ok end),
    ok.

cleanup(_) ->
    meck:unload(),
    ok.

api_versions_server(Connection) ->
    fun() ->
        meck:wait(gen_tcp, send, '_', ?WAIT_TIMEOUT_MS),

        Request = iolist_to_binary(meck:capture(last, gen_tcp, send, '_', 2)),
        <<_ApiKey:16/big-signed, _ApiVersion:16/big-signed, CorrelationId:32/big-signed, _/binary>> =
            Request,

        Response = iolist_to_binary(
            api_versions_response:encode_api_versions_response_3(#{
                correlation_id => CorrelationId,
                error_code => ?NONE,
                throttle_time_ms => 0,
                api_keys => []
            })
        ),

        Connection ! {tcp, ?SOCKET, Response}
    end.

call() ->
    Broker = #{host => <<"ignored">>, port => -1, node_id => 101},
    Options = #{client_id => <<"kafcod_connection_tests">>},
    {ok, Connection} = kafcod_connection:start_link(Broker, Options),

    meck:wait(gen_tcp, connect, '_', ?WAIT_TIMEOUT_MS),

    % Because 'call' is synchronous, we need a separate process to pretend to be the server.
    _Server = spawn_link(api_versions_server(Connection)),

    {ok, #{error_code := ?NONE, api_keys := _}} = kafcod_connection:call(
        Connection,
        fun api_versions_request:encode_api_versions_request_3/1,
        #{
            client_software_name => <<"kafcod">>,
            client_software_version => <<"0.50.1">>
        },
        fun api_versions_response:decode_api_versions_response_3/1
    ),

    kafcod_connection:stop(Connection),
    ok.

-define(async_response, {[alias | _], _}).
-define(receive_response(),
    (fun() ->
        receive
            ?async_response = Msg ->
                Msg
        end
    end)()
).

send_request() ->
    Broker = #{host => <<"ignored">>, port => -1},
    Options = #{client_id => <<"kafcod_connection_tests">>},
    {ok, Connection} = kafcod_connection:start_link(Broker, Options),

    meck:wait(gen_tcp, connect, '_', ?WAIT_TIMEOUT_MS),

    % Even though 'send_request' is async, we might as well reuse the server process from above.
    _Server = spawn_link(api_versions_server(Connection)),

    ReqId = kafcod_connection:send_request(
        Connection,
        fun api_versions_request:encode_api_versions_request_3/1,
        #{
            client_software_name => <<"kafcod">>,
            client_software_version => <<"0.50.1">>
        }
    ),

    Msg = ?receive_response(),
    {ok, #{error_code := ?NONE, api_keys := _}} = kafcod_connection:check_response(
        Msg, ReqId, fun api_versions_response:decode_api_versions_response_3/1
    ),

    kafcod_connection:stop(Connection),
    ok.
