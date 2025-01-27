-module(kafcod_crc32c_tests).
-include_lib("eunit/include/eunit.hrl").

some_string_test() ->
    ?assertEqual(608512271, kafcod_crc32c:value(<<"Some String">>)).

large_string_test() ->
    % 100MiB of randomly-generated data. Use a fixed seed, so it's predictable.
    Len = 100 * 1024 * 1024,
    Seed = 1694598395,      % Unix timestamp when I wrote the test.
    Algo = exsss,           % default algo at the time of writing.
    S1 = rand:seed_s(Algo, Seed),
    {Bytes, _S2} = rand:bytes_s(Len, S1),
    % 100MiB takes about 700ms with the pure Erlang implementation, about 70ms with the NIF implementation.
    % (on a MacBook Pro (14-inch, 2021), Apple M1 Pro)
    {_ElapsedUs, CRC} = timer:tc(fun() -> kafcod_crc32c:value(Bytes) end),
    Expected = 4098849710,      % from the original Erlang implementation.
    ?assertEqual(Expected, CRC),      % 100MiB
    ok.
