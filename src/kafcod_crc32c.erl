-module(kafcod_crc32c).

%% For speed, we prefer to use https://hex.pm/packages/crc32cer, a NIF-based implementation, but that breaks the example
%% escripts (because you can't put NIFs inside escript). If you want to do that, hack on kafcod to call
%% kafcod_crc32c_erl instead (but don't commit the change!).

-export([value/1]).

-spec value(iodata()) -> non_neg_integer().

value(Buf) ->
    value(0, iolist_to_binary(Buf)).

-spec value(non_neg_integer(), binary()) -> non_neg_integer().

% On a MacBook Pro (14-inch, 2021), Apple M1 Pro, it takes about 7ms for the NIF to CRC 10MiB and about 70ms for 100MiB.
% I'd prefer the NIF to take less than ~10ms, so we'll chop the input into 8MiB chunks. This number was chosen
% arbitrarily, both to (hopefully) get the performance numbers required, and to ensure that the "large" test in the unit
% tests wasn't a round multiple of the chunk size.
-define(CHUNK_LEN, 8388608).

value(Acc, _Buf = <<Head:?CHUNK_LEN/binary, Tail/binary>>) ->
    value(crc32cer:nif(Acc, Head), Tail);
value(Acc, Rest) ->
    crc32cer:nif(Acc, Rest).
