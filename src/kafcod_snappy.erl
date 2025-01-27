-module(kafcod_snappy).
-export([
    compress/1,
    decompress/1
]).

-spec compress(Input :: iodata()) -> {ok, iodata()} | {error, binary()}.

compress(Input) ->
    snappyer:compress(Input).

-spec decompress(Input :: iodata()) -> {ok, binary()} | {error, binary()}.

% Kafka uses a non-standard framing format, as defined by the xerial Java library; see
% https://github.com/xerial/snappy-java/blob/develop/src/main/java/org/xerial/snappy/SnappyCodec.java#L51

decompress(<<130, "SNAPPY", 0, 1:32, 1:32, Chunks/binary>>) ->
    dechunk(Chunks, []);
decompress(Compressed) ->
    snappyer:decompress(Compressed).

dechunk(<<Length:32, Compressed:Length/binary, Rest/binary>>, Acc) ->
    {ok, Uncompressed} = snappyer:decompress(Compressed),
    dechunk(Rest, [Acc, Uncompressed]);
dechunk(<<>>, Acc) ->
    {ok, iolist_to_binary(Acc)}.
