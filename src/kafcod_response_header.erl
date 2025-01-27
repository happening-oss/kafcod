-module(kafcod_response_header).
-export([
    decode_response_header_0/1,
    encode_response_header_0/1,
    decode_response_header_1/1,
    encode_response_header_1/1
]).

% erlfmt-ignore
% ^^ don't want spaces around "operators"; can't wrap it in <<>>.
-define(EMPTY_TAG_BUFFER, 0:8/big-signed).

-spec decode_response_header_0(Input :: nonempty_binary()) ->
    {Header :: response_header_0(), Rest :: binary()}.

-type response_header_0() :: #{
    correlation_id := non_neg_integer()
}.

decode_response_header_0(Bin) when is_binary(Bin) ->
    <<CorrelationId:32/big-signed, Rest/binary>> = Bin,
    {#{correlation_id => CorrelationId}, Rest}.

-spec encode_response_header_0(CorrelationId :: integer()) -> iodata().

encode_response_header_0(CorrelationId) ->
    <<CorrelationId:32/big-signed>>.

-spec decode_response_header_1(Input :: nonempty_binary()) ->
    {Header :: response_header_1(), Rest :: binary()}.

-type response_header_1() :: #{
    correlation_id := non_neg_integer()
}.

decode_response_header_1(Bin) when is_binary(Bin) ->
    <<CorrelationId:32/big-signed, ?EMPTY_TAG_BUFFER, Rest/binary>> = Bin,
    {#{correlation_id => CorrelationId}, Rest}.

-spec encode_response_header_1(CorrelationId :: integer()) -> iodata().

encode_response_header_1(CorrelationId) ->
    <<CorrelationId:32/big-signed, ?EMPTY_TAG_BUFFER>>.
