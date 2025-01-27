%% This include file defines the macros used by the decoders. They come in three flavours:
%%
%% - with a leading underscore, e.g. '_decode_int32'; these are unhygienic macros that are used
%%   as, e.g. ?_decode_int32(CorrelationId, Bin1, Bin2), taking 'Bin1' as input, and setting
%%   'CorrelationId' and 'Bin2' as outputs.
%% - with a trailing underscore, e.g. 'decode_int32_'; these are used for decoding array elements.
%% - with no leading/trailing underscores, e.g. 'decode_response_header_1'; these are used
%%   "normally".

-define(decode_request_header_0(Input), kafcod_request_header:decode_request_header_0(Input)).
-define(decode_request_header_1(Input), kafcod_request_header:decode_request_header_1(Input)).
-define(decode_request_header_2(Input), kafcod_request_header:decode_request_header_2(Input)).

-define(decode_response_header_0(Input), kafcod_response_header:decode_response_header_0(Input)).
-define(decode_response_header_1(Input), kafcod_response_header:decode_response_header_1(Input)).

-define(_decode_int8(Variable, Input, Rest), <<Variable:8/big-signed, Rest/binary>> = Input).
-define(_decode_int16(Variable, Input, Rest), <<Variable:16/big-signed, Rest/binary>> = Input).
-define(_decode_int32(Variable, Input, Rest), <<Variable:32/big-signed, Rest/binary>> = Input).
-define(_decode_int64(Variable, Input, Rest), <<Variable:64/big-signed, Rest/binary>> = Input).

% TODO: We're decoding uint and int the same; there are only a few uint-encoded fields in the protocol, so we've not run
% into any problems (yet).
-define(_decode_uint16(Variable, Input, Rest), <<Variable:16/big-signed, Rest/binary>> = Input).

-define(_decode_float64(Variable, Input, Rest), <<Variable:64/float-big, Rest/binary>> = Input).

-define(_decode_bool(Variable, Input, Rest),
    % If this is written with a 'case' expression (see below), you get "variable 'X' exported from 'case'" warnings,
    % where 'X' is whatever variable is substituted for 'Rest'. Using a function prevents that.
    {Variable, Rest} = kafcod_primitives:decode_bool(Input)
).

% We keep these (commented out), to provide context for the comment above.

% This doesn't work; 'Rest_' leaks out of the first _decode_bool, and interferes with the ones afterwards.
% This manifests as 'case clause' errors if you have more than one ?_decode_bool in a decoder.
%
% -define(_decode_bool(Variable, Input, Rest),
%     {Variable, Rest} = case Input of
%         <<0:8/big, Rest_/binary>> -> {false, Rest_};
%         <<1:8/big, Rest_/binary>> -> {true, Rest_}
%     end
% ).

% This _works_ (it _relies_ on the variable being exported), but the compiler still complains about it. Bastard.
%
% -define(_decode_bool(Variable, Input, Rest),
%     Variable = case Input of
%         <<0:8/big, Rest/binary>> -> false;
%         <<1:8/big, Rest/binary>> -> true
%     end
% ).

% This also works, but the compiler still complains about 'Rest' being exported.
%
% -define(_decode_bool(Variable, Input, Rest),
%     {Variable, Rest} = case Input of
%         <<0:8/big, Rest/binary>> -> {false, Rest};
%         <<1:8/big, Rest/binary>> -> {true, Rest}
%     end
% ).

-define(_decode_string(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_string(Input)
).
-define(_decode_nullable_string(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_nullable_string(Input)
).
-define(_decode_compact_string(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_compact_string(Input)
).
-define(_decode_compact_nullable_string(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_compact_nullable_string(Input)
).

-define(_decode_array(Variable, Input, Rest, Fun),
    {Variable, Rest} = kafcod_primitives:decode_array(Input, Fun)
).
-define(_decode_nullable_array(Variable, Input, Rest, Fun),
    {Variable, Rest} = kafcod_primitives:decode_nullable_array(Input, Fun)
).
-define(_decode_compact_array(Variable, Input, Rest, Fun),
    {Variable, Rest} = kafcod_primitives:decode_compact_array(Input, Fun)
).
-define(_decode_compact_nullable_array(Variable, Input, Rest, Fun),
    {Variable, Rest} = kafcod_primitives:decode_compact_nullable_array(Input, Fun)
).
-define(_decode_element(Fun), fun Fun/1).

-define(_decode_bytes(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_bytes(Input)
).
-define(_decode_nullable_bytes(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_nullable_bytes(Input)
).
-define(_decode_compact_bytes(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_compact_bytes(Input)
).
-define(_decode_compact_nullable_bytes(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_compact_nullable_bytes(Input)
).

-define(_decode_uuid(Variable, Input, Rest),
    {Variable, Rest} = kafcod_primitives:decode_uuid(Input)
).

-define(_decode_entity(Variable, Input, Rest, Fun), {Variable, Rest} = Fun(Input)).

-define(_decode_nullable_records(Variable, Input, Rest),
    {Variable, Rest} = kafcod_records:decode_records(Input)
).
-define(_decode_compact_records(Variable, Input, Rest),
    {Variable, Rest} = kafcod_records:decode_compact_records(Input)
).
-define(_decode_compact_nullable_records(Variable, Input, Rest),
    {Variable, Rest} = kafcod_records:decode_compact_records(Input)
).

-define(decode_tagged_fields(Fun, Bin, Acc), kafcod_primitives:decode_tagged_fields(Fun, Bin, Acc)).

% The 'D__' prefix (for "decoder") is to prevent variable shadowing; you're unlikely to have used such an ugly name in
% the containing scope.
-define(decode_int32_, fun(D__Bin) ->
    <<V:32/big, Rest/binary>> = D__Bin,
    {V, Rest}
end).
-define(decode_int64_, fun(D__Bin) ->
    <<V:64/big, Rest/binary>> = D__Bin,
    {V, Rest}
end).
-define(decode_string_, fun(D__Bin) -> kafcod_primitives:decode_string(D__Bin) end).

-ifdef(DECODER_TRACING).
% Decoder tracing replaces (some of) the decoding macros with variants that write to the Erlang logger.
% It's a long way from being a complete set; feel free to write some more.

-include_lib("kernel/include/logger.hrl").

-undef(_decode_int16).
-define(_decode_int16(Variable, Input, Rest), begin
    ?LOG_DEBUG("{~s, _} = _decode_int16(~p)", [??Variable, Input]),
    <<Variable:16/big-signed, Rest/binary>> = Input,
    ?LOG_DEBUG("_decode_int16() => {~s = ~p, ~p}", [??Variable, Variable, Rest])
end).

-undef(_decode_int32).
-define(_decode_int32(Variable, Input, Rest), begin
    ?LOG_DEBUG("{~s, _} = _decode_int32(~p)", [??Variable, Input]),
    <<Variable:32/big-signed, Rest/binary>> = Input,
    ?LOG_DEBUG("_decode_int32() => {~s = ~p, ~p}", [??Variable, Variable, Rest])
end).

-undef(_decode_array).
-define(_decode_array(Variable, Input, Rest, Fun),
    ?LOG_DEBUG("{~s, _} = _decode_array(~p, ~s)", [??Variable, Input, ??Fun]),
    {Variable, Rest} = kafcod_primitives:decode_array(Input, Fun),
    ?LOG_DEBUG("_decode_array() => {~s = ~p, ~p}", [??Variable, Variable, Rest])
).

-undef(_decode_compact_array).
-define(_decode_compact_array(Variable, Input, Rest, Fun), begin
    ?LOG_DEBUG("{~s, _} = _decode_compact_array(~p, ~s)", [??Variable, Input, ??Fun]),
    {Variable, Rest} = kafcod_primitives:decode_compact_array(Input, Fun),
    ?LOG_DEBUG("_decode_compact_array() => {~s = ~p, ~p}", [??Variable, Variable, Rest])
end).

-undef(_decode_records).
-define(_decode_records(Variable, Input, Rest),
    ?LOG_DEBUG("{~s, _} = _decode_records(~p)", [??Variable, Input]),
    {Variable, Rest} = kafcod_records:decode_records(Input),
    ?LOG_DEBUG("_decode_records() => {~s = ~p, ~p}", [??Variable, Variable, Rest])
).

-endif.
