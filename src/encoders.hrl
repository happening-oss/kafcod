%% This include file defines the macros used by the encoders. They come in two flavours:
%%
%% - with a trailing underscore, e.g. 'encode_compact_string_'; these are used for encoding array
%%   elements.
%% - with no trailing/leading underscores, e.g. 'encode_nullable_array'; these are used "normally".

-define(encode_request_header_0(K, V, C),
    % RequestHeader v0 omits ClientId.
    kafcod_request_header:encode_request_header_0(K, V, C)
).
-define(encode_request_header_1(K, V, C, I),
    kafcod_request_header:encode_request_header_1(K, V, C, I)
).
-define(encode_request_header_2(K, V, C, I),
    kafcod_request_header:encode_request_header_2(K, V, C, I)
).

% Note: these functions don't actually exist (because we're not writing a broker here).
-define(encode_response_header_0(C),
    kafcod_response_header:encode_response_header_0(C)
).
-define(encode_response_header_1(C),
    kafcod_response_header:encode_response_header_1(C)
).

-define(encode_bool(V),
    (case V of
        true -> <<1:8/big>>;
        false -> <<0:8/big>>
    end)
).
% Important: Erlang truncates here, so we need to assert the range; we do that in a guard; see guards.hrl.
-define(encode_int8(V), <<V:8/big-signed>>).
-define(encode_int16(V), <<V:16/big-signed>>).
-define(encode_int32(V), <<V:32/big-signed>>).
-define(encode_int64(V), <<V:64/big-signed>>).

-define(encode_float64(V), <<V:64/float-big>>).

% Used in broker registration; not documented.
-define(encode_uint16(V), <<V:16/big-unsigned>>).

-define(encode_array(Arr, Enc), kafcod_primitives:encode_array(Arr, Enc)).
-define(encode_nullable_array(Arr, Enc), kafcod_primitives:encode_nullable_array(Arr, Enc)).
-define(encode_compact_array(Arr, Enc), kafcod_primitives:encode_compact_array(Arr, Enc)).
-define(encode_compact_nullable_array(Arr, Enc),
    kafcod_primitives:encode_compact_nullable_array(Arr, Enc)
).

-define(encode_string(S), kafcod_primitives:encode_string(S)).
-define(encode_nullable_string(S), kafcod_primitives:encode_nullable_string(S)).
-define(encode_compact_string(S), kafcod_primitives:encode_compact_string(S)).
-define(encode_compact_nullable_string(S),
    kafcod_primitives:encode_compact_nullable_string(S)
).

-define(encode_bytes(Bytes), kafcod_primitives:encode_bytes(Bytes)).
-define(encode_nullable_bytes(Bytes), kafcod_primitives:encode_nullable_bytes(Bytes)).
-define(encode_compact_nullable_bytes(Bytes),
    kafcod_primitives:encode_compact_nullable_bytes(Bytes)
).
-define(encode_compact_bytes(Bytes), kafcod_primitives:encode_compact_bytes(Bytes)).

-define(encode_uuid(V), kafcod_primitives:encode_uuid(V)).

-define(encode_tagged_fields(F, A), kafcod_primitives:encode_tagged_fields(F, A)).

-define(encode_nullable_records(R), kafcod_records:encode_records(R)).

-define(encode_compact_records(R), kafcod_records:encode_compact_records(R)).
-define(encode_compact_nullable_records(R), kafcod_records:encode_compact_records(R)).

-define(encode_int8_, fun(V) -> <<V:8/big-signed>> end).
-define(encode_int32_, fun(V) -> <<V:32/big-signed>> end).
-define(encode_int64_, fun(V) -> <<V:64/big-signed>> end).
-define(encode_string_, fun kafcod_primitives:encode_string/1).
-define(encode_compact_string_, fun kafcod_primitives:encode_compact_string/1).
-define(encode_uuid_, fun kafcod_primitives:encode_uuid/1).
