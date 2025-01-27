-define(is_bool(V), is_boolean(V)).

-define(is_int8(V), is_integer(V), V >= -128, V =< 127).
-define(is_int16(V), is_integer(V), V >= -32768, V =< 32767).
-define(is_int32(V), is_integer(V), V >= -2147483648, V =< 2147483647).
-define(is_int64(V), is_integer(V), V >= -9223372036854775808, V =< 9223372036854775807).
-define(is_uint16(V), is_integer(V), V >= 0, V =< 65536).
-define(is_float64(V), is_float(V)).

-define(is_string(V), is_binary(V)).
-define(is_nullable_string(V), (is_binary(V) orelse V =:= null)).
-define(is_array(V), is_list(V)).
-define(is_nullable_array(V), (is_list(V) orelse V =:= null)).
-define(is_bytes(V), is_binary(V)).
-define(is_nullable_bytes(V), (is_binary(V) orelse V =:= null)).

-define(is_records(V), is_list(V)).
-define(is_nullable_records(V), is_list(V)).

% TODO: Assert the length
-define(is_uuid(V), is_binary(V)).

-define(is_entity(V), is_map(V)).
-define(is_nullable_entity(V), (is_map(V) orelse V =:= null)).
