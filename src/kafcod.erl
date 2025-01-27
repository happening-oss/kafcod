-module(kafcod).
-export_type([
    bytes/0,
    nullable_bytes/0,
    uuid/0
]).
-export_type([
    api_key/0
]).

-type bytes() :: binary().
-type nullable_bytes() :: binary() | null.
-type uuid() :: binary().

-type api_key() :: 0..74.
