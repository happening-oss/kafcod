-module(alter_replica_log_dirs_request).
-export([
    encode_alter_replica_log_dirs_request_0/1,
    decode_alter_replica_log_dirs_request_0/1,
    encode_alter_replica_log_dirs_request_1/1,
    decode_alter_replica_log_dirs_request_1/1,
    encode_alter_replica_log_dirs_request_2/1,
    decode_alter_replica_log_dirs_request_2/1
]).
-export_type([
    alter_replica_log_dirs_request_0/0,
    alter_replica_log_dir_topic_0/0,
    alter_replica_log_dir_0/0,
    alter_replica_log_dirs_request_1/0,
    alter_replica_log_dir_topic_1/0,
    alter_replica_log_dir_1/0,
    alter_replica_log_dirs_request_2/0,
    alter_replica_log_dir_topic_2/0,
    alter_replica_log_dir_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(ALTER_REPLICA_LOG_DIRS_REQUEST, 34).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_alter_replica_log_dirs_request_0(alter_replica_log_dirs_request_0()) -> iodata().

encode_alter_replica_log_dirs_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The alterations to make for each directory.
        dirs := Dirs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Dirs)
->
    [
        ?encode_request_header_1(?ALTER_REPLICA_LOG_DIRS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Dirs, fun encode_alter_replica_log_dir_0/1)
    ];
encode_alter_replica_log_dirs_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        dirs => {array, alter_replica_log_dir_0}
    }).

-spec decode_alter_replica_log_dirs_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dirs_request_0(),
    Rest :: binary().

decode_alter_replica_log_dirs_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Dirs, Bin0, Bin1, ?_decode_element(decode_alter_replica_log_dir_0)),
    {
        Header#{
            dirs => Dirs
        },
        Bin1
    }.

-spec encode_alter_replica_log_dir_topic_0(alter_replica_log_dir_topic_0()) -> iodata().

encode_alter_replica_log_dir_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_alter_replica_log_dir_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_alter_replica_log_dir_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_topic_0(),
    Rest :: binary().

decode_alter_replica_log_dir_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_alter_replica_log_dir_0(alter_replica_log_dir_0()) -> iodata().

encode_alter_replica_log_dir_0(
    _Args = #{
        % The absolute directory path.
        path := Path,
        % The topics to add to the directory.
        topics := Topics
    }
) when
    ?is_string(Path),
    ?is_array(Topics)
->
    [
        ?encode_string(Path),
        ?encode_array(Topics, fun encode_alter_replica_log_dir_topic_0/1)
    ];
encode_alter_replica_log_dir_0(Args) ->
    ?encoder_error(Args, #{
        path => string,
        topics => {array, alter_replica_log_dir_topic_0}
    }).

-spec decode_alter_replica_log_dir_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_0(),
    Rest :: binary().

decode_alter_replica_log_dir_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Path, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_topic_0)),
    {
        #{
            path => Path,
            topics => Topics
        },
        Bin2
    }.

-spec encode_alter_replica_log_dirs_request_1(alter_replica_log_dirs_request_1()) -> iodata().

encode_alter_replica_log_dirs_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The alterations to make for each directory.
        dirs := Dirs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Dirs)
->
    [
        ?encode_request_header_1(?ALTER_REPLICA_LOG_DIRS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Dirs, fun encode_alter_replica_log_dir_1/1)
    ];
encode_alter_replica_log_dirs_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        dirs => {array, alter_replica_log_dir_1}
    }).

-spec decode_alter_replica_log_dirs_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dirs_request_1(),
    Rest :: binary().

decode_alter_replica_log_dirs_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Dirs, Bin0, Bin1, ?_decode_element(decode_alter_replica_log_dir_1)),
    {
        Header#{
            dirs => Dirs
        },
        Bin1
    }.

-spec encode_alter_replica_log_dir_topic_1(alter_replica_log_dir_topic_1()) -> iodata().

encode_alter_replica_log_dir_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_string(Name),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_alter_replica_log_dir_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_alter_replica_log_dir_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_topic_1(),
    Rest :: binary().

decode_alter_replica_log_dir_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_alter_replica_log_dir_1(alter_replica_log_dir_1()) -> iodata().

encode_alter_replica_log_dir_1(
    _Args = #{
        % The absolute directory path.
        path := Path,
        % The topics to add to the directory.
        topics := Topics
    }
) when
    ?is_string(Path),
    ?is_array(Topics)
->
    [
        ?encode_string(Path),
        ?encode_array(Topics, fun encode_alter_replica_log_dir_topic_1/1)
    ];
encode_alter_replica_log_dir_1(Args) ->
    ?encoder_error(Args, #{
        path => string,
        topics => {array, alter_replica_log_dir_topic_1}
    }).

-spec decode_alter_replica_log_dir_1(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_1(),
    Rest :: binary().

decode_alter_replica_log_dir_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Path, Bin0, Bin1),
    ?_decode_array(Topics, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_topic_1)),
    {
        #{
            path => Path,
            topics => Topics
        },
        Bin2
    }.

-spec encode_alter_replica_log_dirs_request_2(alter_replica_log_dirs_request_2()) -> iodata().

encode_alter_replica_log_dirs_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The alterations to make for each directory.
        dirs := Dirs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Dirs)
->
    [
        ?encode_request_header_2(?ALTER_REPLICA_LOG_DIRS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_array(Dirs, fun encode_alter_replica_log_dir_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_replica_log_dirs_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        dirs => {array, alter_replica_log_dir_2}
    }).

-spec decode_alter_replica_log_dirs_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dirs_request_2(),
    Rest :: binary().

decode_alter_replica_log_dirs_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Dirs, Bin0, Bin1, ?_decode_element(decode_alter_replica_log_dir_2)),
    ?decode_tagged_fields(
        fun decode_alter_replica_log_dirs_request_2_tagged_field/3,
        Header#{
            dirs => Dirs
        },
        Bin1
    ).

-spec decode_alter_replica_log_dirs_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_replica_log_dirs_request_2().

decode_alter_replica_log_dirs_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_replica_log_dir_topic_2(alter_replica_log_dir_topic_2()) -> iodata().

encode_alter_replica_log_dir_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The partition indexes.
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_replica_log_dir_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, int32}
    }).

-spec decode_alter_replica_log_dir_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_topic_2(),
    Rest :: binary().

decode_alter_replica_log_dir_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_alter_replica_log_dir_topic_2_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_alter_replica_log_dir_topic_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_replica_log_dir_topic_2().

decode_alter_replica_log_dir_topic_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_alter_replica_log_dir_2(alter_replica_log_dir_2()) -> iodata().

encode_alter_replica_log_dir_2(
    _Args = #{
        % The absolute directory path.
        path := Path,
        % The topics to add to the directory.
        topics := Topics
    }
) when
    ?is_string(Path),
    ?is_array(Topics)
->
    [
        ?encode_compact_string(Path),
        ?encode_compact_array(Topics, fun encode_alter_replica_log_dir_topic_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_replica_log_dir_2(Args) ->
    ?encoder_error(Args, #{
        path => string,
        topics => {array, alter_replica_log_dir_topic_2}
    }).

-spec decode_alter_replica_log_dir_2(binary()) -> {Decoded, Rest} when
    Decoded :: alter_replica_log_dir_2(),
    Rest :: binary().

decode_alter_replica_log_dir_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Path, Bin0, Bin1),
    ?_decode_compact_array(Topics, Bin1, Bin2, ?_decode_element(decode_alter_replica_log_dir_topic_2)),
    ?decode_tagged_fields(
        fun decode_alter_replica_log_dir_2_tagged_field/3,
        #{
            path => Path,
            topics => Topics
        },
        Bin2
    ).

-spec decode_alter_replica_log_dir_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_replica_log_dir_2().

decode_alter_replica_log_dir_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type alter_replica_log_dirs_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    dirs := list(alter_replica_log_dir_0())
}.
-type alter_replica_log_dir_topic_0() :: #{
    name := binary(),
    partitions := list(integer())
}.
-type alter_replica_log_dir_0() :: #{
    path := binary(),
    topics := list(alter_replica_log_dir_topic_0())
}.
-type alter_replica_log_dirs_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    dirs := list(alter_replica_log_dir_1())
}.
-type alter_replica_log_dir_topic_1() :: #{
    name := binary(),
    partitions := list(integer())
}.
-type alter_replica_log_dir_1() :: #{
    path := binary(),
    topics := list(alter_replica_log_dir_topic_1())
}.
-type alter_replica_log_dirs_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    dirs := list(alter_replica_log_dir_2())
}.
-type alter_replica_log_dir_topic_2() :: #{
    name := binary(),
    partitions := list(integer())
}.
-type alter_replica_log_dir_2() :: #{
    path := binary(),
    topics := list(alter_replica_log_dir_topic_2())
}.
