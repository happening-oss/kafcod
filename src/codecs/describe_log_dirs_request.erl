-module(describe_log_dirs_request).
-export([
    encode_describe_log_dirs_request_0/1,
    decode_describe_log_dirs_request_0/1,
    encode_describe_log_dirs_request_1/1,
    decode_describe_log_dirs_request_1/1,
    encode_describe_log_dirs_request_2/1,
    decode_describe_log_dirs_request_2/1,
    encode_describe_log_dirs_request_3/1,
    decode_describe_log_dirs_request_3/1,
    encode_describe_log_dirs_request_4/1,
    decode_describe_log_dirs_request_4/1
]).
-export_type([
    describe_log_dirs_request_0/0,
    describable_log_dir_topic_0/0,
    describe_log_dirs_request_1/0,
    describable_log_dir_topic_1/0,
    describe_log_dirs_request_2/0,
    describable_log_dir_topic_2/0,
    describe_log_dirs_request_3/0,
    describable_log_dir_topic_3/0,
    describe_log_dirs_request_4/0,
    describable_log_dir_topic_4/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_LOG_DIRS_REQUEST, 35).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_log_dirs_request_0(describe_log_dirs_request_0()) -> iodata().

encode_describe_log_dirs_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to describe log directories for, or null for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?DESCRIBE_LOG_DIRS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_describable_log_dir_topic_0/1)
    ];
encode_describe_log_dirs_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, describable_log_dir_topic_0}
    }).

-spec decode_describe_log_dirs_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_request_0(),
    Rest :: binary().

decode_describe_log_dirs_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_describable_log_dir_topic_0)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_describable_log_dir_topic_0(describable_log_dir_topic_0()) -> iodata().

encode_describable_log_dir_topic_0(
    _Args = #{
        % The topic name
        topic := Topic,
        % The partition indexes.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_describable_log_dir_topic_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_describable_log_dir_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: describable_log_dir_topic_0(),
    Rest :: binary().

decode_describable_log_dir_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_describe_log_dirs_request_1(describe_log_dirs_request_1()) -> iodata().

encode_describe_log_dirs_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to describe log directories for, or null for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?DESCRIBE_LOG_DIRS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_describable_log_dir_topic_1/1)
    ];
encode_describe_log_dirs_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, describable_log_dir_topic_1}
    }).

-spec decode_describe_log_dirs_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_request_1(),
    Rest :: binary().

decode_describe_log_dirs_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_describable_log_dir_topic_1)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_describable_log_dir_topic_1(describable_log_dir_topic_1()) -> iodata().

encode_describable_log_dir_topic_1(
    _Args = #{
        % The topic name
        topic := Topic,
        % The partition indexes.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_string(Topic),
        ?encode_array(Partitions, ?encode_int32_)
    ];
encode_describable_log_dir_topic_1(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_describable_log_dir_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: describable_log_dir_topic_1(),
    Rest :: binary().

decode_describable_log_dir_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(Partitions, Bin1, Bin2, ?decode_int32_),
    {
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    }.

-spec encode_describe_log_dirs_request_2(describe_log_dirs_request_2()) -> iodata().

encode_describe_log_dirs_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to describe log directories for, or null for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_2(?DESCRIBE_LOG_DIRS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Topics, fun encode_describable_log_dir_topic_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, describable_log_dir_topic_2}
    }).

-spec decode_describe_log_dirs_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_request_2(),
    Rest :: binary().

decode_describe_log_dirs_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_describable_log_dir_topic_2)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_request_2_tagged_field/3,
        Header#{
            topics => Topics
        },
        Bin1
    ).

-spec decode_describe_log_dirs_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_log_dirs_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describable_log_dir_topic_2(describable_log_dir_topic_2()) -> iodata().

encode_describable_log_dir_topic_2(
    _Args = #{
        % The topic name
        topic := Topic,
        % The partition indexes.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_describable_log_dir_topic_2(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_describable_log_dir_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: describable_log_dir_topic_2(),
    Rest :: binary().

decode_describable_log_dir_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_describable_log_dir_topic_2_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_describable_log_dir_topic_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describable_log_dir_topic_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_request_3(describe_log_dirs_request_3()) -> iodata().

encode_describe_log_dirs_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to describe log directories for, or null for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_2(?DESCRIBE_LOG_DIRS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Topics, fun encode_describable_log_dir_topic_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, describable_log_dir_topic_3}
    }).

-spec decode_describe_log_dirs_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_request_3(),
    Rest :: binary().

decode_describe_log_dirs_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_describable_log_dir_topic_3)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_request_3_tagged_field/3,
        Header#{
            topics => Topics
        },
        Bin1
    ).

-spec decode_describe_log_dirs_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_log_dirs_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describable_log_dir_topic_3(describable_log_dir_topic_3()) -> iodata().

encode_describable_log_dir_topic_3(
    _Args = #{
        % The topic name
        topic := Topic,
        % The partition indexes.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_describable_log_dir_topic_3(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_describable_log_dir_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: describable_log_dir_topic_3(),
    Rest :: binary().

decode_describable_log_dir_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_describable_log_dir_topic_3_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_describable_log_dir_topic_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describable_log_dir_topic_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_log_dirs_request_4(describe_log_dirs_request_4()) -> iodata().

encode_describe_log_dirs_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to describe log directories for, or null for all topics.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_2(?DESCRIBE_LOG_DIRS_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Topics, fun encode_describable_log_dir_topic_4/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_log_dirs_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, describable_log_dir_topic_4}
    }).

-spec decode_describe_log_dirs_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: describe_log_dirs_request_4(),
    Rest :: binary().

decode_describe_log_dirs_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_describable_log_dir_topic_4)),
    ?decode_tagged_fields(
        fun decode_describe_log_dirs_request_4_tagged_field/3,
        Header#{
            topics => Topics
        },
        Bin1
    ).

-spec decode_describe_log_dirs_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describe_log_dirs_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describable_log_dir_topic_4(describable_log_dir_topic_4()) -> iodata().

encode_describable_log_dir_topic_4(
    _Args = #{
        % The topic name
        topic := Topic,
        % The partition indexes.
        partitions := Partitions
    }
) when
    ?is_string(Topic),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(Partitions, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_describable_log_dir_topic_4(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partitions => {array, int32}
    }).

-spec decode_describable_log_dir_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: describable_log_dir_topic_4(),
    Rest :: binary().

decode_describable_log_dir_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_describable_log_dir_topic_4_tagged_field/3,
        #{
            topic => Topic,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_describable_log_dir_topic_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_describable_log_dir_topic_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_log_dirs_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(describable_log_dir_topic_0()) | null
}.
-type describable_log_dir_topic_0() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type describe_log_dirs_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(describable_log_dir_topic_1()) | null
}.
-type describable_log_dir_topic_1() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type describe_log_dirs_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(describable_log_dir_topic_2()) | null
}.
-type describable_log_dir_topic_2() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type describe_log_dirs_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(describable_log_dir_topic_3()) | null
}.
-type describable_log_dir_topic_3() :: #{
    topic := binary(),
    partitions := list(integer())
}.
-type describe_log_dirs_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(describable_log_dir_topic_4()) | null
}.
-type describable_log_dir_topic_4() :: #{
    topic := binary(),
    partitions := list(integer())
}.
