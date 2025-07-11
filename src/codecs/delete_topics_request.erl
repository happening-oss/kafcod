-module(delete_topics_request).
-export([
    encode_delete_topics_request_0/1,
    decode_delete_topics_request_0/1,
    encode_delete_topics_request_1/1,
    decode_delete_topics_request_1/1,
    encode_delete_topics_request_2/1,
    decode_delete_topics_request_2/1,
    encode_delete_topics_request_3/1,
    decode_delete_topics_request_3/1,
    encode_delete_topics_request_4/1,
    decode_delete_topics_request_4/1,
    encode_delete_topics_request_5/1,
    decode_delete_topics_request_5/1,
    encode_delete_topics_request_6/1,
    decode_delete_topics_request_6/1
]).
-export_type([
    delete_topics_request_0/0,
    delete_topics_request_1/0,
    delete_topics_request_2/0,
    delete_topics_request_3/0,
    delete_topics_request_4/0,
    delete_topics_request_5/0,
    delete_topics_request_6/0,
    delete_topic_state_6/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DELETE_TOPICS_REQUEST, 20).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_delete_topics_request_0(delete_topics_request_0()) -> iodata().

encode_delete_topics_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the topics to delete
        topic_names := TopicNames,
        % The length of time in milliseconds to wait for the deletions to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(TopicNames),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?DELETE_TOPICS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(TopicNames, ?encode_string_),
        ?encode_int32(TimeoutMs)
    ];
encode_delete_topics_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topic_names => {array, string},
        timeout_ms => int32
    }).

-spec decode_delete_topics_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_request_0(),
    Rest :: binary().

decode_delete_topics_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(TopicNames, Bin0, Bin1, ?decode_string_),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    {
        Header#{
            topic_names => TopicNames,
            timeout_ms => TimeoutMs
        },
        Bin2
    }.

-spec encode_delete_topics_request_1(delete_topics_request_1()) -> iodata().

encode_delete_topics_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the topics to delete
        topic_names := TopicNames,
        % The length of time in milliseconds to wait for the deletions to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(TopicNames),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?DELETE_TOPICS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(TopicNames, ?encode_string_),
        ?encode_int32(TimeoutMs)
    ];
encode_delete_topics_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topic_names => {array, string},
        timeout_ms => int32
    }).

-spec decode_delete_topics_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_request_1(),
    Rest :: binary().

decode_delete_topics_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(TopicNames, Bin0, Bin1, ?decode_string_),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    {
        Header#{
            topic_names => TopicNames,
            timeout_ms => TimeoutMs
        },
        Bin2
    }.

-spec encode_delete_topics_request_2(delete_topics_request_2()) -> iodata().

encode_delete_topics_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the topics to delete
        topic_names := TopicNames,
        % The length of time in milliseconds to wait for the deletions to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(TopicNames),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?DELETE_TOPICS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_array(TopicNames, ?encode_string_),
        ?encode_int32(TimeoutMs)
    ];
encode_delete_topics_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topic_names => {array, string},
        timeout_ms => int32
    }).

-spec decode_delete_topics_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_request_2(),
    Rest :: binary().

decode_delete_topics_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(TopicNames, Bin0, Bin1, ?decode_string_),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    {
        Header#{
            topic_names => TopicNames,
            timeout_ms => TimeoutMs
        },
        Bin2
    }.

-spec encode_delete_topics_request_3(delete_topics_request_3()) -> iodata().

encode_delete_topics_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the topics to delete
        topic_names := TopicNames,
        % The length of time in milliseconds to wait for the deletions to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(TopicNames),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_1(?DELETE_TOPICS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_array(TopicNames, ?encode_string_),
        ?encode_int32(TimeoutMs)
    ];
encode_delete_topics_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topic_names => {array, string},
        timeout_ms => int32
    }).

-spec decode_delete_topics_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_request_3(),
    Rest :: binary().

decode_delete_topics_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(TopicNames, Bin0, Bin1, ?decode_string_),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    {
        Header#{
            topic_names => TopicNames,
            timeout_ms => TimeoutMs
        },
        Bin2
    }.

-spec encode_delete_topics_request_4(delete_topics_request_4()) -> iodata().

encode_delete_topics_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the topics to delete
        topic_names := TopicNames,
        % The length of time in milliseconds to wait for the deletions to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(TopicNames),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_2(?DELETE_TOPICS_REQUEST, 4, CorrelationId, ClientId),
        ?encode_compact_array(TopicNames, ?encode_compact_string_),
        ?encode_int32(TimeoutMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_topics_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topic_names => {array, string},
        timeout_ms => int32
    }).

-spec decode_delete_topics_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_request_4(),
    Rest :: binary().

decode_delete_topics_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(TopicNames, Bin0, Bin1, ?decode_string_),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_delete_topics_request_4_tagged_field/3,
        Header#{
            topic_names => TopicNames,
            timeout_ms => TimeoutMs
        },
        Bin2
    ).

-spec decode_delete_topics_request_4_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_topics_request_4().

decode_delete_topics_request_4_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_topics_request_5(delete_topics_request_5()) -> iodata().

encode_delete_topics_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The names of the topics to delete
        topic_names := TopicNames,
        % The length of time in milliseconds to wait for the deletions to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(TopicNames),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_2(?DELETE_TOPICS_REQUEST, 5, CorrelationId, ClientId),
        ?encode_compact_array(TopicNames, ?encode_compact_string_),
        ?encode_int32(TimeoutMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_topics_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topic_names => {array, string},
        timeout_ms => int32
    }).

-spec decode_delete_topics_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_request_5(),
    Rest :: binary().

decode_delete_topics_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(TopicNames, Bin0, Bin1, ?decode_string_),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_delete_topics_request_5_tagged_field/3,
        Header#{
            topic_names => TopicNames,
            timeout_ms => TimeoutMs
        },
        Bin2
    ).

-spec decode_delete_topics_request_5_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_topics_request_5().

decode_delete_topics_request_5_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_topics_request_6(delete_topics_request_6()) -> iodata().

encode_delete_topics_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The name or topic ID of the topic
        topics := Topics,
        % The length of time in milliseconds to wait for the deletions to complete.
        timeout_ms := TimeoutMs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs)
->
    [
        ?encode_request_header_2(?DELETE_TOPICS_REQUEST, 6, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_delete_topic_state_6/1),
        ?encode_int32(TimeoutMs),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_topics_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, delete_topic_state_6},
        timeout_ms => int32
    }).

-spec decode_delete_topics_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topics_request_6(),
    Rest :: binary().

decode_delete_topics_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_delete_topic_state_6)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_delete_topics_request_6_tagged_field/3,
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs
        },
        Bin2
    ).

-spec decode_delete_topics_request_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_topics_request_6().

decode_delete_topics_request_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_delete_topic_state_6(delete_topic_state_6()) -> iodata().

encode_delete_topic_state_6(
    _Args = #{
        % The topic name
        name := Name,
        % The unique topic ID
        topic_id := TopicId
    }
) when
    ?is_nullable_string(Name),
    ?is_uuid(TopicId)
->
    [
        ?encode_compact_nullable_string(Name),
        ?encode_uuid(TopicId),
        ?EMPTY_TAG_BUFFER
    ];
encode_delete_topic_state_6(Args) ->
    ?encoder_error(Args, #{
        name => nullable_string,
        topic_id => uuid
    }).

-spec decode_delete_topic_state_6(binary()) -> {Decoded, Rest} when
    Decoded :: delete_topic_state_6(),
    Rest :: binary().

decode_delete_topic_state_6(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_nullable_string(Name, Bin0, Bin1),
    ?_decode_uuid(TopicId, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_delete_topic_state_6_tagged_field/3,
        #{
            name => Name,
            topic_id => TopicId
        },
        Bin2
    ).

-spec decode_delete_topic_state_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: delete_topic_state_6().

decode_delete_topic_state_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type delete_topics_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topic_names := list(binary()),
    timeout_ms := integer()
}.
-type delete_topics_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topic_names := list(binary()),
    timeout_ms := integer()
}.
-type delete_topics_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topic_names := list(binary()),
    timeout_ms := integer()
}.
-type delete_topics_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topic_names := list(binary()),
    timeout_ms := integer()
}.
-type delete_topics_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topic_names := list(binary()),
    timeout_ms := integer()
}.
-type delete_topics_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topic_names := list(binary()),
    timeout_ms := integer()
}.
-type delete_topics_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(delete_topic_state_6()),
    timeout_ms := integer()
}.
-type delete_topic_state_6() :: #{
    name := binary() | null,
    topic_id := kafcod:uuid()
}.
