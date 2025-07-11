-module(metadata_request).
-export([
    encode_metadata_request_0/1,
    decode_metadata_request_0/1,
    encode_metadata_request_1/1,
    decode_metadata_request_1/1,
    encode_metadata_request_2/1,
    decode_metadata_request_2/1,
    encode_metadata_request_3/1,
    decode_metadata_request_3/1,
    encode_metadata_request_4/1,
    decode_metadata_request_4/1,
    encode_metadata_request_5/1,
    decode_metadata_request_5/1,
    encode_metadata_request_6/1,
    decode_metadata_request_6/1,
    encode_metadata_request_7/1,
    decode_metadata_request_7/1,
    encode_metadata_request_8/1,
    decode_metadata_request_8/1,
    encode_metadata_request_9/1,
    decode_metadata_request_9/1,
    encode_metadata_request_10/1,
    decode_metadata_request_10/1,
    encode_metadata_request_11/1,
    decode_metadata_request_11/1,
    encode_metadata_request_12/1,
    decode_metadata_request_12/1
]).
-export_type([
    metadata_request_0/0,
    metadata_request_topic_0/0,
    metadata_request_1/0,
    metadata_request_topic_1/0,
    metadata_request_2/0,
    metadata_request_topic_2/0,
    metadata_request_3/0,
    metadata_request_topic_3/0,
    metadata_request_4/0,
    metadata_request_topic_4/0,
    metadata_request_5/0,
    metadata_request_topic_5/0,
    metadata_request_6/0,
    metadata_request_topic_6/0,
    metadata_request_7/0,
    metadata_request_topic_7/0,
    metadata_request_8/0,
    metadata_request_topic_8/0,
    metadata_request_9/0,
    metadata_request_topic_9/0,
    metadata_request_10/0,
    metadata_request_topic_10/0,
    metadata_request_11/0,
    metadata_request_topic_11/0,
    metadata_request_12/0,
    metadata_request_topic_12/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(METADATA_REQUEST, 3).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_metadata_request_0(metadata_request_0()) -> iodata().

encode_metadata_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_metadata_request_topic_0/1)
    ];
encode_metadata_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, metadata_request_topic_0}
    }).

-spec decode_metadata_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_0(),
    Rest :: binary().

decode_metadata_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_0)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_metadata_request_topic_0(metadata_request_topic_0()) -> iodata().

encode_metadata_request_topic_0(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_0(),
    Rest :: binary().

decode_metadata_request_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_1(metadata_request_1()) -> iodata().

encode_metadata_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 1, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_metadata_request_topic_1/1)
    ];
encode_metadata_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_1}
    }).

-spec decode_metadata_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_1(),
    Rest :: binary().

decode_metadata_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_1)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_metadata_request_topic_1(metadata_request_topic_1()) -> iodata().

encode_metadata_request_topic_1(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_1(),
    Rest :: binary().

decode_metadata_request_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_2(metadata_request_2()) -> iodata().

encode_metadata_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 2, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_metadata_request_topic_2/1)
    ];
encode_metadata_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_2}
    }).

-spec decode_metadata_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_2(),
    Rest :: binary().

decode_metadata_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_2)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_metadata_request_topic_2(metadata_request_topic_2()) -> iodata().

encode_metadata_request_topic_2(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_2(),
    Rest :: binary().

decode_metadata_request_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_3(metadata_request_3()) -> iodata().

encode_metadata_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 3, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_metadata_request_topic_3/1)
    ];
encode_metadata_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_3}
    }).

-spec decode_metadata_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_3(),
    Rest :: binary().

decode_metadata_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_3)),
    {
        Header#{
            topics => Topics
        },
        Bin1
    }.

-spec encode_metadata_request_topic_3(metadata_request_topic_3()) -> iodata().

encode_metadata_request_topic_3(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_3(),
    Rest :: binary().

decode_metadata_request_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_4(metadata_request_4()) -> iodata().

encode_metadata_request_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 4, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_metadata_request_topic_4/1),
        ?encode_bool(AllowAutoTopicCreation)
    ];
encode_metadata_request_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_4},
        allow_auto_topic_creation => bool
    }).

-spec decode_metadata_request_4(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_4(),
    Rest :: binary().

decode_metadata_request_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_4)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    {
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation
        },
        Bin2
    }.

-spec encode_metadata_request_topic_4(metadata_request_topic_4()) -> iodata().

encode_metadata_request_topic_4(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_4(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_4(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_4(),
    Rest :: binary().

decode_metadata_request_topic_4(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_5(metadata_request_5()) -> iodata().

encode_metadata_request_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 5, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_metadata_request_topic_5/1),
        ?encode_bool(AllowAutoTopicCreation)
    ];
encode_metadata_request_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_5},
        allow_auto_topic_creation => bool
    }).

-spec decode_metadata_request_5(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_5(),
    Rest :: binary().

decode_metadata_request_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_5)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    {
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation
        },
        Bin2
    }.

-spec encode_metadata_request_topic_5(metadata_request_topic_5()) -> iodata().

encode_metadata_request_topic_5(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_5(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_5(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_5(),
    Rest :: binary().

decode_metadata_request_topic_5(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_6(metadata_request_6()) -> iodata().

encode_metadata_request_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 6, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_metadata_request_topic_6/1),
        ?encode_bool(AllowAutoTopicCreation)
    ];
encode_metadata_request_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_6},
        allow_auto_topic_creation => bool
    }).

-spec decode_metadata_request_6(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_6(),
    Rest :: binary().

decode_metadata_request_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_6)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    {
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation
        },
        Bin2
    }.

-spec encode_metadata_request_topic_6(metadata_request_topic_6()) -> iodata().

encode_metadata_request_topic_6(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_6(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_6(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_6(),
    Rest :: binary().

decode_metadata_request_topic_6(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_7(metadata_request_7()) -> iodata().

encode_metadata_request_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 7, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_metadata_request_topic_7/1),
        ?encode_bool(AllowAutoTopicCreation)
    ];
encode_metadata_request_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_7},
        allow_auto_topic_creation => bool
    }).

-spec decode_metadata_request_7(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_7(),
    Rest :: binary().

decode_metadata_request_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_7)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    {
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation
        },
        Bin2
    }.

-spec encode_metadata_request_topic_7(metadata_request_topic_7()) -> iodata().

encode_metadata_request_topic_7(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_7(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_7(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_7(),
    Rest :: binary().

decode_metadata_request_topic_7(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_8(metadata_request_8()) -> iodata().

encode_metadata_request_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation,
        % Whether to include cluster authorized operations.
        include_cluster_authorized_operations := IncludeClusterAuthorizedOperations,
        % Whether to include topic authorized operations.
        include_topic_authorized_operations := IncludeTopicAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation),
    ?is_bool(IncludeClusterAuthorizedOperations),
    ?is_bool(IncludeTopicAuthorizedOperations)
->
    [
        ?encode_request_header_1(?METADATA_REQUEST, 8, CorrelationId, ClientId),
        ?encode_nullable_array(Topics, fun encode_metadata_request_topic_8/1),
        ?encode_bool(AllowAutoTopicCreation),
        ?encode_bool(IncludeClusterAuthorizedOperations),
        ?encode_bool(IncludeTopicAuthorizedOperations)
    ];
encode_metadata_request_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_8},
        allow_auto_topic_creation => bool,
        include_cluster_authorized_operations => bool,
        include_topic_authorized_operations => bool
    }).

-spec decode_metadata_request_8(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_8(),
    Rest :: binary().

decode_metadata_request_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_8)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    ?_decode_bool(IncludeClusterAuthorizedOperations, Bin2, Bin3),
    ?_decode_bool(IncludeTopicAuthorizedOperations, Bin3, Bin4),
    {
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation,
            include_cluster_authorized_operations => IncludeClusterAuthorizedOperations,
            include_topic_authorized_operations => IncludeTopicAuthorizedOperations
        },
        Bin4
    }.

-spec encode_metadata_request_topic_8(metadata_request_topic_8()) -> iodata().

encode_metadata_request_topic_8(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_string(Name)
    ];
encode_metadata_request_topic_8(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_8(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_8(),
    Rest :: binary().

decode_metadata_request_topic_8(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    {
        #{
            name => Name
        },
        Bin1
    }.

-spec encode_metadata_request_9(metadata_request_9()) -> iodata().

encode_metadata_request_9(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation,
        % Whether to include cluster authorized operations.
        include_cluster_authorized_operations := IncludeClusterAuthorizedOperations,
        % Whether to include topic authorized operations.
        include_topic_authorized_operations := IncludeTopicAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation),
    ?is_bool(IncludeClusterAuthorizedOperations),
    ?is_bool(IncludeTopicAuthorizedOperations)
->
    [
        ?encode_request_header_2(?METADATA_REQUEST, 9, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Topics, fun encode_metadata_request_topic_9/1),
        ?encode_bool(AllowAutoTopicCreation),
        ?encode_bool(IncludeClusterAuthorizedOperations),
        ?encode_bool(IncludeTopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_request_9(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_9},
        allow_auto_topic_creation => bool,
        include_cluster_authorized_operations => bool,
        include_topic_authorized_operations => bool
    }).

-spec decode_metadata_request_9(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_9(),
    Rest :: binary().

decode_metadata_request_9(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_9)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    ?_decode_bool(IncludeClusterAuthorizedOperations, Bin2, Bin3),
    ?_decode_bool(IncludeTopicAuthorizedOperations, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_metadata_request_9_tagged_field/3,
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation,
            include_cluster_authorized_operations => IncludeClusterAuthorizedOperations,
            include_topic_authorized_operations => IncludeTopicAuthorizedOperations
        },
        Bin4
    ).

-spec decode_metadata_request_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: metadata_request_9().

decode_metadata_request_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_request_topic_9(metadata_request_topic_9()) -> iodata().

encode_metadata_request_topic_9(
    _Args = #{
        % The topic name.
        name := Name
    }
) when
    ?is_string(Name)
->
    [
        ?encode_compact_string(Name),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_request_topic_9(Args) ->
    ?encoder_error(Args, #{
        name => string
    }).

-spec decode_metadata_request_topic_9(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_9(),
    Rest :: binary().

decode_metadata_request_topic_9(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_metadata_request_topic_9_tagged_field/3,
        #{
            name => Name
        },
        Bin1
    ).

-spec decode_metadata_request_topic_9_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: metadata_request_topic_9().

decode_metadata_request_topic_9_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_request_10(metadata_request_10()) -> iodata().

encode_metadata_request_10(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation,
        % Whether to include cluster authorized operations.
        include_cluster_authorized_operations := IncludeClusterAuthorizedOperations,
        % Whether to include topic authorized operations.
        include_topic_authorized_operations := IncludeTopicAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation),
    ?is_bool(IncludeClusterAuthorizedOperations),
    ?is_bool(IncludeTopicAuthorizedOperations)
->
    [
        ?encode_request_header_2(?METADATA_REQUEST, 10, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Topics, fun encode_metadata_request_topic_10/1),
        ?encode_bool(AllowAutoTopicCreation),
        ?encode_bool(IncludeClusterAuthorizedOperations),
        ?encode_bool(IncludeTopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_request_10(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_10},
        allow_auto_topic_creation => bool,
        include_cluster_authorized_operations => bool,
        include_topic_authorized_operations => bool
    }).

-spec decode_metadata_request_10(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_10(),
    Rest :: binary().

decode_metadata_request_10(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_10)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    ?_decode_bool(IncludeClusterAuthorizedOperations, Bin2, Bin3),
    ?_decode_bool(IncludeTopicAuthorizedOperations, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_metadata_request_10_tagged_field/3,
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation,
            include_cluster_authorized_operations => IncludeClusterAuthorizedOperations,
            include_topic_authorized_operations => IncludeTopicAuthorizedOperations
        },
        Bin4
    ).

-spec decode_metadata_request_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: metadata_request_10().

decode_metadata_request_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_request_topic_10(metadata_request_topic_10()) -> iodata().

encode_metadata_request_topic_10(
    _Args = #{
        % The topic id.
        topic_id := TopicId,
        % The topic name.
        name := Name
    }
) when
    ?is_uuid(TopicId),
    ?is_nullable_string(Name)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_nullable_string(Name),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_request_topic_10(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        name => nullable_string
    }).

-spec decode_metadata_request_topic_10(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_10(),
    Rest :: binary().

decode_metadata_request_topic_10(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_nullable_string(Name, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_metadata_request_topic_10_tagged_field/3,
        #{
            topic_id => TopicId,
            name => Name
        },
        Bin2
    ).

-spec decode_metadata_request_topic_10_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: metadata_request_topic_10().

decode_metadata_request_topic_10_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_request_11(metadata_request_11()) -> iodata().

encode_metadata_request_11(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation,
        % Whether to include topic authorized operations.
        include_topic_authorized_operations := IncludeTopicAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation),
    ?is_bool(IncludeTopicAuthorizedOperations)
->
    [
        ?encode_request_header_2(?METADATA_REQUEST, 11, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Topics, fun encode_metadata_request_topic_11/1),
        ?encode_bool(AllowAutoTopicCreation),
        ?encode_bool(IncludeTopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_request_11(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_11},
        allow_auto_topic_creation => bool,
        include_topic_authorized_operations => bool
    }).

-spec decode_metadata_request_11(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_11(),
    Rest :: binary().

decode_metadata_request_11(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_11)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    ?_decode_bool(IncludeTopicAuthorizedOperations, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_metadata_request_11_tagged_field/3,
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation,
            include_topic_authorized_operations => IncludeTopicAuthorizedOperations
        },
        Bin3
    ).

-spec decode_metadata_request_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: metadata_request_11().

decode_metadata_request_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_request_topic_11(metadata_request_topic_11()) -> iodata().

encode_metadata_request_topic_11(
    _Args = #{
        % The topic id.
        topic_id := TopicId,
        % The topic name.
        name := Name
    }
) when
    ?is_uuid(TopicId),
    ?is_nullable_string(Name)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_nullable_string(Name),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_request_topic_11(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        name => nullable_string
    }).

-spec decode_metadata_request_topic_11(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_11(),
    Rest :: binary().

decode_metadata_request_topic_11(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_nullable_string(Name, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_metadata_request_topic_11_tagged_field/3,
        #{
            topic_id => TopicId,
            name => Name
        },
        Bin2
    ).

-spec decode_metadata_request_topic_11_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: metadata_request_topic_11().

decode_metadata_request_topic_11_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_request_12(metadata_request_12()) -> iodata().

encode_metadata_request_12(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The topics to fetch metadata for.
        topics := Topics,
        % If this is true, the broker may auto-create topics that we requested which do not already exist, if it is configured to do so.
        allow_auto_topic_creation := AllowAutoTopicCreation,
        % Whether to include topic authorized operations.
        include_topic_authorized_operations := IncludeTopicAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_nullable_array(Topics),
    ?is_bool(AllowAutoTopicCreation),
    ?is_bool(IncludeTopicAuthorizedOperations)
->
    [
        ?encode_request_header_2(?METADATA_REQUEST, 12, CorrelationId, ClientId),
        ?encode_compact_nullable_array(Topics, fun encode_metadata_request_topic_12/1),
        ?encode_bool(AllowAutoTopicCreation),
        ?encode_bool(IncludeTopicAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_request_12(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {nullable_array, metadata_request_topic_12},
        allow_auto_topic_creation => bool,
        include_topic_authorized_operations => bool
    }).

-spec decode_metadata_request_12(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_12(),
    Rest :: binary().

decode_metadata_request_12(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_nullable_array(Topics, Bin0, Bin1, ?_decode_element(decode_metadata_request_topic_12)),
    ?_decode_bool(AllowAutoTopicCreation, Bin1, Bin2),
    ?_decode_bool(IncludeTopicAuthorizedOperations, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_metadata_request_12_tagged_field/3,
        Header#{
            topics => Topics,
            allow_auto_topic_creation => AllowAutoTopicCreation,
            include_topic_authorized_operations => IncludeTopicAuthorizedOperations
        },
        Bin3
    ).

-spec decode_metadata_request_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: metadata_request_12().

decode_metadata_request_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_metadata_request_topic_12(metadata_request_topic_12()) -> iodata().

encode_metadata_request_topic_12(
    _Args = #{
        % The topic id.
        topic_id := TopicId,
        % The topic name.
        name := Name
    }
) when
    ?is_uuid(TopicId),
    ?is_nullable_string(Name)
->
    [
        ?encode_uuid(TopicId),
        ?encode_compact_nullable_string(Name),
        ?EMPTY_TAG_BUFFER
    ];
encode_metadata_request_topic_12(Args) ->
    ?encoder_error(Args, #{
        topic_id => uuid,
        name => nullable_string
    }).

-spec decode_metadata_request_topic_12(binary()) -> {Decoded, Rest} when
    Decoded :: metadata_request_topic_12(),
    Rest :: binary().

decode_metadata_request_topic_12(Bin0) when is_binary(Bin0) ->
    ?_decode_uuid(TopicId, Bin0, Bin1),
    ?_decode_compact_nullable_string(Name, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_metadata_request_topic_12_tagged_field/3,
        #{
            topic_id => TopicId,
            name => Name
        },
        Bin2
    ).

-spec decode_metadata_request_topic_12_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: metadata_request_topic_12().

decode_metadata_request_topic_12_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type metadata_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_0())
}.
-type metadata_request_topic_0() :: #{
    name := binary()
}.
-type metadata_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_1()) | null
}.
-type metadata_request_topic_1() :: #{
    name := binary()
}.
-type metadata_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_2()) | null
}.
-type metadata_request_topic_2() :: #{
    name := binary()
}.
-type metadata_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_3()) | null
}.
-type metadata_request_topic_3() :: #{
    name := binary()
}.
-type metadata_request_4() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_4()) | null,
    allow_auto_topic_creation := boolean()
}.
-type metadata_request_topic_4() :: #{
    name := binary()
}.
-type metadata_request_5() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_5()) | null,
    allow_auto_topic_creation := boolean()
}.
-type metadata_request_topic_5() :: #{
    name := binary()
}.
-type metadata_request_6() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_6()) | null,
    allow_auto_topic_creation := boolean()
}.
-type metadata_request_topic_6() :: #{
    name := binary()
}.
-type metadata_request_7() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_7()) | null,
    allow_auto_topic_creation := boolean()
}.
-type metadata_request_topic_7() :: #{
    name := binary()
}.
-type metadata_request_8() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_8()) | null,
    allow_auto_topic_creation := boolean(),
    include_cluster_authorized_operations := boolean(),
    include_topic_authorized_operations := boolean()
}.
-type metadata_request_topic_8() :: #{
    name := binary()
}.
-type metadata_request_9() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_9()) | null,
    allow_auto_topic_creation := boolean(),
    include_cluster_authorized_operations := boolean(),
    include_topic_authorized_operations := boolean()
}.
-type metadata_request_topic_9() :: #{
    name := binary()
}.
-type metadata_request_10() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_10()) | null,
    allow_auto_topic_creation := boolean(),
    include_cluster_authorized_operations := boolean(),
    include_topic_authorized_operations := boolean()
}.
-type metadata_request_topic_10() :: #{
    topic_id := kafcod:uuid(),
    name := binary() | null
}.
-type metadata_request_11() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_11()) | null,
    allow_auto_topic_creation := boolean(),
    include_topic_authorized_operations := boolean()
}.
-type metadata_request_topic_11() :: #{
    topic_id := kafcod:uuid(),
    name := binary() | null
}.
-type metadata_request_12() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(metadata_request_topic_12()) | null,
    allow_auto_topic_creation := boolean(),
    include_topic_authorized_operations := boolean()
}.
-type metadata_request_topic_12() :: #{
    topic_id := kafcod:uuid(),
    name := binary() | null
}.
