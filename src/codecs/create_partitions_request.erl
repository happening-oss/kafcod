-module(create_partitions_request).
-export([
    encode_create_partitions_request_0/1,
    decode_create_partitions_request_0/1,
    encode_create_partitions_request_1/1,
    decode_create_partitions_request_1/1,
    encode_create_partitions_request_2/1,
    decode_create_partitions_request_2/1,
    encode_create_partitions_request_3/1,
    decode_create_partitions_request_3/1
]).
-export_type([
    create_partitions_request_0/0,
    create_partitions_assignment_0/0,
    create_partitions_topic_0/0,
    create_partitions_request_1/0,
    create_partitions_assignment_1/0,
    create_partitions_topic_1/0,
    create_partitions_request_2/0,
    create_partitions_assignment_2/0,
    create_partitions_topic_2/0,
    create_partitions_request_3/0,
    create_partitions_assignment_3/0,
    create_partitions_topic_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(CREATE_PARTITIONS_REQUEST, 37).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_create_partitions_request_0(create_partitions_request_0()) -> iodata().

encode_create_partitions_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to create new partitions inside.
        topics := Topics,
        % The time in ms to wait for the partitions to be created.
        timeout_ms := TimeoutMs,
        % If true, then validate the request, but don't actually increase the number of partitions.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_1(?CREATE_PARTITIONS_REQUEST, 0, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_create_partitions_topic_0/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly)
    ];
encode_create_partitions_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, create_partitions_topic_0},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_partitions_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_request_0(),
    Rest :: binary().

decode_create_partitions_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_create_partitions_topic_0)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    }.

-spec encode_create_partitions_assignment_0(create_partitions_assignment_0()) -> iodata().

encode_create_partitions_assignment_0(
    _Args = #{
        % The assigned broker IDs.
        broker_ids := BrokerIds
    }
) when
    ?is_array(BrokerIds)
->
    [
        ?encode_array(BrokerIds, ?encode_int32_)
    ];
encode_create_partitions_assignment_0(Args) ->
    ?encoder_error(Args, #{
        broker_ids => {array, int32}
    }).

-spec decode_create_partitions_assignment_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_assignment_0(),
    Rest :: binary().

decode_create_partitions_assignment_0(Bin0) when is_binary(Bin0) ->
    ?_decode_array(BrokerIds, Bin0, Bin1, ?decode_int32_),
    {
        #{
            broker_ids => BrokerIds
        },
        Bin1
    }.

-spec encode_create_partitions_topic_0(create_partitions_topic_0()) -> iodata().

encode_create_partitions_topic_0(
    _Args = #{
        % The topic name.
        name := Name,
        % The new partition count.
        count := Count,
        % The new partition assignments.
        assignments := Assignments
    }
) when
    ?is_string(Name),
    ?is_int32(Count),
    ?is_nullable_array(Assignments)
->
    [
        ?encode_string(Name),
        ?encode_int32(Count),
        ?encode_nullable_array(Assignments, fun encode_create_partitions_assignment_0/1)
    ];
encode_create_partitions_topic_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        count => int32,
        assignments => {nullable_array, create_partitions_assignment_0}
    }).

-spec decode_create_partitions_topic_0(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_topic_0(),
    Rest :: binary().

decode_create_partitions_topic_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int32(Count, Bin1, Bin2),
    ?_decode_nullable_array(Assignments, Bin2, Bin3, ?_decode_element(decode_create_partitions_assignment_0)),
    {
        #{
            name => Name,
            count => Count,
            assignments => Assignments
        },
        Bin3
    }.

-spec encode_create_partitions_request_1(create_partitions_request_1()) -> iodata().

encode_create_partitions_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to create new partitions inside.
        topics := Topics,
        % The time in ms to wait for the partitions to be created.
        timeout_ms := TimeoutMs,
        % If true, then validate the request, but don't actually increase the number of partitions.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_1(?CREATE_PARTITIONS_REQUEST, 1, CorrelationId, ClientId),
        ?encode_array(Topics, fun encode_create_partitions_topic_1/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly)
    ];
encode_create_partitions_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, create_partitions_topic_1},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_partitions_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_request_1(),
    Rest :: binary().

decode_create_partitions_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_1(Bin),
    ?_decode_array(Topics, Bin0, Bin1, ?_decode_element(decode_create_partitions_topic_1)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    {
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    }.

-spec encode_create_partitions_assignment_1(create_partitions_assignment_1()) -> iodata().

encode_create_partitions_assignment_1(
    _Args = #{
        % The assigned broker IDs.
        broker_ids := BrokerIds
    }
) when
    ?is_array(BrokerIds)
->
    [
        ?encode_array(BrokerIds, ?encode_int32_)
    ];
encode_create_partitions_assignment_1(Args) ->
    ?encoder_error(Args, #{
        broker_ids => {array, int32}
    }).

-spec decode_create_partitions_assignment_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_assignment_1(),
    Rest :: binary().

decode_create_partitions_assignment_1(Bin0) when is_binary(Bin0) ->
    ?_decode_array(BrokerIds, Bin0, Bin1, ?decode_int32_),
    {
        #{
            broker_ids => BrokerIds
        },
        Bin1
    }.

-spec encode_create_partitions_topic_1(create_partitions_topic_1()) -> iodata().

encode_create_partitions_topic_1(
    _Args = #{
        % The topic name.
        name := Name,
        % The new partition count.
        count := Count,
        % The new partition assignments.
        assignments := Assignments
    }
) when
    ?is_string(Name),
    ?is_int32(Count),
    ?is_nullable_array(Assignments)
->
    [
        ?encode_string(Name),
        ?encode_int32(Count),
        ?encode_nullable_array(Assignments, fun encode_create_partitions_assignment_1/1)
    ];
encode_create_partitions_topic_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        count => int32,
        assignments => {nullable_array, create_partitions_assignment_1}
    }).

-spec decode_create_partitions_topic_1(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_topic_1(),
    Rest :: binary().

decode_create_partitions_topic_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Name, Bin0, Bin1),
    ?_decode_int32(Count, Bin1, Bin2),
    ?_decode_nullable_array(Assignments, Bin2, Bin3, ?_decode_element(decode_create_partitions_assignment_1)),
    {
        #{
            name => Name,
            count => Count,
            assignments => Assignments
        },
        Bin3
    }.

-spec encode_create_partitions_request_2(create_partitions_request_2()) -> iodata().

encode_create_partitions_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to create new partitions inside.
        topics := Topics,
        % The time in ms to wait for the partitions to be created.
        timeout_ms := TimeoutMs,
        % If true, then validate the request, but don't actually increase the number of partitions.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_2(?CREATE_PARTITIONS_REQUEST, 2, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_create_partitions_topic_2/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_partitions_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, create_partitions_topic_2},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_partitions_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_request_2(),
    Rest :: binary().

decode_create_partitions_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_create_partitions_topic_2)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_create_partitions_request_2_tagged_field/3,
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    ).

-spec decode_create_partitions_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_partitions_assignment_2(create_partitions_assignment_2()) -> iodata().

encode_create_partitions_assignment_2(
    _Args = #{
        % The assigned broker IDs.
        broker_ids := BrokerIds
    }
) when
    ?is_array(BrokerIds)
->
    [
        ?encode_compact_array(BrokerIds, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_partitions_assignment_2(Args) ->
    ?encoder_error(Args, #{
        broker_ids => {array, int32}
    }).

-spec decode_create_partitions_assignment_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_assignment_2(),
    Rest :: binary().

decode_create_partitions_assignment_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_array(BrokerIds, Bin0, Bin1, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_create_partitions_assignment_2_tagged_field/3,
        #{
            broker_ids => BrokerIds
        },
        Bin1
    ).

-spec decode_create_partitions_assignment_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_assignment_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_partitions_topic_2(create_partitions_topic_2()) -> iodata().

encode_create_partitions_topic_2(
    _Args = #{
        % The topic name.
        name := Name,
        % The new partition count.
        count := Count,
        % The new partition assignments.
        assignments := Assignments
    }
) when
    ?is_string(Name),
    ?is_int32(Count),
    ?is_nullable_array(Assignments)
->
    [
        ?encode_compact_string(Name),
        ?encode_int32(Count),
        ?encode_compact_nullable_array(Assignments, fun encode_create_partitions_assignment_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_partitions_topic_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        count => int32,
        assignments => {nullable_array, create_partitions_assignment_2}
    }).

-spec decode_create_partitions_topic_2(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_topic_2(),
    Rest :: binary().

decode_create_partitions_topic_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int32(Count, Bin1, Bin2),
    ?_decode_compact_nullable_array(Assignments, Bin2, Bin3, ?_decode_element(decode_create_partitions_assignment_2)),
    ?decode_tagged_fields(
        fun decode_create_partitions_topic_2_tagged_field/3,
        #{
            name => Name,
            count => Count,
            assignments => Assignments
        },
        Bin3
    ).

-spec decode_create_partitions_topic_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_topic_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_partitions_request_3(create_partitions_request_3()) -> iodata().

encode_create_partitions_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Each topic that we want to create new partitions inside.
        topics := Topics,
        % The time in ms to wait for the partitions to be created.
        timeout_ms := TimeoutMs,
        % If true, then validate the request, but don't actually increase the number of partitions.
        validate_only := ValidateOnly
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(Topics),
    ?is_int32(TimeoutMs),
    ?is_bool(ValidateOnly)
->
    [
        ?encode_request_header_2(?CREATE_PARTITIONS_REQUEST, 3, CorrelationId, ClientId),
        ?encode_compact_array(Topics, fun encode_create_partitions_topic_3/1),
        ?encode_int32(TimeoutMs),
        ?encode_bool(ValidateOnly),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_partitions_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        topics => {array, create_partitions_topic_3},
        timeout_ms => int32,
        validate_only => bool
    }).

-spec decode_create_partitions_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_request_3(),
    Rest :: binary().

decode_create_partitions_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(Topics, Bin0, Bin1, ?_decode_element(decode_create_partitions_topic_3)),
    ?_decode_int32(TimeoutMs, Bin1, Bin2),
    ?_decode_bool(ValidateOnly, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_create_partitions_request_3_tagged_field/3,
        Header#{
            topics => Topics,
            timeout_ms => TimeoutMs,
            validate_only => ValidateOnly
        },
        Bin3
    ).

-spec decode_create_partitions_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_partitions_assignment_3(create_partitions_assignment_3()) -> iodata().

encode_create_partitions_assignment_3(
    _Args = #{
        % The assigned broker IDs.
        broker_ids := BrokerIds
    }
) when
    ?is_array(BrokerIds)
->
    [
        ?encode_compact_array(BrokerIds, ?encode_int32_),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_partitions_assignment_3(Args) ->
    ?encoder_error(Args, #{
        broker_ids => {array, int32}
    }).

-spec decode_create_partitions_assignment_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_assignment_3(),
    Rest :: binary().

decode_create_partitions_assignment_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_array(BrokerIds, Bin0, Bin1, ?decode_int32_),
    ?decode_tagged_fields(
        fun decode_create_partitions_assignment_3_tagged_field/3,
        #{
            broker_ids => BrokerIds
        },
        Bin1
    ).

-spec decode_create_partitions_assignment_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_assignment_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_create_partitions_topic_3(create_partitions_topic_3()) -> iodata().

encode_create_partitions_topic_3(
    _Args = #{
        % The topic name.
        name := Name,
        % The new partition count.
        count := Count,
        % The new partition assignments.
        assignments := Assignments
    }
) when
    ?is_string(Name),
    ?is_int32(Count),
    ?is_nullable_array(Assignments)
->
    [
        ?encode_compact_string(Name),
        ?encode_int32(Count),
        ?encode_compact_nullable_array(Assignments, fun encode_create_partitions_assignment_3/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_create_partitions_topic_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        count => int32,
        assignments => {nullable_array, create_partitions_assignment_3}
    }).

-spec decode_create_partitions_topic_3(binary()) -> {Decoded, Rest} when
    Decoded :: create_partitions_topic_3(),
    Rest :: binary().

decode_create_partitions_topic_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int32(Count, Bin1, Bin2),
    ?_decode_compact_nullable_array(Assignments, Bin2, Bin3, ?_decode_element(decode_create_partitions_assignment_3)),
    ?decode_tagged_fields(
        fun decode_create_partitions_topic_3_tagged_field/3,
        #{
            name => Name,
            count => Count,
            assignments => Assignments
        },
        Bin3
    ).

-spec decode_create_partitions_topic_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_create_partitions_topic_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type create_partitions_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(create_partitions_topic_0()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type create_partitions_assignment_0() :: #{
    broker_ids := list(integer())
}.
-type create_partitions_topic_0() :: #{
    name := binary(),
    count := integer(),
    assignments := list(create_partitions_assignment_0()) | null
}.
-type create_partitions_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(create_partitions_topic_1()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type create_partitions_assignment_1() :: #{
    broker_ids := list(integer())
}.
-type create_partitions_topic_1() :: #{
    name := binary(),
    count := integer(),
    assignments := list(create_partitions_assignment_1()) | null
}.
-type create_partitions_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(create_partitions_topic_2()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type create_partitions_assignment_2() :: #{
    broker_ids := list(integer())
}.
-type create_partitions_topic_2() :: #{
    name := binary(),
    count := integer(),
    assignments := list(create_partitions_assignment_2()) | null
}.
-type create_partitions_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    topics := list(create_partitions_topic_3()),
    timeout_ms := integer(),
    validate_only := boolean()
}.
-type create_partitions_assignment_3() :: #{
    broker_ids := list(integer())
}.
-type create_partitions_topic_3() :: #{
    name := binary(),
    count := integer(),
    assignments := list(create_partitions_assignment_3()) | null
}.
