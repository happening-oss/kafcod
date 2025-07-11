-module(describe_cluster_request).
-export([
    encode_describe_cluster_request_0/1,
    decode_describe_cluster_request_0/1,
    encode_describe_cluster_request_1/1,
    decode_describe_cluster_request_1/1
]).
-export_type([
    describe_cluster_request_0/0,
    describe_cluster_request_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(DESCRIBE_CLUSTER_REQUEST, 60).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_describe_cluster_request_0(describe_cluster_request_0()) -> iodata().

encode_describe_cluster_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Whether to include cluster authorized operations.
        include_cluster_authorized_operations := IncludeClusterAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bool(IncludeClusterAuthorizedOperations)
->
    [
        ?encode_request_header_2(?DESCRIBE_CLUSTER_REQUEST, 0, CorrelationId, ClientId),
        ?encode_bool(IncludeClusterAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_cluster_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        include_cluster_authorized_operations => bool
    }).

-spec decode_describe_cluster_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: describe_cluster_request_0(),
    Rest :: binary().

decode_describe_cluster_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_bool(IncludeClusterAuthorizedOperations, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_describe_cluster_request_0_tagged_field/3,
        Header#{
            include_cluster_authorized_operations => IncludeClusterAuthorizedOperations
        },
        Bin1
    ).

-spec decode_describe_cluster_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_cluster_request_0().

decode_describe_cluster_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_describe_cluster_request_1(describe_cluster_request_1()) -> iodata().

encode_describe_cluster_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % Whether to include cluster authorized operations.
        include_cluster_authorized_operations := IncludeClusterAuthorizedOperations,
        % The endpoint type to describe. 1=brokers, 2=controllers.
        endpoint_type := EndpointType
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_bool(IncludeClusterAuthorizedOperations),
    ?is_int8(EndpointType)
->
    [
        ?encode_request_header_2(?DESCRIBE_CLUSTER_REQUEST, 1, CorrelationId, ClientId),
        ?encode_bool(IncludeClusterAuthorizedOperations),
        ?encode_int8(EndpointType),
        ?EMPTY_TAG_BUFFER
    ];
encode_describe_cluster_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        include_cluster_authorized_operations => bool,
        endpoint_type => int8
    }).

-spec decode_describe_cluster_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: describe_cluster_request_1(),
    Rest :: binary().

decode_describe_cluster_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_bool(IncludeClusterAuthorizedOperations, Bin0, Bin1),
    ?_decode_int8(EndpointType, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_describe_cluster_request_1_tagged_field/3,
        Header#{
            include_cluster_authorized_operations => IncludeClusterAuthorizedOperations,
            endpoint_type => EndpointType
        },
        Bin2
    ).

-spec decode_describe_cluster_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: describe_cluster_request_1().

decode_describe_cluster_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type describe_cluster_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    include_cluster_authorized_operations := boolean()
}.
-type describe_cluster_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    include_cluster_authorized_operations := boolean(),
    endpoint_type := integer()
}.
