-module(consumer_group_describe_request).
-export([
    encode_consumer_group_describe_request_0/1,
    decode_consumer_group_describe_request_0/1
]).
-export_type([
    consumer_group_describe_request_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(CONSUMER_GROUP_DESCRIBE_REQUEST, 69).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_consumer_group_describe_request_0(consumer_group_describe_request_0()) -> iodata().

encode_consumer_group_describe_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ids of the groups to describe
        group_ids := GroupIds,
        % Whether to include authorized operations.
        include_authorized_operations := IncludeAuthorizedOperations
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_array(GroupIds),
    ?is_bool(IncludeAuthorizedOperations)
->
    [
        ?encode_request_header_2(?CONSUMER_GROUP_DESCRIBE_REQUEST, 0, CorrelationId, ClientId),
        ?encode_compact_array(GroupIds, ?encode_compact_string_),
        ?encode_bool(IncludeAuthorizedOperations),
        ?EMPTY_TAG_BUFFER
    ];
encode_consumer_group_describe_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        group_ids => {array, string},
        include_authorized_operations => bool
    }).

-spec decode_consumer_group_describe_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: consumer_group_describe_request_0(),
    Rest :: binary().

decode_consumer_group_describe_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_compact_array(GroupIds, Bin0, Bin1, ?decode_string_),
    ?_decode_bool(IncludeAuthorizedOperations, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_consumer_group_describe_request_0_tagged_field/3,
        Header#{
            group_ids => GroupIds,
            include_authorized_operations => IncludeAuthorizedOperations
        },
        Bin2
    ).

-spec decode_consumer_group_describe_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_consumer_group_describe_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type consumer_group_describe_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    group_ids := list(binary()),
    include_authorized_operations := boolean()
}.
