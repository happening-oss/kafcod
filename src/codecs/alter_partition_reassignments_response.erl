-module(alter_partition_reassignments_response).
-export([
    encode_alter_partition_reassignments_response_0/1,
    decode_alter_partition_reassignments_response_0/1
]).
-export_type([
    alter_partition_reassignments_response_0/0,
    reassignable_partition_response_0/0,
    reassignable_topic_response_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_alter_partition_reassignments_response_0(alter_partition_reassignments_response_0()) -> iodata().

encode_alter_partition_reassignments_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top-level error code, or 0 if there was no error.
        error_code := ErrorCode,
        % The top-level error message, or null if there was no error.
        error_message := ErrorMessage,
        % The responses to topics to reassign.
        responses := Responses
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage),
    ?is_array(Responses)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?encode_compact_array(Responses, fun encode_reassignable_topic_response_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_alter_partition_reassignments_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        error_message => nullable_string,
        responses => {array, reassignable_topic_response_0}
    }).

-spec decode_alter_partition_reassignments_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: alter_partition_reassignments_response_0(),
    Rest :: binary().

decode_alter_partition_reassignments_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?_decode_compact_array(Responses, Bin3, Bin4, ?_decode_element(decode_reassignable_topic_response_0)),
    ?decode_tagged_fields(
        fun decode_alter_partition_reassignments_response_0_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            error_message => ErrorMessage,
            responses => Responses
        },
        Bin4
    ).

-spec decode_alter_partition_reassignments_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: alter_partition_reassignments_response_0().

decode_alter_partition_reassignments_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_reassignable_partition_response_0(reassignable_partition_response_0()) -> iodata().

encode_reassignable_partition_response_0(
    _Args = #{
        % The partition index.
        partition_index := PartitionIndex,
        % The error code for this partition, or 0 if there was no error.
        error_code := ErrorCode,
        % The error message for this partition, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_int32(PartitionIndex),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_int32(PartitionIndex),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_reassignable_partition_response_0(Args) ->
    ?encoder_error(Args, #{
        partition_index => int32,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_reassignable_partition_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: reassignable_partition_response_0(),
    Rest :: binary().

decode_reassignable_partition_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionIndex, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_reassignable_partition_response_0_tagged_field/3,
        #{
            partition_index => PartitionIndex,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    ).

-spec decode_reassignable_partition_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: reassignable_partition_response_0().

decode_reassignable_partition_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_reassignable_topic_response_0(reassignable_topic_response_0()) -> iodata().

encode_reassignable_topic_response_0(
    _Args = #{
        % The topic name
        name := Name,
        % The responses to partitions to reassign
        partitions := Partitions
    }
) when
    ?is_string(Name),
    ?is_array(Partitions)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_array(Partitions, fun encode_reassignable_partition_response_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_reassignable_topic_response_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        partitions => {array, reassignable_partition_response_0}
    }).

-spec decode_reassignable_topic_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: reassignable_topic_response_0(),
    Rest :: binary().

decode_reassignable_topic_response_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_array(Partitions, Bin1, Bin2, ?_decode_element(decode_reassignable_partition_response_0)),
    ?decode_tagged_fields(
        fun decode_reassignable_topic_response_0_tagged_field/3,
        #{
            name => Name,
            partitions => Partitions
        },
        Bin2
    ).

-spec decode_reassignable_topic_response_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: reassignable_topic_response_0().

decode_reassignable_topic_response_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type alter_partition_reassignments_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    error_message := binary() | null,
    responses := list(reassignable_topic_response_0())
}.
-type reassignable_partition_response_0() :: #{
    partition_index := integer(),
    error_code := integer(),
    error_message := binary() | null
}.
-type reassignable_topic_response_0() :: #{
    name := binary(),
    partitions := list(reassignable_partition_response_0())
}.
