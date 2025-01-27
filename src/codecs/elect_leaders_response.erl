-module(elect_leaders_response).
-export([
    encode_elect_leaders_response_0/1,
    decode_elect_leaders_response_0/1,
    encode_elect_leaders_response_1/1,
    decode_elect_leaders_response_1/1,
    encode_elect_leaders_response_2/1,
    decode_elect_leaders_response_2/1
]).
-export_type([
    elect_leaders_response_0/0,
    partition_result_0/0,
    replica_election_result_0/0,
    elect_leaders_response_1/0,
    partition_result_1/0,
    replica_election_result_1/0,
    elect_leaders_response_2/0,
    partition_result_2/0,
    replica_election_result_2/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_elect_leaders_response_0(elect_leaders_response_0()) -> iodata().

encode_elect_leaders_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The election results, or an empty array if the requester did not have permission and the request asks for all partitions.
        replica_election_results := ReplicaElectionResults
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_array(ReplicaElectionResults)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_array(ReplicaElectionResults, fun encode_replica_election_result_0/1)
    ];
encode_elect_leaders_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        replica_election_results => {array, replica_election_result_0}
    }).

-spec decode_elect_leaders_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: elect_leaders_response_0(),
    Rest :: binary().

decode_elect_leaders_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_array(ReplicaElectionResults, Bin1, Bin2, ?_decode_element(decode_replica_election_result_0)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            replica_election_results => ReplicaElectionResults
        },
        Bin2
    }.

-spec encode_partition_result_0(partition_result_0()) -> iodata().

encode_partition_result_0(
    _Args = #{
        % The partition id
        partition_id := PartitionId,
        % The result error, or zero if there was no error.
        error_code := ErrorCode,
        % The result message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_int32(PartitionId),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_int32(PartitionId),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_partition_result_0(Args) ->
    ?encoder_error(Args, #{
        partition_id => int32,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_partition_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: partition_result_0(),
    Rest :: binary().

decode_partition_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionId, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    {
        #{
            partition_id => PartitionId,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    }.

-spec encode_replica_election_result_0(replica_election_result_0()) -> iodata().

encode_replica_election_result_0(
    _Args = #{
        % The topic name
        topic := Topic,
        % The results for each partition
        partition_result := PartitionResult
    }
) when
    ?is_string(Topic),
    ?is_array(PartitionResult)
->
    [
        ?encode_string(Topic),
        ?encode_array(PartitionResult, fun encode_partition_result_0/1)
    ];
encode_replica_election_result_0(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partition_result => {array, partition_result_0}
    }).

-spec decode_replica_election_result_0(binary()) -> {Decoded, Rest} when
    Decoded :: replica_election_result_0(),
    Rest :: binary().

decode_replica_election_result_0(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(PartitionResult, Bin1, Bin2, ?_decode_element(decode_partition_result_0)),
    {
        #{
            topic => Topic,
            partition_result => PartitionResult
        },
        Bin2
    }.

-spec encode_elect_leaders_response_1(elect_leaders_response_1()) -> iodata().

encode_elect_leaders_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The election results, or an empty array if the requester did not have permission and the request asks for all partitions.
        replica_election_results := ReplicaElectionResults
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(ReplicaElectionResults)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_array(ReplicaElectionResults, fun encode_replica_election_result_1/1)
    ];
encode_elect_leaders_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        replica_election_results => {array, replica_election_result_1}
    }).

-spec decode_elect_leaders_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: elect_leaders_response_1(),
    Rest :: binary().

decode_elect_leaders_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_array(ReplicaElectionResults, Bin2, Bin3, ?_decode_element(decode_replica_election_result_1)),
    {
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            replica_election_results => ReplicaElectionResults
        },
        Bin3
    }.

-spec encode_partition_result_1(partition_result_1()) -> iodata().

encode_partition_result_1(
    _Args = #{
        % The partition id
        partition_id := PartitionId,
        % The result error, or zero if there was no error.
        error_code := ErrorCode,
        % The result message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_int32(PartitionId),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_int32(PartitionId),
        ?encode_int16(ErrorCode),
        ?encode_nullable_string(ErrorMessage)
    ];
encode_partition_result_1(Args) ->
    ?encoder_error(Args, #{
        partition_id => int32,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_partition_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: partition_result_1(),
    Rest :: binary().

decode_partition_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionId, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_nullable_string(ErrorMessage, Bin2, Bin3),
    {
        #{
            partition_id => PartitionId,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    }.

-spec encode_replica_election_result_1(replica_election_result_1()) -> iodata().

encode_replica_election_result_1(
    _Args = #{
        % The topic name
        topic := Topic,
        % The results for each partition
        partition_result := PartitionResult
    }
) when
    ?is_string(Topic),
    ?is_array(PartitionResult)
->
    [
        ?encode_string(Topic),
        ?encode_array(PartitionResult, fun encode_partition_result_1/1)
    ];
encode_replica_election_result_1(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partition_result => {array, partition_result_1}
    }).

-spec decode_replica_election_result_1(binary()) -> {Decoded, Rest} when
    Decoded :: replica_election_result_1(),
    Rest :: binary().

decode_replica_election_result_1(Bin0) when is_binary(Bin0) ->
    ?_decode_string(Topic, Bin0, Bin1),
    ?_decode_array(PartitionResult, Bin1, Bin2, ?_decode_element(decode_partition_result_1)),
    {
        #{
            topic => Topic,
            partition_result => PartitionResult
        },
        Bin2
    }.

-spec encode_elect_leaders_response_2(elect_leaders_response_2()) -> iodata().

encode_elect_leaders_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The duration in milliseconds for which the request was throttled due to a quota violation, or zero if the request did not violate any quota.
        throttle_time_ms := ThrottleTimeMs,
        % The top level response error code.
        error_code := ErrorCode,
        % The election results, or an empty array if the requester did not have permission and the request asks for all partitions.
        replica_election_results := ReplicaElectionResults
    }
) when
    ?is_int32(CorrelationId),
    ?is_int32(ThrottleTimeMs),
    ?is_int16(ErrorCode),
    ?is_array(ReplicaElectionResults)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int32(ThrottleTimeMs),
        ?encode_int16(ErrorCode),
        ?encode_compact_array(ReplicaElectionResults, fun encode_replica_election_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_elect_leaders_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        throttle_time_ms => int32,
        error_code => int16,
        replica_election_results => {array, replica_election_result_2}
    }).

-spec decode_elect_leaders_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: elect_leaders_response_2(),
    Rest :: binary().

decode_elect_leaders_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int32(ThrottleTimeMs, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_array(ReplicaElectionResults, Bin2, Bin3, ?_decode_element(decode_replica_election_result_2)),
    ?decode_tagged_fields(
        fun decode_elect_leaders_response_2_tagged_field/3,
        Header#{
            throttle_time_ms => ThrottleTimeMs,
            error_code => ErrorCode,
            replica_election_results => ReplicaElectionResults
        },
        Bin3
    ).

-spec decode_elect_leaders_response_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_elect_leaders_response_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_partition_result_2(partition_result_2()) -> iodata().

encode_partition_result_2(
    _Args = #{
        % The partition id
        partition_id := PartitionId,
        % The result error, or zero if there was no error.
        error_code := ErrorCode,
        % The result message, or null if there was no error.
        error_message := ErrorMessage
    }
) when
    ?is_int32(PartitionId),
    ?is_int16(ErrorCode),
    ?is_nullable_string(ErrorMessage)
->
    [
        ?encode_int32(PartitionId),
        ?encode_int16(ErrorCode),
        ?encode_compact_nullable_string(ErrorMessage),
        ?EMPTY_TAG_BUFFER
    ];
encode_partition_result_2(Args) ->
    ?encoder_error(Args, #{
        partition_id => int32,
        error_code => int16,
        error_message => nullable_string
    }).

-spec decode_partition_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: partition_result_2(),
    Rest :: binary().

decode_partition_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(PartitionId, Bin0, Bin1),
    ?_decode_int16(ErrorCode, Bin1, Bin2),
    ?_decode_compact_nullable_string(ErrorMessage, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_partition_result_2_tagged_field/3,
        #{
            partition_id => PartitionId,
            error_code => ErrorCode,
            error_message => ErrorMessage
        },
        Bin3
    ).

-spec decode_partition_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_partition_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_replica_election_result_2(replica_election_result_2()) -> iodata().

encode_replica_election_result_2(
    _Args = #{
        % The topic name
        topic := Topic,
        % The results for each partition
        partition_result := PartitionResult
    }
) when
    ?is_string(Topic),
    ?is_array(PartitionResult)
->
    [
        ?encode_compact_string(Topic),
        ?encode_compact_array(PartitionResult, fun encode_partition_result_2/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_replica_election_result_2(Args) ->
    ?encoder_error(Args, #{
        topic => string,
        partition_result => {array, partition_result_2}
    }).

-spec decode_replica_election_result_2(binary()) -> {Decoded, Rest} when
    Decoded :: replica_election_result_2(),
    Rest :: binary().

decode_replica_election_result_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Topic, Bin0, Bin1),
    ?_decode_compact_array(PartitionResult, Bin1, Bin2, ?_decode_element(decode_partition_result_2)),
    ?decode_tagged_fields(
        fun decode_replica_election_result_2_tagged_field/3,
        #{
            topic => Topic,
            partition_result => PartitionResult
        },
        Bin2
    ).

-spec decode_replica_election_result_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_replica_election_result_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type elect_leaders_response_0() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    replica_election_results := list(replica_election_result_0())
}.
-type partition_result_0() :: #{
    partition_id := integer(),
    error_code := integer(),
    error_message := binary() | null
}.
-type replica_election_result_0() :: #{
    topic := binary(),
    partition_result := list(partition_result_0())
}.
-type elect_leaders_response_1() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    replica_election_results := list(replica_election_result_1())
}.
-type partition_result_1() :: #{
    partition_id := integer(),
    error_code := integer(),
    error_message := binary() | null
}.
-type replica_election_result_1() :: #{
    topic := binary(),
    partition_result := list(partition_result_1())
}.
-type elect_leaders_response_2() :: #{
    correlation_id => integer(),
    throttle_time_ms := integer(),
    error_code := integer(),
    replica_election_results := list(replica_election_result_2())
}.
-type partition_result_2() :: #{
    partition_id := integer(),
    error_code := integer(),
    error_message := binary() | null
}.
-type replica_election_result_2() :: #{
    topic := binary(),
    partition_result := list(partition_result_2())
}.
