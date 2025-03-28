-module(voters_record).
-export([
    encode_voters_record_0/1,
    decode_voters_record_0/1
]).
-export_type([
    voters_record_0/0,
    endpoint_0/0,
    k_raft_version_feature_0/0,
    voter_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_voters_record_0(voters_record_0()) -> iodata().

encode_voters_record_0(
    _Args = #{
        % The version of the voters record
        version := Version,
        voters := Voters
    }
) when
    ?is_int16(Version),
    ?is_array(Voters)
->
    [
        ?encode_int16(Version),
        ?encode_compact_array(Voters, fun encode_voter_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_voters_record_0(Args) ->
    ?encoder_error(Args, #{
        version => int16,
        voters => {array, voter_0}
    }).

-spec decode_voters_record_0(binary()) -> {Decoded, Rest} when
    Decoded :: voters_record_0(),
    Rest :: binary().

decode_voters_record_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(Version, Bin0, Bin1),
    ?_decode_compact_array(Voters, Bin1, Bin2, ?_decode_element(decode_voter_0)),
    ?decode_tagged_fields(
        fun decode_voters_record_0_tagged_field/3,
        #{
            version => Version,
            voters => Voters
        },
        Bin2
    ).

-spec decode_voters_record_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_voters_record_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_endpoint_0(endpoint_0()) -> iodata().

encode_endpoint_0(
    _Args = #{
        % The name of the endpoint
        name := Name,
        % The hostname
        host := Host,
        % The port
        port := Port
    }
) when
    ?is_string(Name),
    ?is_string(Host),
    ?is_uint16(Port)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_string(Host),
        ?encode_uint16(Port),
        ?EMPTY_TAG_BUFFER
    ];
encode_endpoint_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        host => string,
        port => uint16
    }).

-spec decode_endpoint_0(binary()) -> {Decoded, Rest} when
    Decoded :: endpoint_0(),
    Rest :: binary().

decode_endpoint_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_uint16(Port, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_endpoint_0_tagged_field/3,
        #{
            name => Name,
            host => Host,
            port => Port
        },
        Bin3
    ).

-spec decode_endpoint_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_endpoint_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_k_raft_version_feature_0(k_raft_version_feature_0()) -> iodata().

encode_k_raft_version_feature_0(
    _Args = #{
        % The minimum supported KRaft protocol version
        min_supported_version := MinSupportedVersion,
        % The maximum supported KRaft protocol version
        max_supported_version := MaxSupportedVersion
    }
) when
    ?is_int16(MinSupportedVersion),
    ?is_int16(MaxSupportedVersion)
->
    [
        ?encode_int16(MinSupportedVersion),
        ?encode_int16(MaxSupportedVersion),
        ?EMPTY_TAG_BUFFER
    ];
encode_k_raft_version_feature_0(Args) ->
    ?encoder_error(Args, #{
        min_supported_version => int16,
        max_supported_version => int16
    }).

-spec decode_k_raft_version_feature_0(binary()) -> {Decoded, Rest} when
    Decoded :: k_raft_version_feature_0(),
    Rest :: binary().

decode_k_raft_version_feature_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(MinSupportedVersion, Bin0, Bin1),
    ?_decode_int16(MaxSupportedVersion, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_k_raft_version_feature_0_tagged_field/3,
        #{
            min_supported_version => MinSupportedVersion,
            max_supported_version => MaxSupportedVersion
        },
        Bin2
    ).

-spec decode_k_raft_version_feature_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_k_raft_version_feature_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_voter_0(voter_0()) -> iodata().

encode_voter_0(
    _Args = #{
        % The replica id of the voter in the topic partition
        voter_id := VoterId,
        % The directory id of the voter in the topic partition
        voter_directory_id := VoterDirectoryId,
        % The endpoint that can be used to communicate with the voter
        endpoints := Endpoints,
        % The range of versions of the protocol that the replica supports
        k_raft_version_feature := KRaftVersionFeature
    }
) when
    ?is_int32(VoterId),
    ?is_uuid(VoterDirectoryId),
    ?is_array(Endpoints),
    ?is_entity(KRaftVersionFeature)
->
    [
        ?encode_int32(VoterId),
        ?encode_uuid(VoterDirectoryId),
        ?encode_compact_array(Endpoints, fun encode_endpoint_0/1),
        encode_k_raft_version_feature_0(KRaftVersionFeature),
        ?EMPTY_TAG_BUFFER
    ];
encode_voter_0(Args) ->
    ?encoder_error(Args, #{
        voter_id => int32,
        voter_directory_id => uuid,
        endpoints => {array, endpoint_0},
        k_raft_version_feature => map
    }).

-spec decode_voter_0(binary()) -> {Decoded, Rest} when
    Decoded :: voter_0(),
    Rest :: binary().

decode_voter_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(VoterId, Bin0, Bin1),
    ?_decode_uuid(VoterDirectoryId, Bin1, Bin2),
    ?_decode_compact_array(Endpoints, Bin2, Bin3, ?_decode_element(decode_endpoint_0)),
    ?_decode_entity(KRaftVersionFeature, Bin3, Bin4, decode_k_raft_version_feature_0),
    ?decode_tagged_fields(
        fun decode_voter_0_tagged_field/3,
        #{
            voter_id => VoterId,
            voter_directory_id => VoterDirectoryId,
            endpoints => Endpoints,
            k_raft_version_feature => KRaftVersionFeature
        },
        Bin4
    ).

-spec decode_voter_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_voter_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type voters_record_0() :: #{
    version := integer(),
    voters := list(voter_0())
}.
-type endpoint_0() :: #{
    name := binary(),
    host := binary(),
    port := non_neg_integer()
}.
-type k_raft_version_feature_0() :: #{
    min_supported_version := integer(),
    max_supported_version := integer()
}.
-type voter_0() :: #{
    voter_id := integer(),
    voter_directory_id := kafcod:uuid(),
    endpoints := list(endpoint_0()),
    k_raft_version_feature := k_raft_version_feature_0()
}.
