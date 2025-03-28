-module(k_raft_version_record).
-export([
    encode_k_raft_version_record_0/1,
    decode_k_raft_version_record_0/1
]).
-export_type([
    k_raft_version_record_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_k_raft_version_record_0(k_raft_version_record_0()) -> iodata().

encode_k_raft_version_record_0(
    _Args = #{
        % The version of the kraft version record
        version := Version,
        % The kraft protocol version
        k_raft_version := KRaftVersion
    }
) when
    ?is_int16(Version),
    ?is_int16(KRaftVersion)
->
    [
        ?encode_int16(Version),
        ?encode_int16(KRaftVersion),
        ?EMPTY_TAG_BUFFER
    ];
encode_k_raft_version_record_0(Args) ->
    ?encoder_error(Args, #{
        version => int16,
        k_raft_version => int16
    }).

-spec decode_k_raft_version_record_0(binary()) -> {Decoded, Rest} when
    Decoded :: k_raft_version_record_0(),
    Rest :: binary().

decode_k_raft_version_record_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(Version, Bin0, Bin1),
    ?_decode_int16(KRaftVersion, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_k_raft_version_record_0_tagged_field/3,
        #{
            version => Version,
            k_raft_version => KRaftVersion
        },
        Bin2
    ).

-spec decode_k_raft_version_record_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_k_raft_version_record_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type k_raft_version_record_0() :: #{
    version := integer(),
    k_raft_version := integer()
}.
