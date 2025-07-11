-module(leader_change_message).
-export([
    encode_leader_change_message_0/1,
    decode_leader_change_message_0/1
]).
-export_type([
    leader_change_message_0/0,
    voter_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_leader_change_message_0(leader_change_message_0()) -> iodata().

encode_leader_change_message_0(
    _Args = #{
        % The version of the leader change message
        version := Version,
        % The ID of the newly elected leader
        leader_id := LeaderId,
        % The set of voters in the quorum for this epoch
        voters := Voters,
        % The voters who voted for the leader at the time of election
        granting_voters := GrantingVoters
    }
) when
    ?is_int16(Version),
    ?is_int32(LeaderId),
    ?is_array(Voters),
    ?is_array(GrantingVoters)
->
    [
        ?encode_int16(Version),
        ?encode_int32(LeaderId),
        ?encode_compact_array(Voters, fun encode_voter_0/1),
        ?encode_compact_array(GrantingVoters, fun encode_voter_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_leader_change_message_0(Args) ->
    ?encoder_error(Args, #{
        version => int16,
        leader_id => int32,
        voters => {array, voter_0},
        granting_voters => {array, voter_0}
    }).

-spec decode_leader_change_message_0(binary()) -> {Decoded, Rest} when
    Decoded :: leader_change_message_0(),
    Rest :: binary().

decode_leader_change_message_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(Version, Bin0, Bin1),
    ?_decode_int32(LeaderId, Bin1, Bin2),
    ?_decode_compact_array(Voters, Bin2, Bin3, ?_decode_element(decode_voter_0)),
    ?_decode_compact_array(GrantingVoters, Bin3, Bin4, ?_decode_element(decode_voter_0)),
    ?decode_tagged_fields(
        fun decode_leader_change_message_0_tagged_field/3,
        #{
            version => Version,
            leader_id => LeaderId,
            voters => Voters,
            granting_voters => GrantingVoters
        },
        Bin4
    ).

-spec decode_leader_change_message_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: leader_change_message_0().

decode_leader_change_message_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_voter_0(voter_0()) -> iodata().

encode_voter_0(
    _Args = #{
        voter_id := VoterId
    }
) when
    ?is_int32(VoterId)
->
    [
        ?encode_int32(VoterId),
        ?EMPTY_TAG_BUFFER
    ];
encode_voter_0(Args) ->
    ?encoder_error(Args, #{
        voter_id => int32
    }).

-spec decode_voter_0(binary()) -> {Decoded, Rest} when
    Decoded :: voter_0(),
    Rest :: binary().

decode_voter_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int32(VoterId, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_voter_0_tagged_field/3,
        #{
            voter_id => VoterId
        },
        Bin1
    ).

-spec decode_voter_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: voter_0().

decode_voter_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type leader_change_message_0() :: #{
    version := integer(),
    leader_id := integer(),
    voters := list(voter_0()),
    granting_voters := list(voter_0())
}.
-type voter_0() :: #{
    voter_id := integer()
}.
