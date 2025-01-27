-module(snapshot_footer_record).
-export([
    encode_snapshot_footer_record_0/1,
    decode_snapshot_footer_record_0/1
]).
-export_type([
    snapshot_footer_record_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_snapshot_footer_record_0(snapshot_footer_record_0()) -> iodata().

encode_snapshot_footer_record_0(
    _Args = #{
        % The version of the snapshot footer record
        version := Version
    }
) when
    ?is_int16(Version)
->
    [
        ?encode_int16(Version),
        ?EMPTY_TAG_BUFFER
    ];
encode_snapshot_footer_record_0(Args) ->
    ?encoder_error(Args, #{
        version => int16
    }).

-spec decode_snapshot_footer_record_0(binary()) -> {Decoded, Rest} when
    Decoded :: snapshot_footer_record_0(),
    Rest :: binary().

decode_snapshot_footer_record_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(Version, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_snapshot_footer_record_0_tagged_field/3,
        #{
            version => Version
        },
        Bin1
    ).

-spec decode_snapshot_footer_record_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_snapshot_footer_record_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type snapshot_footer_record_0() :: #{
    version := integer()
}.
