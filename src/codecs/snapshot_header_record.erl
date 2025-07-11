-module(snapshot_header_record).
-export([
    encode_snapshot_header_record_0/1,
    decode_snapshot_header_record_0/1
]).
-export_type([
    snapshot_header_record_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_snapshot_header_record_0(snapshot_header_record_0()) -> iodata().

encode_snapshot_header_record_0(
    _Args = #{
        % The version of the snapshot header record
        version := Version,
        % The append time of the last record from the log contained in this snapshot
        last_contained_log_timestamp := LastContainedLogTimestamp
    }
) when
    ?is_int16(Version),
    ?is_int64(LastContainedLogTimestamp)
->
    [
        ?encode_int16(Version),
        ?encode_int64(LastContainedLogTimestamp),
        ?EMPTY_TAG_BUFFER
    ];
encode_snapshot_header_record_0(Args) ->
    ?encoder_error(Args, #{
        version => int16,
        last_contained_log_timestamp => int64
    }).

-spec decode_snapshot_header_record_0(binary()) -> {Decoded, Rest} when
    Decoded :: snapshot_header_record_0(),
    Rest :: binary().

decode_snapshot_header_record_0(Bin0) when is_binary(Bin0) ->
    ?_decode_int16(Version, Bin0, Bin1),
    ?_decode_int64(LastContainedLogTimestamp, Bin1, Bin2),
    ?decode_tagged_fields(
        fun decode_snapshot_header_record_0_tagged_field/3,
        #{
            version => Version,
            last_contained_log_timestamp => LastContainedLogTimestamp
        },
        Bin2
    ).

-spec decode_snapshot_header_record_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: snapshot_header_record_0().

decode_snapshot_header_record_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type snapshot_header_record_0() :: #{
    version := integer(),
    last_contained_log_timestamp := integer()
}.
