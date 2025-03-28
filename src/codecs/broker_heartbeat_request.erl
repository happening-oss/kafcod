-module(broker_heartbeat_request).
-export([
    encode_broker_heartbeat_request_0/1,
    decode_broker_heartbeat_request_0/1,
    encode_broker_heartbeat_request_1/1,
    decode_broker_heartbeat_request_1/1
]).
-export_type([
    broker_heartbeat_request_0/0,
    broker_heartbeat_request_1/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(BROKER_HEARTBEAT_REQUEST, 63).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_broker_heartbeat_request_0(broker_heartbeat_request_0()) -> iodata().

encode_broker_heartbeat_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID.
        broker_id := BrokerId,
        % The broker epoch.
        broker_epoch := BrokerEpoch,
        % The highest metadata offset which the broker has reached.
        current_metadata_offset := CurrentMetadataOffset,
        % True if the broker wants to be fenced, false otherwise.
        want_fence := WantFence,
        % True if the broker wants to be shut down, false otherwise.
        want_shut_down := WantShutDown
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch),
    ?is_int64(CurrentMetadataOffset),
    ?is_bool(WantFence),
    ?is_bool(WantShutDown)
->
    [
        ?encode_request_header_2(?BROKER_HEARTBEAT_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?encode_int64(CurrentMetadataOffset),
        ?encode_bool(WantFence),
        ?encode_bool(WantShutDown),
        ?EMPTY_TAG_BUFFER
    ];
encode_broker_heartbeat_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64,
        current_metadata_offset => int64,
        want_fence => bool,
        want_shut_down => bool
    }).

-spec decode_broker_heartbeat_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: broker_heartbeat_request_0(),
    Rest :: binary().

decode_broker_heartbeat_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?_decode_int64(CurrentMetadataOffset, Bin2, Bin3),
    ?_decode_bool(WantFence, Bin3, Bin4),
    ?_decode_bool(WantShutDown, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_broker_heartbeat_request_0_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch,
            current_metadata_offset => CurrentMetadataOffset,
            want_fence => WantFence,
            want_shut_down => WantShutDown
        },
        Bin5
    ).

-spec decode_broker_heartbeat_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_broker_heartbeat_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_broker_heartbeat_request_1(broker_heartbeat_request_1()) -> iodata().

encode_broker_heartbeat_request_1(
    Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID.
        broker_id := BrokerId,
        % The broker epoch.
        broker_epoch := BrokerEpoch,
        % The highest metadata offset which the broker has reached.
        current_metadata_offset := CurrentMetadataOffset,
        % True if the broker wants to be fenced, false otherwise.
        want_fence := WantFence,
        % True if the broker wants to be shut down, false otherwise.
        want_shut_down := WantShutDown
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_int64(BrokerEpoch),
    ?is_int64(CurrentMetadataOffset),
    ?is_bool(WantFence),
    ?is_bool(WantShutDown)
->
    [
        ?encode_request_header_2(?BROKER_HEARTBEAT_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_int64(BrokerEpoch),
        ?encode_int64(CurrentMetadataOffset),
        ?encode_bool(WantFence),
        ?encode_bool(WantShutDown),
        ?encode_tagged_fields(
            fun encode_broker_heartbeat_request_1_tagged_field/2,
            Args
        )
    ];
encode_broker_heartbeat_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        broker_epoch => int64,
        current_metadata_offset => int64,
        want_fence => bool,
        want_shut_down => bool
    }).

-spec encode_broker_heartbeat_request_1_tagged_field(Key :: atom(), Value :: term()) -> iodata() | ignore.

encode_broker_heartbeat_request_1_tagged_field(_Key = offline_log_dirs, OfflineLogDirs) ->
    {0, ?encode_compact_array(OfflineLogDirs, ?encode_uuid_)};
encode_broker_heartbeat_request_1_tagged_field(_Key, _Value) ->
    ignore.

-spec decode_broker_heartbeat_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: broker_heartbeat_request_1(),
    Rest :: binary().

decode_broker_heartbeat_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_int64(BrokerEpoch, Bin1, Bin2),
    ?_decode_int64(CurrentMetadataOffset, Bin2, Bin3),
    ?_decode_bool(WantFence, Bin3, Bin4),
    ?_decode_bool(WantShutDown, Bin4, Bin5),
    ?decode_tagged_fields(
        fun decode_broker_heartbeat_request_1_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            broker_epoch => BrokerEpoch,
            current_metadata_offset => CurrentMetadataOffset,
            want_fence => WantFence,
            want_shut_down => WantShutDown
        },
        Bin5
    ).

-spec decode_broker_heartbeat_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

%% OfflineLogDirs
%% Log directories that failed and went offline.
decode_broker_heartbeat_request_1_tagged_field(_Tag = 0, Bin0, Acc) ->
    ?_decode_compact_array(OfflineLogDirs, Bin0, Bin1, ?decode_uuid_),
    <<>> = Bin1,
    Acc#{offline_log_dirs => OfflineLogDirs};
decode_broker_heartbeat_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type broker_heartbeat_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer(),
    current_metadata_offset := integer(),
    want_fence := boolean(),
    want_shut_down := boolean()
}.
-type broker_heartbeat_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    broker_epoch := integer(),
    current_metadata_offset := integer(),
    want_fence := boolean(),
    want_shut_down := boolean(),
    offline_log_dirs := list(kafcod:uuid())
}.
