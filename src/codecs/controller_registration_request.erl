-module(controller_registration_request).
-export([
    encode_controller_registration_request_0/1,
    decode_controller_registration_request_0/1
]).
-export_type([
    controller_registration_request_0/0,
    listener_0/0,
    feature_0/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(CONTROLLER_REGISTRATION_REQUEST, 70).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_controller_registration_request_0(controller_registration_request_0()) -> iodata().

encode_controller_registration_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The ID of the controller to register.
        controller_id := ControllerId,
        % The controller incarnation ID, which is unique to each process run.
        incarnation_id := IncarnationId,
        % Set if the required configurations for ZK migration are present.
        zk_migration_ready := ZkMigrationReady,
        % The listeners of this controller
        listeners := Listeners,
        % The features on this controller
        features := Features
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(ControllerId),
    ?is_uuid(IncarnationId),
    ?is_bool(ZkMigrationReady),
    ?is_array(Listeners),
    ?is_array(Features)
->
    [
        ?encode_request_header_2(?CONTROLLER_REGISTRATION_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(ControllerId),
        ?encode_uuid(IncarnationId),
        ?encode_bool(ZkMigrationReady),
        ?encode_compact_array(Listeners, fun encode_listener_0/1),
        ?encode_compact_array(Features, fun encode_feature_0/1),
        ?EMPTY_TAG_BUFFER
    ];
encode_controller_registration_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        controller_id => int32,
        incarnation_id => uuid,
        zk_migration_ready => bool,
        listeners => {array, listener_0},
        features => {array, feature_0}
    }).

-spec decode_controller_registration_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: controller_registration_request_0(),
    Rest :: binary().

decode_controller_registration_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(ControllerId, Bin0, Bin1),
    ?_decode_uuid(IncarnationId, Bin1, Bin2),
    ?_decode_bool(ZkMigrationReady, Bin2, Bin3),
    ?_decode_compact_array(Listeners, Bin3, Bin4, ?_decode_element(decode_listener_0)),
    ?_decode_compact_array(Features, Bin4, Bin5, ?_decode_element(decode_feature_0)),
    ?decode_tagged_fields(
        fun decode_controller_registration_request_0_tagged_field/3,
        Header#{
            controller_id => ControllerId,
            incarnation_id => IncarnationId,
            zk_migration_ready => ZkMigrationReady,
            listeners => Listeners,
            features => Features
        },
        Bin5
    ).

-spec decode_controller_registration_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_controller_registration_request_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_listener_0(listener_0()) -> iodata().

encode_listener_0(
    _Args = #{
        % The name of the endpoint.
        name := Name,
        % The hostname.
        host := Host,
        % The port.
        port := Port,
        % The security protocol.
        security_protocol := SecurityProtocol
    }
) when
    ?is_string(Name),
    ?is_string(Host),
    ?is_uint16(Port),
    ?is_int16(SecurityProtocol)
->
    [
        ?encode_compact_string(Name),
        ?encode_compact_string(Host),
        ?encode_uint16(Port),
        ?encode_int16(SecurityProtocol),
        ?EMPTY_TAG_BUFFER
    ];
encode_listener_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        host => string,
        port => uint16,
        security_protocol => int16
    }).

-spec decode_listener_0(binary()) -> {Decoded, Rest} when
    Decoded :: listener_0(),
    Rest :: binary().

decode_listener_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_uint16(Port, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_listener_0_tagged_field/3,
        #{
            name => Name,
            host => Host,
            port => Port,
            security_protocol => SecurityProtocol
        },
        Bin4
    ).

-spec decode_listener_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_listener_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_feature_0(feature_0()) -> iodata().

encode_feature_0(
    _Args = #{
        % The feature name.
        name := Name,
        % The minimum supported feature level.
        min_supported_version := MinSupportedVersion,
        % The maximum supported feature level.
        max_supported_version := MaxSupportedVersion
    }
) when
    ?is_string(Name),
    ?is_int16(MinSupportedVersion),
    ?is_int16(MaxSupportedVersion)
->
    [
        ?encode_compact_string(Name),
        ?encode_int16(MinSupportedVersion),
        ?encode_int16(MaxSupportedVersion),
        ?EMPTY_TAG_BUFFER
    ];
encode_feature_0(Args) ->
    ?encoder_error(Args, #{
        name => string,
        min_supported_version => int16,
        max_supported_version => int16
    }).

-spec decode_feature_0(binary()) -> {Decoded, Rest} when
    Decoded :: feature_0(),
    Rest :: binary().

decode_feature_0(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(MinSupportedVersion, Bin1, Bin2),
    ?_decode_int16(MaxSupportedVersion, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_feature_0_tagged_field/3,
        #{
            name => Name,
            min_supported_version => MinSupportedVersion,
            max_supported_version => MaxSupportedVersion
        },
        Bin3
    ).

-spec decode_feature_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_feature_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type controller_registration_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    controller_id := integer(),
    incarnation_id := kafcod:uuid(),
    zk_migration_ready := boolean(),
    listeners := list(listener_0()),
    features := list(feature_0())
}.
-type listener_0() :: #{
    name := binary(),
    host := binary(),
    port := non_neg_integer(),
    security_protocol := integer()
}.
-type feature_0() :: #{
    name := binary(),
    min_supported_version := integer(),
    max_supported_version := integer()
}.
