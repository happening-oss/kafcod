-module(broker_registration_request).
-export([
    encode_broker_registration_request_0/1,
    decode_broker_registration_request_0/1,
    encode_broker_registration_request_1/1,
    decode_broker_registration_request_1/1,
    encode_broker_registration_request_2/1,
    decode_broker_registration_request_2/1,
    encode_broker_registration_request_3/1,
    decode_broker_registration_request_3/1
]).
-export_type([
    broker_registration_request_0/0,
    listener_0/0,
    feature_0/0,
    broker_registration_request_1/0,
    listener_1/0,
    feature_1/0,
    broker_registration_request_2/0,
    listener_2/0,
    feature_2/0,
    broker_registration_request_3/0,
    listener_3/0,
    feature_3/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(BROKER_REGISTRATION_REQUEST, 62).
-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_broker_registration_request_0(broker_registration_request_0()) -> iodata().

encode_broker_registration_request_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID.
        broker_id := BrokerId,
        % The cluster id of the broker process.
        cluster_id := ClusterId,
        % The incarnation id of the broker process.
        incarnation_id := IncarnationId,
        % The listeners of this broker
        listeners := Listeners,
        % The features on this broker
        features := Features,
        % The rack which this broker is in.
        rack := Rack
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_string(ClusterId),
    ?is_uuid(IncarnationId),
    ?is_array(Listeners),
    ?is_array(Features),
    ?is_nullable_string(Rack)
->
    [
        ?encode_request_header_2(?BROKER_REGISTRATION_REQUEST, 0, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_compact_string(ClusterId),
        ?encode_uuid(IncarnationId),
        ?encode_compact_array(Listeners, fun encode_listener_0/1),
        ?encode_compact_array(Features, fun encode_feature_0/1),
        ?encode_compact_nullable_string(Rack),
        ?EMPTY_TAG_BUFFER
    ];
encode_broker_registration_request_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        cluster_id => string,
        incarnation_id => uuid,
        listeners => {array, listener_0},
        features => {array, feature_0},
        rack => nullable_string
    }).

-spec decode_broker_registration_request_0(binary()) -> {Decoded, Rest} when
    Decoded :: broker_registration_request_0(),
    Rest :: binary().

decode_broker_registration_request_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(ClusterId, Bin1, Bin2),
    ?_decode_uuid(IncarnationId, Bin2, Bin3),
    ?_decode_compact_array(Listeners, Bin3, Bin4, ?_decode_element(decode_listener_0)),
    ?_decode_compact_array(Features, Bin4, Bin5, ?_decode_element(decode_feature_0)),
    ?_decode_compact_nullable_string(Rack, Bin5, Bin6),
    ?decode_tagged_fields(
        fun decode_broker_registration_request_0_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            cluster_id => ClusterId,
            incarnation_id => IncarnationId,
            listeners => Listeners,
            features => Features,
            rack => Rack
        },
        Bin6
    ).

-spec decode_broker_registration_request_0_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: broker_registration_request_0().

decode_broker_registration_request_0_tagged_field(_Tag, _Bin0, Acc) ->
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
    AccOut :: Acc,
    Acc :: listener_0().

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
    AccOut :: Acc,
    Acc :: feature_0().

decode_feature_0_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_broker_registration_request_1(broker_registration_request_1()) -> iodata().

encode_broker_registration_request_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID.
        broker_id := BrokerId,
        % The cluster id of the broker process.
        cluster_id := ClusterId,
        % The incarnation id of the broker process.
        incarnation_id := IncarnationId,
        % The listeners of this broker
        listeners := Listeners,
        % The features on this broker
        features := Features,
        % The rack which this broker is in.
        rack := Rack,
        % If the required configurations for ZK migration are present, this value is set to true
        is_migrating_zk_broker := IsMigratingZkBroker
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_string(ClusterId),
    ?is_uuid(IncarnationId),
    ?is_array(Listeners),
    ?is_array(Features),
    ?is_nullable_string(Rack),
    ?is_bool(IsMigratingZkBroker)
->
    [
        ?encode_request_header_2(?BROKER_REGISTRATION_REQUEST, 1, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_compact_string(ClusterId),
        ?encode_uuid(IncarnationId),
        ?encode_compact_array(Listeners, fun encode_listener_1/1),
        ?encode_compact_array(Features, fun encode_feature_1/1),
        ?encode_compact_nullable_string(Rack),
        ?encode_bool(IsMigratingZkBroker),
        ?EMPTY_TAG_BUFFER
    ];
encode_broker_registration_request_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        cluster_id => string,
        incarnation_id => uuid,
        listeners => {array, listener_1},
        features => {array, feature_1},
        rack => nullable_string,
        is_migrating_zk_broker => bool
    }).

-spec decode_broker_registration_request_1(binary()) -> {Decoded, Rest} when
    Decoded :: broker_registration_request_1(),
    Rest :: binary().

decode_broker_registration_request_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(ClusterId, Bin1, Bin2),
    ?_decode_uuid(IncarnationId, Bin2, Bin3),
    ?_decode_compact_array(Listeners, Bin3, Bin4, ?_decode_element(decode_listener_1)),
    ?_decode_compact_array(Features, Bin4, Bin5, ?_decode_element(decode_feature_1)),
    ?_decode_compact_nullable_string(Rack, Bin5, Bin6),
    ?_decode_bool(IsMigratingZkBroker, Bin6, Bin7),
    ?decode_tagged_fields(
        fun decode_broker_registration_request_1_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            cluster_id => ClusterId,
            incarnation_id => IncarnationId,
            listeners => Listeners,
            features => Features,
            rack => Rack,
            is_migrating_zk_broker => IsMigratingZkBroker
        },
        Bin7
    ).

-spec decode_broker_registration_request_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: broker_registration_request_1().

decode_broker_registration_request_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_listener_1(listener_1()) -> iodata().

encode_listener_1(
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
encode_listener_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        host => string,
        port => uint16,
        security_protocol => int16
    }).

-spec decode_listener_1(binary()) -> {Decoded, Rest} when
    Decoded :: listener_1(),
    Rest :: binary().

decode_listener_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_uint16(Port, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_listener_1_tagged_field/3,
        #{
            name => Name,
            host => Host,
            port => Port,
            security_protocol => SecurityProtocol
        },
        Bin4
    ).

-spec decode_listener_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: listener_1().

decode_listener_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_feature_1(feature_1()) -> iodata().

encode_feature_1(
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
encode_feature_1(Args) ->
    ?encoder_error(Args, #{
        name => string,
        min_supported_version => int16,
        max_supported_version => int16
    }).

-spec decode_feature_1(binary()) -> {Decoded, Rest} when
    Decoded :: feature_1(),
    Rest :: binary().

decode_feature_1(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(MinSupportedVersion, Bin1, Bin2),
    ?_decode_int16(MaxSupportedVersion, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_feature_1_tagged_field/3,
        #{
            name => Name,
            min_supported_version => MinSupportedVersion,
            max_supported_version => MaxSupportedVersion
        },
        Bin3
    ).

-spec decode_feature_1_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: feature_1().

decode_feature_1_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_broker_registration_request_2(broker_registration_request_2()) -> iodata().

encode_broker_registration_request_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID.
        broker_id := BrokerId,
        % The cluster id of the broker process.
        cluster_id := ClusterId,
        % The incarnation id of the broker process.
        incarnation_id := IncarnationId,
        % The listeners of this broker
        listeners := Listeners,
        % The features on this broker
        features := Features,
        % The rack which this broker is in.
        rack := Rack,
        % If the required configurations for ZK migration are present, this value is set to true
        is_migrating_zk_broker := IsMigratingZkBroker,
        % Log directories configured in this broker which are available.
        log_dirs := LogDirs
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_string(ClusterId),
    ?is_uuid(IncarnationId),
    ?is_array(Listeners),
    ?is_array(Features),
    ?is_nullable_string(Rack),
    ?is_bool(IsMigratingZkBroker),
    ?is_array(LogDirs)
->
    [
        ?encode_request_header_2(?BROKER_REGISTRATION_REQUEST, 2, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_compact_string(ClusterId),
        ?encode_uuid(IncarnationId),
        ?encode_compact_array(Listeners, fun encode_listener_2/1),
        ?encode_compact_array(Features, fun encode_feature_2/1),
        ?encode_compact_nullable_string(Rack),
        ?encode_bool(IsMigratingZkBroker),
        ?encode_compact_array(LogDirs, ?encode_uuid_),
        ?EMPTY_TAG_BUFFER
    ];
encode_broker_registration_request_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        cluster_id => string,
        incarnation_id => uuid,
        listeners => {array, listener_2},
        features => {array, feature_2},
        rack => nullable_string,
        is_migrating_zk_broker => bool,
        log_dirs => {array, uuid}
    }).

-spec decode_broker_registration_request_2(binary()) -> {Decoded, Rest} when
    Decoded :: broker_registration_request_2(),
    Rest :: binary().

decode_broker_registration_request_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(ClusterId, Bin1, Bin2),
    ?_decode_uuid(IncarnationId, Bin2, Bin3),
    ?_decode_compact_array(Listeners, Bin3, Bin4, ?_decode_element(decode_listener_2)),
    ?_decode_compact_array(Features, Bin4, Bin5, ?_decode_element(decode_feature_2)),
    ?_decode_compact_nullable_string(Rack, Bin5, Bin6),
    ?_decode_bool(IsMigratingZkBroker, Bin6, Bin7),
    ?_decode_compact_array(LogDirs, Bin7, Bin8, ?decode_uuid_),
    ?decode_tagged_fields(
        fun decode_broker_registration_request_2_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            cluster_id => ClusterId,
            incarnation_id => IncarnationId,
            listeners => Listeners,
            features => Features,
            rack => Rack,
            is_migrating_zk_broker => IsMigratingZkBroker,
            log_dirs => LogDirs
        },
        Bin8
    ).

-spec decode_broker_registration_request_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: broker_registration_request_2().

decode_broker_registration_request_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_listener_2(listener_2()) -> iodata().

encode_listener_2(
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
encode_listener_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        host => string,
        port => uint16,
        security_protocol => int16
    }).

-spec decode_listener_2(binary()) -> {Decoded, Rest} when
    Decoded :: listener_2(),
    Rest :: binary().

decode_listener_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_uint16(Port, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_listener_2_tagged_field/3,
        #{
            name => Name,
            host => Host,
            port => Port,
            security_protocol => SecurityProtocol
        },
        Bin4
    ).

-spec decode_listener_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: listener_2().

decode_listener_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_feature_2(feature_2()) -> iodata().

encode_feature_2(
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
encode_feature_2(Args) ->
    ?encoder_error(Args, #{
        name => string,
        min_supported_version => int16,
        max_supported_version => int16
    }).

-spec decode_feature_2(binary()) -> {Decoded, Rest} when
    Decoded :: feature_2(),
    Rest :: binary().

decode_feature_2(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(MinSupportedVersion, Bin1, Bin2),
    ?_decode_int16(MaxSupportedVersion, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_feature_2_tagged_field/3,
        #{
            name => Name,
            min_supported_version => MinSupportedVersion,
            max_supported_version => MaxSupportedVersion
        },
        Bin3
    ).

-spec decode_feature_2_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: feature_2().

decode_feature_2_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_broker_registration_request_3(broker_registration_request_3()) -> iodata().

encode_broker_registration_request_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The client ID string.
        client_id := ClientId,
        % The broker ID.
        broker_id := BrokerId,
        % The cluster id of the broker process.
        cluster_id := ClusterId,
        % The incarnation id of the broker process.
        incarnation_id := IncarnationId,
        % The listeners of this broker
        listeners := Listeners,
        % The features on this broker
        features := Features,
        % The rack which this broker is in.
        rack := Rack,
        % If the required configurations for ZK migration are present, this value is set to true
        is_migrating_zk_broker := IsMigratingZkBroker,
        % Log directories configured in this broker which are available.
        log_dirs := LogDirs,
        % The epoch before a clean shutdown.
        previous_broker_epoch := PreviousBrokerEpoch
    }
) when
    ?is_int32(CorrelationId),
    ?is_nullable_string(ClientId),
    ?is_int32(BrokerId),
    ?is_string(ClusterId),
    ?is_uuid(IncarnationId),
    ?is_array(Listeners),
    ?is_array(Features),
    ?is_nullable_string(Rack),
    ?is_bool(IsMigratingZkBroker),
    ?is_array(LogDirs),
    ?is_int64(PreviousBrokerEpoch)
->
    [
        ?encode_request_header_2(?BROKER_REGISTRATION_REQUEST, 3, CorrelationId, ClientId),
        ?encode_int32(BrokerId),
        ?encode_compact_string(ClusterId),
        ?encode_uuid(IncarnationId),
        ?encode_compact_array(Listeners, fun encode_listener_3/1),
        ?encode_compact_array(Features, fun encode_feature_3/1),
        ?encode_compact_nullable_string(Rack),
        ?encode_bool(IsMigratingZkBroker),
        ?encode_compact_array(LogDirs, ?encode_uuid_),
        ?encode_int64(PreviousBrokerEpoch),
        ?EMPTY_TAG_BUFFER
    ];
encode_broker_registration_request_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        client_id => nullable_string,
        broker_id => int32,
        cluster_id => string,
        incarnation_id => uuid,
        listeners => {array, listener_3},
        features => {array, feature_3},
        rack => nullable_string,
        is_migrating_zk_broker => bool,
        log_dirs => {array, uuid},
        previous_broker_epoch => int64
    }).

-spec decode_broker_registration_request_3(binary()) -> {Decoded, Rest} when
    Decoded :: broker_registration_request_3(),
    Rest :: binary().

decode_broker_registration_request_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_request_header_2(Bin),
    ?_decode_int32(BrokerId, Bin0, Bin1),
    ?_decode_compact_string(ClusterId, Bin1, Bin2),
    ?_decode_uuid(IncarnationId, Bin2, Bin3),
    ?_decode_compact_array(Listeners, Bin3, Bin4, ?_decode_element(decode_listener_3)),
    ?_decode_compact_array(Features, Bin4, Bin5, ?_decode_element(decode_feature_3)),
    ?_decode_compact_nullable_string(Rack, Bin5, Bin6),
    ?_decode_bool(IsMigratingZkBroker, Bin6, Bin7),
    ?_decode_compact_array(LogDirs, Bin7, Bin8, ?decode_uuid_),
    ?_decode_int64(PreviousBrokerEpoch, Bin8, Bin9),
    ?decode_tagged_fields(
        fun decode_broker_registration_request_3_tagged_field/3,
        Header#{
            broker_id => BrokerId,
            cluster_id => ClusterId,
            incarnation_id => IncarnationId,
            listeners => Listeners,
            features => Features,
            rack => Rack,
            is_migrating_zk_broker => IsMigratingZkBroker,
            log_dirs => LogDirs,
            previous_broker_epoch => PreviousBrokerEpoch
        },
        Bin9
    ).

-spec decode_broker_registration_request_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: broker_registration_request_3().

decode_broker_registration_request_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_listener_3(listener_3()) -> iodata().

encode_listener_3(
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
encode_listener_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        host => string,
        port => uint16,
        security_protocol => int16
    }).

-spec decode_listener_3(binary()) -> {Decoded, Rest} when
    Decoded :: listener_3(),
    Rest :: binary().

decode_listener_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_compact_string(Host, Bin1, Bin2),
    ?_decode_uint16(Port, Bin2, Bin3),
    ?_decode_int16(SecurityProtocol, Bin3, Bin4),
    ?decode_tagged_fields(
        fun decode_listener_3_tagged_field/3,
        #{
            name => Name,
            host => Host,
            port => Port,
            security_protocol => SecurityProtocol
        },
        Bin4
    ).

-spec decode_listener_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: listener_3().

decode_listener_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_feature_3(feature_3()) -> iodata().

encode_feature_3(
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
encode_feature_3(Args) ->
    ?encoder_error(Args, #{
        name => string,
        min_supported_version => int16,
        max_supported_version => int16
    }).

-spec decode_feature_3(binary()) -> {Decoded, Rest} when
    Decoded :: feature_3(),
    Rest :: binary().

decode_feature_3(Bin0) when is_binary(Bin0) ->
    ?_decode_compact_string(Name, Bin0, Bin1),
    ?_decode_int16(MinSupportedVersion, Bin1, Bin2),
    ?_decode_int16(MaxSupportedVersion, Bin2, Bin3),
    ?decode_tagged_fields(
        fun decode_feature_3_tagged_field/3,
        #{
            name => Name,
            min_supported_version => MinSupportedVersion,
            max_supported_version => MaxSupportedVersion
        },
        Bin3
    ).

-spec decode_feature_3_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc,
    Acc :: feature_3().

decode_feature_3_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type broker_registration_request_0() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    cluster_id := binary(),
    incarnation_id := kafcod:uuid(),
    listeners := list(listener_0()),
    features := list(feature_0()),
    rack := binary() | null
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
-type broker_registration_request_1() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    cluster_id := binary(),
    incarnation_id := kafcod:uuid(),
    listeners := list(listener_1()),
    features := list(feature_1()),
    rack := binary() | null,
    is_migrating_zk_broker := boolean()
}.
-type listener_1() :: #{
    name := binary(),
    host := binary(),
    port := non_neg_integer(),
    security_protocol := integer()
}.
-type feature_1() :: #{
    name := binary(),
    min_supported_version := integer(),
    max_supported_version := integer()
}.
-type broker_registration_request_2() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    cluster_id := binary(),
    incarnation_id := kafcod:uuid(),
    listeners := list(listener_2()),
    features := list(feature_2()),
    rack := binary() | null,
    is_migrating_zk_broker := boolean(),
    log_dirs := list(kafcod:uuid())
}.
-type listener_2() :: #{
    name := binary(),
    host := binary(),
    port := non_neg_integer(),
    security_protocol := integer()
}.
-type feature_2() :: #{
    name := binary(),
    min_supported_version := integer(),
    max_supported_version := integer()
}.
-type broker_registration_request_3() :: #{
    api_key => integer(),
    api_version => integer(),
    correlation_id => integer(),
    client_id => binary() | null,
    broker_id := integer(),
    cluster_id := binary(),
    incarnation_id := kafcod:uuid(),
    listeners := list(listener_3()),
    features := list(feature_3()),
    rack := binary() | null,
    is_migrating_zk_broker := boolean(),
    log_dirs := list(kafcod:uuid()),
    previous_broker_epoch := integer()
}.
-type listener_3() :: #{
    name := binary(),
    host := binary(),
    port := non_neg_integer(),
    security_protocol := integer()
}.
-type feature_3() :: #{
    name := binary(),
    min_supported_version := integer(),
    max_supported_version := integer()
}.
