-module(update_metadata_response).
-export([
    encode_update_metadata_response_0/1,
    decode_update_metadata_response_0/1,
    encode_update_metadata_response_1/1,
    decode_update_metadata_response_1/1,
    encode_update_metadata_response_2/1,
    decode_update_metadata_response_2/1,
    encode_update_metadata_response_3/1,
    decode_update_metadata_response_3/1,
    encode_update_metadata_response_4/1,
    decode_update_metadata_response_4/1,
    encode_update_metadata_response_5/1,
    decode_update_metadata_response_5/1,
    encode_update_metadata_response_6/1,
    decode_update_metadata_response_6/1,
    encode_update_metadata_response_7/1,
    decode_update_metadata_response_7/1,
    encode_update_metadata_response_8/1,
    decode_update_metadata_response_8/1
]).
-export_type([
    update_metadata_response_0/0,
    update_metadata_response_1/0,
    update_metadata_response_2/0,
    update_metadata_response_3/0,
    update_metadata_response_4/0,
    update_metadata_response_5/0,
    update_metadata_response_6/0,
    update_metadata_response_7/0,
    update_metadata_response_8/0
]).
-include("../encoders.hrl").
-include("../decoders.hrl").
-include("../guards.hrl").
-include("../error.hrl").

-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).

-spec encode_update_metadata_response_0(update_metadata_response_0()) -> iodata().

encode_update_metadata_response_0(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode)
    ];
encode_update_metadata_response_0(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_0(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_0(),
    Rest :: binary().

decode_update_metadata_response_0(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    {
        Header#{
            error_code => ErrorCode
        },
        Bin1
    }.

-spec encode_update_metadata_response_1(update_metadata_response_1()) -> iodata().

encode_update_metadata_response_1(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode)
    ];
encode_update_metadata_response_1(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_1(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_1(),
    Rest :: binary().

decode_update_metadata_response_1(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    {
        Header#{
            error_code => ErrorCode
        },
        Bin1
    }.

-spec encode_update_metadata_response_2(update_metadata_response_2()) -> iodata().

encode_update_metadata_response_2(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode)
    ];
encode_update_metadata_response_2(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_2(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_2(),
    Rest :: binary().

decode_update_metadata_response_2(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    {
        Header#{
            error_code => ErrorCode
        },
        Bin1
    }.

-spec encode_update_metadata_response_3(update_metadata_response_3()) -> iodata().

encode_update_metadata_response_3(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode)
    ];
encode_update_metadata_response_3(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_3(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_3(),
    Rest :: binary().

decode_update_metadata_response_3(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    {
        Header#{
            error_code => ErrorCode
        },
        Bin1
    }.

-spec encode_update_metadata_response_4(update_metadata_response_4()) -> iodata().

encode_update_metadata_response_4(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode)
    ];
encode_update_metadata_response_4(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_4(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_4(),
    Rest :: binary().

decode_update_metadata_response_4(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    {
        Header#{
            error_code => ErrorCode
        },
        Bin1
    }.

-spec encode_update_metadata_response_5(update_metadata_response_5()) -> iodata().

encode_update_metadata_response_5(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_0(CorrelationId),
        ?encode_int16(ErrorCode)
    ];
encode_update_metadata_response_5(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_5(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_5(),
    Rest :: binary().

decode_update_metadata_response_5(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_0(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    {
        Header#{
            error_code => ErrorCode
        },
        Bin1
    }.

-spec encode_update_metadata_response_6(update_metadata_response_6()) -> iodata().

encode_update_metadata_response_6(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_response_6(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_6(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_6(),
    Rest :: binary().

decode_update_metadata_response_6(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_update_metadata_response_6_tagged_field/3,
        Header#{
            error_code => ErrorCode
        },
        Bin1
    ).

-spec decode_update_metadata_response_6_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_update_metadata_response_6_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_response_7(update_metadata_response_7()) -> iodata().

encode_update_metadata_response_7(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_response_7(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_7(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_7(),
    Rest :: binary().

decode_update_metadata_response_7(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_update_metadata_response_7_tagged_field/3,
        Header#{
            error_code => ErrorCode
        },
        Bin1
    ).

-spec decode_update_metadata_response_7_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_update_metadata_response_7_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-spec encode_update_metadata_response_8(update_metadata_response_8()) -> iodata().

encode_update_metadata_response_8(
    _Args = #{
        % The correlation ID of this request.
        correlation_id := CorrelationId,
        % The error code, or 0 if there was no error.
        error_code := ErrorCode
    }
) when
    ?is_int32(CorrelationId),
    ?is_int16(ErrorCode)
->
    [
        ?encode_response_header_1(CorrelationId),
        ?encode_int16(ErrorCode),
        ?EMPTY_TAG_BUFFER
    ];
encode_update_metadata_response_8(Args) ->
    ?encoder_error(Args, #{
        correlation_id => int32,
        error_code => int16
    }).

-spec decode_update_metadata_response_8(binary()) -> {Decoded, Rest} when
    Decoded :: update_metadata_response_8(),
    Rest :: binary().

decode_update_metadata_response_8(Bin) when is_binary(Bin) ->
    {Header, Bin0} = ?decode_response_header_1(Bin),
    ?_decode_int16(ErrorCode, Bin0, Bin1),
    ?decode_tagged_fields(
        fun decode_update_metadata_response_8_tagged_field/3,
        Header#{
            error_code => ErrorCode
        },
        Bin1
    ).

-spec decode_update_metadata_response_8_tagged_field(Tag, Input, AccIn) -> AccOut when
    Tag :: non_neg_integer(),
    Input :: binary(),
    AccIn :: Acc,
    AccOut :: Acc.

decode_update_metadata_response_8_tagged_field(_Tag, _Bin0, Acc) ->
    % Unrecognised tag; ignore it.
    Acc.

-type update_metadata_response_0() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type update_metadata_response_1() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type update_metadata_response_2() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type update_metadata_response_3() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type update_metadata_response_4() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type update_metadata_response_5() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type update_metadata_response_6() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type update_metadata_response_7() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
-type update_metadata_response_8() :: #{
    correlation_id => integer(),
    error_code := integer()
}.
