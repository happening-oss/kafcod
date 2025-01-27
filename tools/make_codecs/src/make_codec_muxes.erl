-module(make_codec_muxes).
-export([
    format_encoder_muxes/1,
    format_decoder_muxes/1
]).

format_encoder_muxes(InputFiles) ->
    format_muxes(InputFiles, kafcod_encoders, encode).

format_decoder_muxes(InputFiles) ->
    format_muxes(InputFiles, kafcod_decoders, decode).

format_muxes(InputFiles, ModuleName, Direction) ->
    [
        io_lib:format("-module(~s).~n", [ModuleName]),
        io_lib:format("-export([~s/4]).~n", [Direction]),

        begin
            Schemas =
                lists:map(
                    fun(InputFile) ->
                        {ok, Json} = file:read_file(InputFile),
                        jsx:decode(Json)
                    end,
                    InputFiles
                ),

            [
                format_codec_mux(Schema, Direction)
             || Schema <- lists:sort(
                    fun
                        (#{<<"apiKey">> := A}, #{<<"apiKey">> := B}) -> A =< B;
                        (#{<<"apiKey">> := _A}, _) -> true;
                        (_, _) -> false
                    end,
                    Schemas
                )
            ]
        end,

        io_lib:format("~s(_ApiKey, _ApiVersion, _Which, _Payload) -> error(not_implemented).~n", [
            Direction
        ])
    ].

format_codec_mux(
    Schema = #{<<"name">> := MessageName, <<"type">> := MessageType, <<"apiKey">> := ApiKey},
    Direction
) when MessageType =:= <<"request">>; MessageType =:= <<"response">> ->
    ModuleName = casey:underscore(MessageName),

    ValidVersions = version:parse_version_range(maps:get(<<"validVersions">>, Schema)),
    lists:map(
        fun(ApiVersion) ->
            io_lib:format(
                "~s(_ApiKey = ~B, _ApiVersion = ~B, ~s, Payload) -> ~s:~s_~s_~B(Payload);~n",
                [
                    Direction,
                    ApiKey,
                    ApiVersion,
                    MessageType,
                    ModuleName,
                    Direction,
                    casey:underscore(MessageName),
                    ApiVersion
                ]
            )
        end,
        version:version_seq(ValidVersions)
    );
format_codec_mux(_, _) ->
    [].
