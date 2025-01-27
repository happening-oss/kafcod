-module(make_codecs).
-export([main/1]).

main([InputDir, OutputDir]) ->
    InputFiles = [
        filename:join([InputDir, InputFile])
     || InputFile <- filelib:wildcard("*.json", InputDir)
    ],
    % Make the individual codecs.
    lists:foreach(
        fun(InputFile) ->
            make_codecs(InputFile, filename:join([OutputDir, "codecs"]))
        end,
        InputFiles
    ),

    % Make the encoder/decoder mux.
    file:write_file(
        filename:join([OutputDir, "kafcod_encoders.erl"]), make_codec_muxes:format_encoder_muxes(InputFiles)
    ),
    file:write_file(
        filename:join([OutputDir, "kafcod_decoders.erl"]), make_codec_muxes:format_decoder_muxes(InputFiles)
    ),
    erlang:halt(0).

make_codecs(InputFile, OutputDir) ->
    {ok, Json} = file:read_file(InputFile),
    Schema = jsx:decode(Json),
    MessageName = maps:get(<<"name">>, Schema),
    MessageType = maps:get(<<"type">>, Schema),
    ModuleName = casey:underscore(MessageName),

    OutputFile = filename:join([OutputDir, binary_to_list(ModuleName) ++ ".erl"]),

    io:format("~s -> ~s~n", [InputFile, OutputFile]),
    make_codecs(ModuleName, MessageName, MessageType, Schema, OutputFile).

make_codecs(ModuleName, MessageName, MessageType, Schema, OutputFile) when
    MessageType =:= <<"request">>; MessageType =:= <<"response">>; MessageType =:= <<"data">>
->
    ok = file:write_file(
        OutputFile, make_codec_file:format_file(ModuleName, MessageName, MessageType, Schema)
    );
make_codecs(_Mo, _Me, _Ty, _Sc, _Ou) ->
    ok.
