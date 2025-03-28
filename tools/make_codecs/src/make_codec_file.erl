-module(make_codec_file).
-export([format_file/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(NL, [io_lib:nl()]).

format_file(ModuleName, MessageName, MessageType, Schema) ->
    ValidVersions = version:parse_version_range(maps:get(<<"validVersions">>, Schema)),
    Codecs = get_codecs(MessageName, MessageType, Schema, ValidVersions),

    [
        io_lib:format("-module(~s).~n", [ModuleName]),
        format_exports(MessageName, ValidVersions),
        format_exported_types(Codecs),
        format_includes(),
        case MessageType of
            <<"request">> ->
                ApiKeyName = string:uppercase(casey:underscore(MessageName)),
                ApiKey = maps:get(<<"apiKey">>, Schema),
                io_lib:format("-define(~s, ~B).~n", [ApiKeyName, ApiKey]);
            _ ->
                []
        end,
        % Wrap tag buffer in a useless list, to make it easier to recognise in the encoded output.
        "-define(EMPTY_TAG_BUFFER, [<<0:8/big>>]).",
        ?NL,
        ?NL,
        format_codecs(Codecs),
        ?NL,
        format_types(Codecs),
        ?NL
    ].

format_exports(MessageName, ValidVersions) ->
    [
        "-export([",
        ?NL,
        lists:join(
            [",", ?NL],
            lists:foldr(
                fun(Version, Acc) ->
                    [
                        [
                            io_lib:format("    encode_~s_~B/1", [
                                casey:underscore(MessageName), Version
                            ]),
                            ",",
                            ?NL,
                            io_lib:format("    decode_~s_~B/1", [
                                casey:underscore(MessageName), Version
                            ])
                        ]
                        | Acc
                    ]
                end,
                [],
                version:version_seq(ValidVersions)
            )
        ),
        ?NL,
        "]).",
        ?NL
    ].

format_exported_types(Codecs) ->
    [
        "-export_type([",
        ?NL,
        lists:join(
            [",", ?NL],
            lists:map(
                fun(#{name := Name, version := Version}) ->
                    io_lib:format("    ~s_~B/0", [casey:underscore(Name), Version])
                end,
                Codecs
            )
        ),
        ?NL,
        "]).",
        ?NL
    ].

format_includes() ->
    Includes = ["../encoders.hrl", "../decoders.hrl", "../guards.hrl", "../error.hrl"],
    [
        [io_lib:format("-include(\"~s\").~n", [Include]) || Include <- Includes],
        ?NL
    ].

get_codecs(MessageName, MessageType, Schema, ValidVersions) ->
    % See KIP-482: adds tagged fields; strings and arrays are compact.
    FlexibleVersions = version:parse_version_range(maps:get(<<"flexibleVersions">>, Schema)),

    lists:foldl(
        fun(Version, Acc) ->
            IsFlexible = version:is_version_in_range(Version, FlexibleVersions),
            Fields0 = maps:get(<<"fields">>, Schema, []),
            Fields = version:fields_for_version(Version, Fields0),
            Codec = #{
                name => MessageName,
                version => Version,
                type => MessageType,
                flexible => IsFlexible,
                fields => transform_fields(Fields, IsFlexible, Version)
            },
            % This ordering is slightly weird; it's attempting to keep the message codecs first, followed by the nested
            % codecs, followed by the common struct codecs. The compiler doesn't care, so feel free to tidy it up if
            % it's bothering you.
            Acc ++ [Codec | add_extra_codecs(Fields, IsFlexible, Version, [])] ++
                add_common_structs(Schema, Version, IsFlexible)
        end,
        [],
        version:version_seq(ValidVersions)
    ).

add_common_structs(Schema, Version, IsFlexible) ->
    lists:foldl(
        fun(Struct = #{<<"name">> := Name, <<"versions">> := Versions0}, Acc) ->
            Versions = version:parse_version_range(Versions0),
            case version:is_version_in_range(Version, Versions) of
                true ->
                    Fields0 = maps:get(<<"fields">>, Struct, []),
                    Fields = version:fields_for_version(Version, Fields0),
                    Codec = #{
                        name => Name,
                        version => Version,
                        type => <<"commonStruct">>,
                        flexible => IsFlexible,
                        fields => transform_fields(Fields, IsFlexible, Version)
                    },
                    %add_extra_codecs(Fields, IsFlexible, Version, Acc);
                    [Codec | Acc];
                false ->
                    Acc
            end
        end,
        [],
        maps:get(<<"commonStructs">>, Schema, [])
    ).

transform_fields(Fields, IsFlexible, Version) ->
    lists:map(fun(Field) -> transform_field(Field, IsFlexible, Version) end, Fields).

transform_field(Field = #{<<"name">> := Name, <<"type">> := Type}, IsFlexible, Version) ->
    Transform0 = #{
        name => Name,
        type => Type,
        flexible => IsFlexible,
        version => Version
    },

    NullableVersions = version:parse_version_range(
        maps:get(<<"nullableVersions">>, Field, <<"none">>)
    ),
    IsNullable = version:is_version_in_range(Version, NullableVersions),
    Transform1 = Transform0#{
        nullable => IsNullable
    },

    TaggedVersions = version:parse_version_range(
        maps:get(<<"taggedVersions">>, Field, <<"none">>)
    ),
    Transform2 =
        case version:is_version_in_range(Version, TaggedVersions) of
            false ->
                Transform1;
            true ->
                Transform1#{tag => maps:get(<<"tag">>, Field)}
        end,

    Transform3 =
        case maps:get(<<"about">>, Field, undefined) of
            undefined ->
                Transform2;
            About ->
                Transform2#{about => About}
        end,
    Transform3.

add_extra_codecs(ExtraFields, IsFlexible, Version, Acc0) ->
    lists:foldr(
        fun
            (
                #{
                    <<"type">> := Type0,
                    <<"fields">> := Fields0
                },
                Acc
            ) ->
                Type =
                    case Type0 of
                        <<"[]", ElementType/binary>> -> ElementType;
                        _ -> Type0
                    end,
                Fields = version:fields_for_version(Version, Fields0),
                Codec = #{
                    name => Type,
                    version => Version,
                    flexible => IsFlexible,
                    fields => transform_fields(Fields, IsFlexible, Version),
                    type => nested
                },
                add_extra_codecs(Fields, IsFlexible, Version, [Codec | Acc]);
            (_, Acc) ->
                Acc
        end,
        Acc0,
        ExtraFields
    ).

format_codecs(Codecs) ->
    [
        lists:join(
            ?NL,
            lists:map(
                fun(Codec) ->
                    [
                        format_encoder(Codec),
                        format_decoder(Codec)
                    ]
                end,
                Codecs
            )
        )
    ].

format_encoder(
    _Codec = #{
        name := Name, type := Type, version := Version, fields := Fields, flexible := IsFlexible
    }
) ->
    HeaderFields = get_header_fields(Name, Type, Version),

    % Tagged fields are not required.
    {RequiredFields, TaggedFields} = lists:partition(
        fun
            (#{tag := _}) -> false;
            (_) -> true
        end,
        Fields
    ),

    [
        format_encoder_spec(Name, Version),
        ?NL,
        format_encoder_head(
            Name, Version, HeaderFields ++ RequiredFields, IsFlexible, TaggedFields
        ),
        "    [",
        [
            ?NL,
            format_header_encoder(Name, Type, Version, IsFlexible),
            format_header_encoder_separator(Type, RequiredFields),
            format_field_encoders(RequiredFields, IsFlexible),
            format_tagged_field_encoders(Name, Version, IsFlexible, TaggedFields)
        ],
        "    ];",
        ?NL,
        format_encoder_error_handler(Name, Version, HeaderFields ++ RequiredFields),
        ?NL,
        format_tagged_fields_encoder(Name, Type, Version, TaggedFields, IsFlexible)
    ].

format_encoder_spec(Name, Version) ->
    [
        "-spec ",
        format_encoder_function_name(Name, Version),
        "(",
        io_lib:format("~s_~B()", [casey:underscore(Name), Version]),
        ") -> iodata().",
        ?NL
    ].

format_encoder_head(Name, Version, Fields, IsFlexible, TaggedFields) ->
    [
        format_encoder_function_name(Name, Version),
        format_encoder_parameters(Fields, IsFlexible, TaggedFields),
        " when",
        ?NL,
        format_encoder_guards(Fields),
        "->",
        ?NL
    ].

format_encoder_function_name(Name, Version) ->
    io_lib:format("encode_~s_~B", [casey:underscore(Name), Version]).

format_encoder_parameters(Fields, IsFlexible, TaggedFields) ->
    [
        "(",
        ?NL,
        case {IsFlexible, TaggedFields} of
            {true, [_ | _]} ->
                "    Args = #{";
            _ ->
                "    _Args = #{"
        end,
        ?NL,
        lists:join(
            [",", ?NL],
            lists:map(
                fun format_encoder_parameter/1,
                Fields
            )
        ),
        ?NL,
        "    }",
        ?NL,
        ")"
    ].

format_encoder_parameter(#{name := FieldName, about := About}) ->
    [
        io_lib:format("        % ~s~n", [About]),
        io_lib:format("        ~s := ~s", [
            casey:underscore(FieldName), casey:title(FieldName)
        ])
    ];
format_encoder_parameter(#{name := FieldName}) ->
    io_lib:format("        ~s := ~s", [
        casey:underscore(FieldName), casey:title(FieldName)
    ]).

format_encoder_guards(Fields) ->
    [
        lists:join(
            [",", ?NL],
            lists:map(
                fun format_encoder_guard/1,
                Fields
            )
        ),
        ?NL
    ].

format_encoder_guard(
    #{name := Name, type := <<"[]", _/binary>>, nullable := true}
) ->
    io_lib:format("    ?is_nullable_array(~s)", [casey:title(Name)]);
format_encoder_guard(
    #{name := Name, type := <<"[]", _/binary>>, nullable := false}
) ->
    io_lib:format("    ?is_array(~s)", [casey:title(Name)]);
format_encoder_guard(
    #{name := Name, type := <<H, _/binary>>, nullable := false}
) when H >= $A, H =< $Z ->
    io_lib:format("    ?is_entity(~s)", [casey:title(Name)]);
format_encoder_guard(
    #{name := Name, type := <<H, _/binary>>, nullable := true}
) when H >= $A, H =< $Z ->
    io_lib:format("    ?is_nullable_entity(~s)", [casey:title(Name)]);
format_encoder_guard(#{name := Name, type := Type, nullable := true}) ->
    io_lib:format("    ?is_nullable_~s(~s)", [Type, casey:title(Name)]);
format_encoder_guard(#{name := Name, type := Type}) ->
    io_lib:format("    ?is_~s(~s)", [Type, casey:title(Name)]).

get_header_fields(_Name = <<"ControlledShutdownRequest">>, _Type = <<"request">>, _Version = 0) ->
    [
        #{
            name => <<"CorrelationId">>,
            type => <<"int32">>,
            nullable => false,
            about => <<"The correlation ID of this request.">>,
            version => undefined
        }
    ];
get_header_fields(_Name, <<"request">>, _Type) ->
    [
        #{
            name => <<"CorrelationId">>,
            type => <<"int32">>,
            nullable => false,
            about => <<"The correlation ID of this request.">>,
            version => undefined
        },
        #{
            name => <<"ClientId">>,
            type => <<"string">>,
            % ClientId is not a compact string.
            flexible => false,
            % It is allowed to be null.
            nullable => true,
            about => <<"The client ID string.">>,
            version => undefined
        }
    ];
get_header_fields(_Name, <<"response">>, _Type) ->
    [
        #{
            name => <<"CorrelationId">>,
            type => <<"int32">>,
            nullable => false,
            about => <<"The correlation ID of this request.">>,
            version => undefined
        }
    ];
get_header_fields(_Name, _, _Type) ->
    [].

format_header_encoder(
    MessageName = <<"ControlledShutdownRequest">>, <<"request">>, Version = 0, IsFlexible
) ->
    ApiKeyName = string:uppercase(casey:underscore(MessageName)),
    HeaderType = get_request_header_type(MessageName, Version, IsFlexible),
    io_lib:format("        ?encode_~s(?~s, ~B, CorrelationId)", [
        HeaderType, ApiKeyName, Version
    ]);
format_header_encoder(MessageName, <<"request">>, Version, IsFlexible) ->
    ApiKeyName = string:uppercase(casey:underscore(MessageName)),
    HeaderType = get_request_header_type(MessageName, Version, IsFlexible),
    io_lib:format("        ?encode_~s(?~s, ~B, CorrelationId, ClientId)", [
        HeaderType, ApiKeyName, Version
    ]);
format_header_encoder(MessageName, <<"response">>, Version, IsFlexible) ->
    HeaderType = get_response_header_type(MessageName, Version, IsFlexible),
    io_lib:format("        ?encode_~s(CorrelationId)", [HeaderType]);
format_header_encoder(_MessageName, _Type, _Version, _IsFlexible) ->
    [].

format_header_encoder_separator(Type, _RequiredFields = [_ | _]) when
    Type =:= <<"request">>; Type =:= <<"response">>
->
    % If this is a request or a response, then we must have output a header encoder. If there are required fields, then
    % we need a separator.
    [",", ?NL];
format_header_encoder_separator(_Type, _RequiredFields) ->
    % If this is NOT a request or a response, then we didn't output a header encoder, so we don't need a separator. If
    % we did, but there are no required fields, then we don't need a separator.
    [].

get_request_header_type(<<"ControlledShutdownRequest">>, _Version = 0, _IsFlexible) ->
    % "Version 0 of the RequestHeader is only used by v0 of ControlledShutdownRequest."
    % See:
    % - https://github.com/apache/kafka/blob/3.4.0/generator/src/main/java/org/apache/kafka/message/ApiMessageTypeGenerator.java#L343
    % - https://github.com/apache/kafka/blob/3.4.0/clients/src/main/resources/common/message/ControlledShutdownRequest.json#L21
    % - https://github.com/apache/kafka/blob/3.4.0/clients/src/main/resources/common/message/RequestHeader.json#L19
    <<"request_header_0">>;
get_request_header_type(_, _, _IsFlexible = false) ->
    <<"request_header_1">>;
get_request_header_type(_, _, _IsFlexible = true) ->
    <<"request_header_2">>.

get_response_header_type(<<"ApiVersionsResponse">>, _Version, _IsFlexible) ->
    % ApiVersionsResponse always includes a v0 header.
    % See KIP-511 for details.
    <<"response_header_0">>;
get_response_header_type(_MessageName, _Version, _IsFlexible = false) ->
    <<"response_header_0">>;
get_response_header_type(_MessageName, _Version, _IsFlexible = true) ->
    <<"response_header_1">>.

format_field_encoders(Fields, IsFlexible) ->
    [
        lists:join(
            [",", ?NL],
            lists:map(fun(Field) -> ["        ", format_field_encoder(Field)] end, Fields)
        ),
        format_field_encoders_end(IsFlexible)
    ].

format_field_encoders_end(_IsFlexible = false) ->
    ?NL;
format_field_encoders_end(_IsFlexible = true) ->
    [",", ?NL].

format_field_encoder(Field = #{type := <<"[]", _/binary>>}) ->
    format_array_encoder(Field);
format_field_encoder(Field = #{type := <<H, _/binary>>}) when H >= $A, H =< $Z ->
    format_complex_encoder(Field);
format_field_encoder(Field) ->
    format_simple_encoder(Field).

format_simple_encoder(
    #{name := Name, type := Type, flexible := IsFlexible, nullable := IsNullable}
) when Type =:= <<"string">>; Type =:= <<"bytes">>; Type =:= <<"records">> ->
    io_lib:format("?encode_~s~s(~s)", [get_flavour(IsFlexible, IsNullable), Type, casey:title(Name)]);
format_simple_encoder(#{name := Name, type := Type}) ->
    io_lib:format("?encode_~s(~s)", [Type, casey:title(Name)]).

format_array_encoder(#{
    name := Name,
    type := <<"[]", ElementType/binary>>,
    version := Version,
    nullable := IsNullable,
    flexible := IsFlexible
}) ->
    io_lib:format("?encode_~sarray(~s, ~s)", [
        get_flavour(IsFlexible, IsNullable),
        casey:title(Name),
        get_element_encoder(ElementType, Version, IsFlexible)
    ]).

format_complex_encoder(_Field = #{name := Name, type := Type, version := Version}) ->
    io_lib:format("encode_~s_~B(~s)", [casey:underscore(Type), Version, casey:title(Name)]).

get_element_encoder(ElementType = <<"string">>, _Version, _IsFlexible = true) ->
    io_lib:format("?encode_compact_~s_", [ElementType]);
get_element_encoder(ElementType = <<"string">>, _Version, _IsFlexible) ->
    io_lib:format("?encode_~s_", [ElementType]);
get_element_encoder(ElementType = <<H, _/binary>>, _Version, _IsFlexible) when H >= $a, H =< $z ->
    % primitive types start with lower-case letters.
    io_lib:format("?encode_~s_", [ElementType]);
get_element_encoder(ElementType, Version, _IsFlexible) ->
    io_lib:format("fun encode_~s_~B/1", [casey:underscore(ElementType), Version]).

format_encoder_error_handler(Name, Version, Fields) ->
    [
        io_lib:format("encode_~s_~B(Args) ->~n", [casey:underscore(Name), Version]),
        "    ?encoder_error(Args, #{",
        ?NL,
        lists:join(
            [",", ?NL],
            lists:map(fun format_error_type/1, Fields)
        ),
        ?NL,
        "    })."
    ].

format_element_type(ElementType = <<H, _/binary>>, Version) when H >= $A, H =< $Z ->
    io_lib:format("~s_~B", [casey:underscore(ElementType), Version]);
format_element_type(ElementType, _Version) ->
    ElementType.

format_error_type(#{
    name := Key, type := <<"[]", ElementType/binary>>, version := Version, nullable := false
}) ->
    io_lib:format("        ~s => {array, ~s}", [
        casey:underscore(Key), format_element_type(ElementType, Version)
    ]);
format_error_type(#{
    name := Key, type := <<"[]", ElementType/binary>>, version := Version, nullable := true
}) ->
    io_lib:format("        ~s => {nullable_array, ~s}", [
        casey:underscore(Key), format_element_type(ElementType, Version)
    ]);
format_error_type(#{name := Key, type := Type, nullable := true}) ->
    io_lib:format("        ~s => nullable_~s", [casey:underscore(Key), Type]);
format_error_type(#{name := Key, type := <<H, _/binary>>}) when H >= $A, H =< $Z ->
    io_lib:format("        ~s => map", [casey:underscore(Key)]);
format_error_type(#{name := Key, type := Type}) ->
    io_lib:format("        ~s => ~s", [casey:underscore(Key), Type]).

format_tagged_field_encoders(_Name, _Version, _IsFlexible = false, _TaggedFields = []) ->
    [];
format_tagged_field_encoders(Name, Version, _IsFlexible = true, _TaggedFields = [_ | _]) ->
    [
        "        ?encode_tagged_fields(",
        ?NL,
        io_lib:format("            fun encode_~s_~B_tagged_field/2,", [
            casey:underscore(Name), Version
        ]),
        ?NL,
        "            Args",
        ?NL,
        "        )",
        ?NL
    ];
format_tagged_field_encoders(_Name, _Version, _IsFlexible = true, _TaggedFields = []) ->
    [
        "        ?EMPTY_TAG_BUFFER",
        ?NL
    ].

format_tagged_fields_encoder(Name, _Type, Version, TaggedFields = [_ | _], _IsFlexible = true) ->
    [
        ?NL,
        "-spec ",
        format_tagged_field_encoder_name(Name, Version),
        % TODO: Use the correct type for 'Value'. That's tricky, though, because the type varies based on the key, so
        % we'd need to collect them. This'll do for now.
        "(Key :: atom(), Value :: term()) -> iodata() | ignore.",
        ?NL,

        ?NL,
        lists:join(
            ?NL,
            lists:map(
                fun(Field) ->
                    format_tagged_field_encoder(Name, Version, Field)
                end,
                TaggedFields
            ) ++ [format_unrecognised_tagged_field_encoder(Name, Version)]
        ),
        ?NL
    ];
format_tagged_fields_encoder(_Name, _Type, _Version, _TaggedFields, _IsFlexible) ->
    [].

format_tagged_field_encoder(Name, Version, Field = #{name := FieldName, tag := Tag}) ->
    [
        format_tagged_field_encoder_name(Name, Version),
        io_lib:format("(_Key = ~s, ~s) ->", [casey:underscore(FieldName), FieldName]),
        ?NL,
        format_tagged_field_encoder_tag(Tag),
        format_field_encoder(Field),
        "};"
    ].

format_tagged_field_encoder_tag(Tag) when is_integer(Tag) ->
    io_lib:format("    {~B, ", [Tag]);
format_tagged_field_encoder_tag(Tag) when is_binary(Tag) ->
    io_lib:format("    {~B, ", [binary_to_integer(Tag)]).

format_unrecognised_tagged_field_encoder(Name, Version) ->
    [
        format_tagged_field_encoder_name(Name, Version),
        "(_Key, _Value) ->",
        ?NL,
        "    ignore."
    ].

format_tagged_field_encoder_name(Name, Version) ->
    io_lib:format("encode_~s_~B_tagged_field", [casey:underscore(Name), Version]).

-ifdef(TEST).
format_field_encoder_test_() ->
    [
        % From LeaveGroupRequest:
        ?_assertEqual(
            <<"?encode_string(GroupId)">>,
            iolist_to_binary(
                format_field_encoder(#{
                    name => <<"GroupId">>,
                    type => <<"string">>,
                    version => 3,
                    nullable => false,
                    flexible => false
                })
            )
        ),
        ?_assertEqual(
            <<"?encode_compact_string(GroupId)">>,
            iolist_to_binary(
                format_field_encoder(#{
                    name => <<"GroupId">>,
                    type => <<"string">>,
                    version => 4,
                    nullable => false,
                    flexible => true
                })
            )
        ),
        ?_assertEqual(
            <<"?encode_array(Members, fun encode_member_identity_3/1)">>,
            iolist_to_binary(
                format_field_encoder(#{
                    name => <<"Members">>,
                    type => <<"[]MemberIdentity">>,
                    version => 3,
                    nullable => false,
                    flexible => false
                })
            )
        ),
        ?_assertEqual(
            <<"?encode_compact_array(Members, fun encode_member_identity_4/1)">>,
            iolist_to_binary(
                format_field_encoder(#{
                    name => <<"Members">>,
                    type => <<"[]MemberIdentity">>,
                    version => 4,
                    nullable => false,
                    flexible => true
                })
            )
        )
    ].
-endif.

format_decoder(
    _Codec = #{
        name := Name,
        type := Type,
        version := Version,
        fields := Fields,
        flexible := IsFlexible
    }
) ->
    % There's an annoying asymmetry between the way that encoders and decoders handle headers.

    % When encoding, the sender doesn't know what kind of header to attach, because that's tied to the particular
    % version of each message.

    % But, when decoding, the receiver needs to parse the header before it can decide which decoder to use: the client
    % needs to see CorrelationId before it can choose the correct response decoder; the broker needs to see ApiKey and
    % ApiVersion before it can choose the correct request decoder.

    % That's the asymmetry.

    % The annoying part comes after the receiver has (partially) parsed the header to find the correct decoder, it
    % _still_ needs the decoder to parse the rest of the header (or the entire header) because only the decoder knows
    % whether it's a "flexible" header or not, based on the ApiKey/ApiVersion.

    {RequiredFields, TaggedFields} = lists:partition(
        fun
            (#{tag := _}) -> false;
            (_) -> true
        end,
        Fields
    ),

    [
        format_decoder_spec(Name, Version),
        format_decoder_head(Name, Type, Version),
        format_header_decoder(Name, Type, Version, IsFlexible),
        format_decoder_body(Name, Type, Version, RequiredFields, IsFlexible),
        format_tagged_fields_decoder(Name, Type, Version, TaggedFields, IsFlexible)
    ].

format_decoder_spec(Name, Version) ->
    DecodedType = io_lib:format("~s_~B()", [casey:underscore(Name), Version]),
    [
        ?NL,
        "-spec ",
        format_decoder_function_name(Name, Version),
        "(binary()) -> {Decoded, Rest} when",
        ?NL,
        ["    Decoded :: ", DecodedType, ",", ?NL],
        "    Rest :: binary().",
        ?NL
    ].

format_decoder_head(Name, Type, Version) when Type =:= <<"request">>; Type =:= <<"response">> ->
    % Decoders for top-level messages (request, response) add a macro for decoding the header; see below, so we start
    % with 'Bin', rather than 'Bin0'.
    [
        ?NL,
        format_decoder_function_name(Name, Version),
        "(Bin) when is_binary(Bin) ->",
        ?NL
    ];
format_decoder_head(Name, _Type, Version) ->
    % Decoders for embedded objects don't have a header, so they start with 'Bin0'.
    [
        ?NL,
        format_decoder_function_name(Name, Version),
        "(Bin0) when is_binary(Bin0) ->",
        ?NL
    ].

format_decoder_function_name(Name, Version) ->
    io_lib:format("decode_~s_~B", [casey:underscore(Name), Version]).

format_decoder_body(Name, Type, Version, Fields, IsFlexible) ->
    {Decoders, Last} = format_field_decoders(Fields),
    [
        Decoders,
        wrap_decoder_result(
            Name,
            Version,
            format_decoder_result(Type, Fields),
            Last,
            IsFlexible
        )
    ].

format_field_decoders(Fields) ->
    format_field_decoders(Fields, [], 0).

format_field_decoders([Field | Fields], Acc, Index) ->
    format_field_decoders(
        Fields, [Acc, [format_field_decoder(Field, Index), ",", ?NL]], Index + 1
    );
format_field_decoders([], Acc, Index) ->
    {Acc, Index}.

wrap_decoder_result(_Name, _Version, Result, Last, _IsFlexible = false) ->
    [
        "    {",
        ?NL,
        Result,
        ",",
        ?NL,
        io_lib:format("        Bin~B", [Last]),
        ?NL,
        "    }.",
        ?NL
    ];
wrap_decoder_result(Name, Version, Result, Last, _IsFlexible = true) ->
    [
        "    ?decode_tagged_fields(",
        ?NL,

        io_lib:format("        fun decode_~s_~B_tagged_field/3,", [
            casey:underscore(Name), Version
        ]),
        ?NL,
        Result,
        ",",
        ?NL,
        io_lib:format("        Bin~B", [Last]),
        ?NL,
        "    ).",
        ?NL
    ].

format_decoder_result(Type, Fields) ->
    [
        case Type of
            T when T =:= <<"request">>; T =:= <<"response">> ->
                "        Header#{";
            _ ->
                "        #{"
        end,
        ?NL,
        format_decoder_result_fields(Fields),
        ?NL,
        "        }"
    ].

format_decoder_result_fields(Fields) ->
    lists:join(
        [",", ?NL],
        lists:filtermap(
            fun(#{name := Name}) ->
                {true,
                    io_lib:format("            ~s => ~s", [
                        casey:underscore(Name), casey:title(Name)
                    ])}
            end,
            Fields
        )
    ).

format_tagged_fields_decoder(_Name, _Type, _Version, _TaggedFields, _IsFlexible = false) ->
    [];
format_tagged_fields_decoder(Name, _Type, Version, TaggedFields, _IsFlexible = true) ->
    [
        ?NL,
        format_tagged_fields_decoder_spec(Name, Version),
        ?NL,
        lists:join(
            ?NL,
            lists:map(
                fun(Field) ->
                    format_tagged_field_decoder(Name, Version, Field)
                end,
                TaggedFields
            ) ++ [format_unrecognised_tagged_field_decoder(Name, Version)]
        )
    ].

format_tagged_fields_decoder_spec(Name, Version) ->
    [
        "-spec ",
        format_tagged_field_decoder_name(Name, Version),
        "(Tag, Input, AccIn) -> AccOut when",
        ?NL,
        "    Tag :: non_neg_integer(),",
        ?NL,
        "    Input :: binary(),",
        ?NL,
        "    AccIn :: Acc,",
        ?NL,
        "    AccOut :: Acc.",
        ?NL
    ].

format_unrecognised_tagged_field_decoder(Name, Version) ->
    [
        format_tagged_field_decoder_name(Name, Version),
        "(_Tag, _Bin0, Acc) ->",
        ?NL,
        "    % Unrecognised tag; ignore it.",
        ?NL,
        "    Acc.",
        ?NL
    ].

format_tagged_field_decoder(Name, Version, Field = #{name := FieldName, tag := Tag}) ->
    [
        io_lib:format("%% ~s~n", [FieldName]),
        format_tagged_field_about(Field),
        format_tagged_field_decoder_name(Name, Version),
        format_tagged_field_decoder_tag(Tag),
        ?NL,
        format_field_decoder(Field, 0),
        [",", ?NL],
        "    <<>> = Bin1,",
        ?NL,
        io_lib:format("    Acc#{~s => ~s};", [casey:underscore(FieldName), FieldName])
    ].

format_tagged_field_decoder_tag(Tag) when is_integer(Tag) ->
    io_lib:format("(_Tag = ~B, Bin0, Acc) ->", [Tag]);
format_tagged_field_decoder_tag(Tag) when is_binary(Tag) ->
    io_lib:format("(_Tag = ~B, Bin0, Acc) ->", [binary_to_integer(Tag)]).

format_tagged_field_about(_Field = #{about := About}) ->
    % TODO: Wrap the comment at ~100 characters.
    io_lib:format("%% ~s~n", [About]);
format_tagged_field_about(_Field) ->
    [].

format_tagged_field_decoder_name(Name, Version) ->
    io_lib:format("decode_~s_~B_tagged_field", [casey:underscore(Name), Version]).

format_header_decoder(MessageName, _Type = <<"request">>, Version, IsFlexible) ->
    HeaderType = get_request_header_type(MessageName, Version, IsFlexible),
    io_lib:format("    {Header, Bin0} = ?decode_~s(Bin),~n", [HeaderType]);
format_header_decoder(MessageName, _Type = <<"response">>, Version, IsFlexible) ->
    HeaderType = get_response_header_type(MessageName, Version, IsFlexible),
    io_lib:format("    {Header, Bin0} = ?decode_~s(Bin),~n", [HeaderType]);
format_header_decoder(_MessageName, _Type, _Version, _IsFlexible) ->
    [].

format_field_decoder(
    #{
        name := Name,
        type := <<"[]", ElementType/binary>>,
        version := Version,
        nullable := IsNullable,
        flexible := IsFlexible
    },
    Index
) ->
    io_lib:format("    ?_decode_~sarray(~s, Bin~B, Bin~B, ~s)", [
        get_flavour(IsFlexible, IsNullable),
        casey:title(Name),
        Index,
        Index + 1,
        get_element_decoder(ElementType, Version)
    ]);
format_field_decoder(
    #{name := Name, type := Type = <<H, _/binary>>, version := Version}, Index
) when H >= $A, H =< $Z ->
    io_lib:format(
        "    ?_decode_entity(~s, Bin~B, Bin~B, decode_~s_~B)",
        [casey:title(Name), Index, Index + 1, casey:underscore(Type), Version]
    );
format_field_decoder(
    #{name := Name, type := Type, nullable := IsNullable, flexible := IsFlexible}, Index
) when Type =:= <<"string">>; Type =:= <<"bytes">>; Type =:= <<"records">> ->
    io_lib:format("    ?_decode_~s~s(~s, Bin~B, Bin~B)", [
        get_flavour(IsFlexible, IsNullable), Type, casey:title(Name), Index, Index + 1
    ]);
format_field_decoder(#{name := Name, type := Type}, Index) ->
    io_lib:format("    ?_decode_~s(~s, Bin~B, Bin~B)", [
        casey:underscore(Type), casey:title(Name), Index, Index + 1
    ]).

get_element_decoder(ElementType = <<H, _/binary>>, Version) when H >= $A, H =< $Z ->
    io_lib:format("?_decode_element(decode_~s_~B)", [casey:underscore(ElementType), Version]);
get_element_decoder(ElementType, _Version) ->
    io_lib:format("?decode_~s_", [ElementType]).

get_flavour(_IsFlexible = false, _IsNullable = false) -> <<"">>;
get_flavour(_IsFlexible = false, _IsNullable = true) -> <<"nullable_">>;
get_flavour(_IsFlexible = true, _IsNullable = false) -> <<"compact_">>;
get_flavour(_IsFlexible = true, _IsNullable = true) -> <<"compact_nullable_">>.

format_types(Codecs) ->
    lists:join(
        ?NL,
        lists:map(
            fun(Codec = #{name := Name, version := Version, fields := Fields, type := _Type}) ->
                % The ApiKey, ApiVersion, CorrelationId, ClientId, etc. fields are "optional" in the sense that a client
                % doesn't need to specify them. But we include them in the typespec because they're included by the
                % decoder. They're included in the decoder because a broker or server needs them.
                [
                    io_lib:format("-type ~s_~B() :: #{", [casey:underscore(Name), Version]),
                    ?NL,
                    format_header_field_type_associations(Codec),
                    lists:join(
                        [",", ?NL],
                        lists:map(fun format_type_association/1, Fields)
                    ),
                    ?NL,
                    "}."
                ]
            end,
            Codecs
        )
    ).

format_header_field_type_associations(
    _Codec = #{name := Name, version := Version, fields := Fields, type := Type}
) when Type =:= <<"request">>; Type =:= <<"response">> ->
    OptionalFields =
        get_extra_type_fields(Type) ++ get_header_fields(Name, Type, Version),
    [
        lists:join(
            [",", ?NL],
            lists:map(fun format_optional_type_association/1, OptionalFields)
        ),
        case Fields of
            [_ | _] ->
                [",", ?NL];
            _ ->
                []
        end
    ];
format_header_field_type_associations(_) ->
    [].

get_extra_type_fields(_Type = <<"request">>) ->
    [
        #{
            name => <<"ApiKey">>,
            type => <<"int32">>,
            nullable => false,
            version => undefined
        },
        #{
            name => <<"ApiVersion">>,
            type => <<"int32">>,
            nullable => false,
            version => undefined
        }
    ];
get_extra_type_fields(_Type) ->
    [].

format_type_association(
    _Field = #{
        name := Name0,
        type := Type,
        nullable := IsNullable,
        version := Version
    }
) ->
    Name = casey:underscore(Name0),
    Indent = "    ",
    [
        Indent,
        Name,
        % TODO: tagged fields are optional.
        " := ",
        format_type_name(Name, Type, IsNullable, Version)
    ].

format_optional_type_association(
    _Field = #{
        name := Name0,
        type := Type,
        nullable := IsNullable,
        version := Version
    }
) ->
    Name = casey:underscore(Name0),
    Indent = "    ",
    [
        Indent,
        Name,
        " => ",
        format_type_name(Name, Type, IsNullable, Version)
    ].

% TODO: Use the field name to specialise 'topic', etc.
format_type_name(_FieldName, <<"string">>, _IsNullable = false, _Version) ->
    "binary()";
format_type_name(_FieldName, <<"string">>, _IsNullable = true, _Version) ->
    "binary() | null";
format_type_name(_FieldName, <<"bytes">>, _IsNullable = false, _Version) ->
    "kafcod:bytes()";
format_type_name(_FieldName, <<"bytes">>, _IsNullable = true, _Version) ->
    "kafcod:nullable_bytes()";
format_type_name(_FieldName, <<"bool">>, _IsNullable, _Version) ->
    "boolean()";
format_type_name(_FieldName, <<"int", _/binary>>, _IsNullable, _Version) ->
    "integer()";
format_type_name(_FieldName, <<"uint", _/binary>>, _IsNullable, _Version) ->
    "non_neg_integer()";
format_type_name(_FieldName, <<"float", _/binary>>, _IsNullable, _Version) ->
    "number()";
format_type_name(_FieldName, <<"uuid", _/binary>>, _IsNullable, _Version) ->
    "kafcod:uuid()";
format_type_name(_FieldName, <<"records", _/binary>>, _IsNullable, _Version) ->
    "kafcod_records:records()";
format_type_name(FieldName, <<"[]", ElementType/binary>>, _IsNullable = false, Version) ->
    io_lib:format("list(~s)", [format_type_name(FieldName, ElementType, false, Version)]);
format_type_name(FieldName, <<"[]", ElementType/binary>>, _IsNullable = true, Version) ->
    io_lib:format("list(~s) | null", [format_type_name(FieldName, ElementType, false, Version)]);
format_type_name(_FieldName, ElementType = <<H, _/binary>>, _IsNullable = false, Version) when
    H >= $A, H =< $Z
->
    io_lib:format("~s_~B()", [casey:underscore(ElementType), Version]);
format_type_name(_FieldName, ElementType = <<H, _/binary>>, _IsNullable = true, Version) when
    H >= $A, H =< $Z
->
    io_lib:format("~s_~B() | null", [casey:underscore(ElementType), Version]).

% TODO: tests that differentiate between int16 and uint16.
% TODO: tests for the EpochEndOffset thing.
