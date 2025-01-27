-module(kafcod_primitives_uuid_tests).
-include_lib("eunit/include/eunit.hrl").
-include("uuid.hrl").

% Serialization for UUIDs is ... tricky.

% The Kafka documentation states that a UUID "Represents a type 4 immutable universally unique identifier (Uuid). The
% values are encoded using sixteen bytes in network byte order (big-endian)."

% But if you look at the Kafka source code, it simply treats the UUID as a pair of 64-bit integers. When converting them
% to a string, it base64-encodes them. This is at odds with the standard 8-4-4-4-12 string format.

% According to [Wikipedia](https://en.wikipedia.org/wiki/Universally_unique_identifier#Encoding), "Variant 1 UUIDs,
% nowadays the most common variant, are encoded in a big-endian format. For example,
% 00112233-4455-6677-8899-aabbccddeeff is encoded as the bytes 00 11 22 33 44 55 66 77 88 99 aa bb cc dd ee ff".

% Note that we don't wrap the UUID in braces.

decode_uuid_test() ->
    ?assertEqual(
        {<<"00112233-4455-6677-8899-aabbccddeeff">>, <<>>},
        kafcod_primitives:decode_uuid(
            <<16#00, 16#11, 16#22, 16#33, 16#44, 16#55, 16#66, 16#77, 16#88, 16#99, 16#aa, 16#bb,
                16#cc, 16#dd, 16#ee, 16#ff>>
        )
    ).

encode_uuid_test() ->
    ?assertEqual(
        <<16#00, 16#11, 16#22, 16#33, 16#44, 16#55, 16#66, 16#77, 16#88, 16#99, 16#aa, 16#bb, 16#cc,
            16#dd, 16#ee, 16#ff>>,
        kafcod_primitives:encode_uuid(<<"00112233-4455-6677-8899-aabbccddeeff">>)
    ).

byte_ordering_test_() ->
    % Our mock broker, kamock, generates the topic ID as a v5 UUID (SHA1 hash of the topic name).
    [
        % The e19ß.. value is displayed in Wireshark (in a Metadata v12 response), which means we've got the byte ordering
        % correct (see the comments above).
        ?_assertEqual(
            <<"e1952705-cc0e-5ec2-a2d6-720f6ace41fd">>,
            uuid:uuid_to_string(uuid:get_v5(<<"cars">>), binary_standard)
        ),
        % It should be encoded as bytes like this:
        ?_assertEqual(
            <<225, 149, 39, 5, 204, 14, 94, 194, 162, 214, 114, 15, 106, 206, 65, 253>>,
            kafcod_primitives:encode_uuid(<<"e1952705-cc0e-5ec2-a2d6-720f6ace41fd">>)
        ),
        % In hex, so you can see the ordering.
        ?_assertEqual(
            <<16#e1, 16#95, 16#27, 16#05, 16#cc, 16#0e, 16#5e, 16#c2, 16#a2, 16#d6, 16#72, 16#0f,
                16#6a, 16#ce, 16#41, 16#fd>>,
            kafcod_primitives:encode_uuid(<<"e1952705-cc0e-5ec2-a2d6-720f6ace41fd">>)
        ),
        % When librdkafka displays that UUID, we get "4ZUnBcwOXsKi1nIPas5B/Q".
        ?_assertEqual(
            <<"4ZUnBcwOXsKi1nIPas5B/Q">>,
            base64:encode(
                <<16#e1, 16#95, 16#27, 16#05, 16#cc, 16#0e, 16#5e, 16#c2, 16#a2, 16#d6, 16#72,
                    16#0f, 16#6a, 16#ce, 16#41, 16#fd>>,
                #{padding => false}
            )
        )
    ].

null_uuid_test_() ->
    % When using (e.g.) Metadata v12 and you don't know the topic's ID, you need to provide an all-zero UUID. Do we encode/decode that correctly?
    [
        ?_assertEqual(
            {?NULL_UUID, <<>>},
            kafcod_primitives:decode_uuid(<<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>)
        ),
        ?_assertEqual(
            <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
            kafcod_primitives:encode_uuid(?NULL_UUID)
        )
    ].
