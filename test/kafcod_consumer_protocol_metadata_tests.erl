-module(kafcod_consumer_protocol_metadata_tests).
-include_lib("eunit/include/eunit.hrl").

% See also consumer_protocol_subscription_tests.erl
% 'subscription' is referred to as 'metadata' in the JoinGroup request.

v3_metadata_test() ->
    % See join_group_request_tests:v9_sticky_join_test/0
    ?assertEqual(
        #{
            user_data => null,
            topics => [<<"oneone">>],
            rack_id => null,
            owned_partitions => [],
            generation_id => -1
        },
        kafcod_consumer_protocol:decode_metadata(
            <<0, 3, 0, 0, 0, 1, 0, 6, 111, 110, 101, 111, 110, 101, 255, 255, 255, 255, 0, 0, 0, 0,
                255, 255, 255, 255, 255, 255>>
        )
    ).
