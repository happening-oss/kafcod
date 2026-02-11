-module(list_groups_request_tests).
-include_lib("eunit/include/eunit.hrl").
-include("catch.hrl").

null_client_id_test() ->
    % I found this one in the Kafire source code; see kafire/test/terms_files/kafire_packet_tests/list_groups_request.terms
    % The Kafire test has the size prefix on it. We don't.
    ?assertEqual(
        <<0, 16, 0, 0, 0, 0, 0, 1, 255, 255>>,
        iolist_to_binary(
            list_groups_request:encode_list_groups_request_0(#{
                correlation_id => 1, client_id => null
            })
        )
    ).

invalid_filter_test() ->
    % Manual testing revealed a bug in the error reporting. The filters are arrays of strings, not strings.
    {error, Reason = badarg, StackTrace} = ?CATCH(
        list_groups_request:encode_list_groups_request_5(#{
            correlation_id => 1, client_id => null, states_filter => <<>>, types_filter => <<>>
        })
    ),
    ?assertEqual(
        #{
            1 =>
                "expected 'states_filter' to be an array of 'string', but has type 'string', value <<>>\n"
                "expected 'types_filter' to be an array of 'string', but has type 'string', value <<>>"
        },
        kafcod_errors:format_error(Reason, StackTrace)
    ),
    ok.
