-define(encoder_error(Args, Expected),
    % We use erlang:error inside the macro, so that the stack trace is correct, but we call create_error_info for the
    % complicated bit.
    erlang:error(badarg, [Args], [kafcod_errors:create_error_info(Args, Expected)])
).
