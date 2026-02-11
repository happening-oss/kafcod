-define(CATCH(Expr),
    (fun() ->
        try Expr of
            _ -> ok
        catch
            Type:Reason:StackTrace ->
                {Type, Reason, StackTrace}
        end
    end)()
).
