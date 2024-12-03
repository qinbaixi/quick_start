%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%
%%% @end
%%% Created : 03. 12月 2024 14:18
%%%-------------------------------------------------------------------
-ifndef(__CODE_DEF_GENERIC_HRL__).
-define(__CODE_DEF_GENERIC_HRL__, true).

-define(iif(A, B, C), (case A of true -> B; false -> C end)).

-define(ASSERT(Check), case Check of true -> true; false -> erlang:throw({error, false}) end).
-define(ASSERT(Check, RetCode), case Check of true -> true; false -> erlang:throw({error, RetCode}) end).
-define(THROW(RetCode), erlang:throw({error, RetCode})).
-define(THROW_IGNORE, erlang:throw(ignore)).
-define(THROW_SKIP, erlang:throw(skip)).
-define(THROW_SKIP(Check), case Check of true -> true; false -> erlang:throw(skip) end).
%% BeforeThrow : 在erlang:throw(RetCode)之前执行的句子
-define(ASSERT(Check, RetCode, BeforeThrow), case Check of true -> true; false -> BeforeThrow, erlang:throw({error, RetCode}) end).

%% 布尔值 =================================================================
-define(TRUE, 1).
-define(FALSE, 0).

%% 数值范围 ===============================================================
-define(UINT8_MAX, 255).
-define(UINT16_MAX, 65535).
-define(UINT24_MAX, 16777215).
-define(INT32_MAX, 2147483647).
-define(UINT32_MAX, 4294967295).
-define(UINT64_MAX, 18446744073709551615).

%% 控制台打印 =============================================================
-define(IO_FMT(FMT), ?IO_FMT(FMT, [])).
-define(IO_FMT(FMT, Args),
    io:format(string:concat("~w:~w:~w ~n~n", string:concat(FMT, "~n~n")), [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

-define(D(Val), io:format("~s: ~w~n", [??Val, Val])).

%% gen_server 辅助 =======================================================
-define(CATCH_HANDLE_CALL(Request, From, State),
    try
        do_handle_call(Request, From, State)
    catch
        _:Reason:Stacktrace ->
%%            ?ERROR("do_handle_call Ex:~p Req:~p, from:~p, stacktrace ~s", [Reason, Request, From, ?STACK(Stacktrace)]),
            {reply, {error, 255}, State}
    end).

-define(CATCH_HANDLE_CAST(Request, State),
    try
        do_handle_cast(Request, State)
    catch
        _:Reason:Stacktrace ->
%%            ?ERROR("do_handle_cast Ex:~p req ~p, stacktrace ~s", [Reason, Request, ?STACK(Stacktrace)]),
            {noreply, State}
    end).

-define(CATCH_HANDLE_INFO(Request, State),
    try
        do_handle_info(Request, State)
    catch
        _:Reason:Stacktrace ->
%%            ?ERROR("do_handle_info Ex:~p req ~p, stacktrace ~s", [Reason, Request, ?STACK(Stacktrace)]),
            {noreply, State}
    end).

-define(CATCH_HANDLE_EVENT(EVENT, State),
    try
        do_handle_event(EVENT, State)
    catch
        _:Reason:Stacktrace ->
%%            ?ERROR("do_handle_event Ex:~p req ~p, stacktrace ~s", [Reason, EVENT, ?STACK(Stacktrace)]),
            {ok, State}
    end).

-define(CATCH_HANDLE_EVENT_CALL(Request, State),
    try
        do_handle_event_call(Request, State)
    catch
        _:Reason:Stacktrace ->
%%            ?ERROR("do_handle_event_call Ex:~p req ~p, stacktrace ~s", [Reason, Request, ?STACK(Stacktrace)]),
            {ok, {error, 255}, State}
    end).

-define(CATCH_HANDLE_EVENT_INFO(Request, State),
    try
        do_handle_event_info(Request, State)
    catch
        _:Reason:Stacktrace ->
%%            ?ERROR("do_handle_event_info Ex:~p req ~p, stacktrace ~s", [Reason, Request, ?STACK(Stacktrace)]),
            {ok, State}
    end).

-define(cast(Msg),
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg);
        _ ->
            ignore
    end).

-define(cast(ID, Msg),
    case p(ID) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg);
        _ ->
            ignore
    end).

-define(call(Msg),
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg);
        _ ->
            {error, 254}
    end).

-define(call(ID, Msg),
    case p(ID) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg);
        _ ->
            {error, 254}
    end).

-define(call_wait(Msg, Timeout),
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg, Timeout);
        _ ->
            {error, 254}
    end).

-define(call_wait(ID, Msg, Timeout),
    case p(ID) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg, Timeout);
        _ ->
            {error, 254}
    end).

-define(info(Msg),
    case p() of
        Pid when is_pid(Pid) ->
            Pid ! Msg;
        _ ->
            ignore
    end).

-define(info(ID, Msg),
    case p(ID) of
        Pid when is_pid(Pid) ->
            Pid ! Msg;
        _ ->
            ignore
    end).


%% try catch 辅助 =============================================================
-define(TRY_CATCH(Expression),
    fun() ->
        try
            Expression
        catch
            _:ErrReason:Stacktrace ->
                ?ERROR("Catch exception: Reason:~p, Stacktrace:~s", [ErrReason, ?STACK(Stacktrace)])
        end
    end()).

-define(TRY_CATCH(Expression, Tip),
    fun() ->
        try
            Expression
        catch
            _:ErrReason:Stacktrace ->
                ?ERROR("~s, Catch exception: Reason:~p, Stacktrace:~s", [Tip, ErrReason, ?STACK(Stacktrace)])
        end
    end()).


%% start child 辅助 ===========================================================
-define(START_SUP(Sup, Mod, Args),
    begin
        {ok, _} = supervisor:start_child(Sup,
            {Mod,
                {Mod, start_link, Args},
                transient, infinity, supervisor, [Mod]
            }
        ),
        ?INFO("supervisor ~w started ~n", [Mod]),
        ok
    end
).

-define(START_WORKER(Sup, Mod, Args),
    begin
        {ok, _} = supervisor:start_child(Sup,
            {Mod,
                {Mod, start_link, Args},
                permanent, infinity, worker, [Mod]
            }
        ),
        ?INFO("worker ~w started ~n", [Mod]),
        ok
    end
).

-endif.
