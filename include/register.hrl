%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%
%%% @end
%%% Created : 23. 12月 2024 23:09
%%%-------------------------------------------------------------------
-ifndef(__REGISTER_HRL__).
-define(__REGISTER_HRL__, 1).

-record(reg_info, {
    name,
    pid,
    node
}).

-define(VIA_NAME(Name), {via, svr_register, Name}).
-endif.
