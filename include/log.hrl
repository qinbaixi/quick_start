%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%
%%% @end
%%% Created : 03. 12月 2024 14:22
%%%-------------------------------------------------------------------
-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

-include("lager.hrl").

-define(ERROR(Msg), lager:error(Msg)).
-define(ERROR(Msg, Args), lager:error(Msg, Args)).

-define(WARN(Msg), lager:warning(Msg)).
-define(WARN(Msg, Args), lager:warning(Msg, Args)).

-define(INFO(Msg),  lager:info(Msg)).
-define(INFO(Msg, Args), lager:info(Msg, Args)).

-define(DEBUG(Msg), lager:debug(Msg)).
-define(DEBUG(Msg, Args), lager:debug(Msg, Args)).


-define(CRITICAL(Msg),  lager:critical(Msg)).
-define(CRITICAL(Msg, Args), lager:critical(Msg, Args)).

-endif.
