%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%
%%% @end
%%% Created : 03. 12月 2024 15:38
%%%-------------------------------------------------------------------
-module(node_logic).

-include("common.hrl").
-include("ets_defines.hrl").
-include("register.hrl").

%% API
-export([
    start/1,
    stop/0
]).

-export([
    init_ets/0,
    logic_ready/0
]).

start([_NodeId]) ->
    ?START_WORKER(?TOP_SUP, svr_register, []),
    ?START_WORKER(?TOP_SUP, svr_node_basic, []),
    ?START_SUP(?TOP_SUP, sup_mfa, []),
    svr_node_basic:logic_ready(),
    ok.

stop() ->
    ok.

init_ets() ->
    ets:new(?ETS_ENV_CONFIG, ?ETS_DEFAULT_OPTIONS),
    ets:new(?ETS_PROCESS_PID_LOCAL, ?ETS_KEYPOS_OPTIONS(#reg_info.name)),
    ets:new(?ETS_PROCESS_PID_REMOTE, ?ETS_KEYPOS_OPTIONS(#reg_info.name)),
    ok.

logic_ready() ->
    config:init_env(),
    ok.