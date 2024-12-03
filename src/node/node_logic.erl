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
    ?START_WORKER(?TOP_SUP, svr_node_basic, []),
    svr_node_basic:logic_ready(),
    ok.

stop() ->
    ok.

init_ets() ->
    ets:new(?ETS_ENV_CONFIG, ?ETS_DEFAULT_OPTIONS),
    ok.

logic_ready() ->
    config:init_env(),
    ok.