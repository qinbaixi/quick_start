%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%     全局数据
%%% @end
%%% Created : 03. 12月 2024 15:20
%%%-------------------------------------------------------------------
-module(util_svr).

-compile(export_all).

-include("persistent_key.hrl").

%% API
-export([]).

put_node_id(NodeId) ->
    persistent_term:put(?pt_node_id, NodeId).

%% @doc 获取当前节点id
get_node_id() ->
    persistent_term:get(?pt_node_id, undefined).
