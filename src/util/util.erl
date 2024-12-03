%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%     杂项工具箱
%%% @end
%%% Created : 03. 12月 2024 16:01
%%%-------------------------------------------------------------------
-module(util).

-compile(export_all).

%% API
-export([

]).

%% -----------------------------------------------------------------------------
%% proplists 弱化版，采用lists实现，仅支持键值对列表 [{K, V}]
%% -----------------------------------------------------------------------------

%% @doc proplists:get_value/2
prop_get_value(Key, List) ->
    prop_get_value(Key, List, undefined).

prop_get_value(Key, List, Def) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Val} -> Val;
        _ -> Def
    end.

prop_get_value2(Key, List) ->
    prop_get_value(Key, List, undefined).

prop_get_value2(Key, List, Def) ->
    case lists:keyfind(Key, 1, List) of
        Tuple when is_tuple(Tuple) -> Tuple;
        _ -> Def
    end.

%% @doc proplists:get_keys/1
prop_get_keys(List) ->
    [K || {K, _V} <- List].

prop_get_values(List) ->
    [V || {_K, V} <- List].

%% @doc proplists:delete/2
prop_delete(K, List) ->
    lists:keydelete(K, 1, List).

%% @doc
prop_store({K, V}, List) ->
    prop_store(K, V, List).

prop_store(K, V, List) ->
    lists:keystore(K, 1, List, {K, V}).

%% @doc
prop_increase(K, List) ->
    prop_increase(K, List, 1).

prop_increase(K, List, Incl) ->
    case prop_get_value(K, List, 0) of
        V when is_integer(V) ->
            prop_store(K, V + Incl, List);
        _ ->
            List
    end.

%% @doc
prop_decrease(K, List) ->
    prop_decrease(K, List, 1).

prop_decrease(K, List, Dec) ->
    case prop_get_value(K, List, 0) of
        V when is_integer(V) ->
            prop_store(K, V - Dec, List);
        _ ->
            List
    end.

%% @doc
prop_update_counter(KvList, List) ->
    lists:foldl(fun
                    ({K, V}, Acc) ->
                        util:prop_increase(K, Acc, V);
                    (K, Acc) when is_integer(K) ->
                        util:prop_increase(K, Acc)
                end, List, KvList).

prop_merge(List1, List2) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case lists:keytake(Key, 1, Acc) of
                false -> [{Key, [Value]} | Acc];
                {value, {Key, Values}, L} -> [{Key, [Value | Values]} | L]
            end
        end, List2, List1
    ).
