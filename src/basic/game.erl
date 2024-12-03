%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%     启动器
%%% @end
%%% Created : 03. 12月 2024 14:40
%%%-------------------------------------------------------------------
-module(game).

-include("log.hrl").

%% Application callbacks
-export([
    start/0,
    stop/0,
    stop/1,
    stop_by_sync/0,
    stop_by_sync/1,
    preload_codes/0
]).

-define(APP, quick_start).
-define(APPS_BRIDGE, [lager, sasl, inets, ssl, ?APP]).
-define(APPS_NORMAL, [lager, sasl, inets, ssl, ?APP]).

%% @doc 启动游戏
start() ->
    preload_codes(),
    io:format("preload_codes done ~n"),
    [NodeIDStr | _] = init:get_plain_arguments(),
    NodeID = list_to_integer(NodeIDStr),
    util_svr:put_node_id(NodeID),
    start_apps(?APPS_NORMAL),
    ok.

start_apps([]) ->
    ok;
start_apps([App | T]) ->
    Ret = application:ensure_all_started(App),
    ?INFO("start_apps:~p, ret=~p", [App, Ret]),
    start_apps(T).

%% @doc 结束游戏(linux异步) TODO delete
stop() ->
    stop_apps(lists:reverse(?APPS_NORMAL) -- [lager]),
    %% spawn(fun() -> stop_apps(lists:reverse(?APPS_NORMAL)) end),
    ?INFO("stopped all application finish, halting..."),
    application:stop(lager),
    timer:sleep(1000),
    erlang:halt(0).
%% stopping.

%% @doc 结束游戏(linux异步)
stop(_NodeID) ->
    spawn(fun() -> stop_apps(lists:reverse(?APPS_NORMAL)) end),
    stopping.

%% @doc 结束游戏(windows同步) TODO delete
stop_by_sync() ->
    stop_apps(lists:reverse(?APPS_NORMAL)),
    stopped.

%% @doc 结束游戏(windows同步)
stop_by_sync(_NodeID) ->
    stop_apps(lists:reverse(?APPS_NORMAL)),
    stopped.

%% @doc 结束App
stop_apps(Apps) ->
    [stop_app(App) || App <- Apps].

stop_app(App) ->
    Ret = application:stop(App),
    ?INFO("stop_app:~p ret:~p", [App, Ret]),
    ok.

%% @doc preload
preload_codes() ->
    Paths = code:get_path(),
    RootDir = code:root_dir(),
    ScanPaths = [Path || Path <- Paths, string:prefix(filename:absname(Path), RootDir) =:= nomatch],
    %% io:format("preload_codes for the following paths:~p~n", [ScanPaths]),
    [preload_dir(Path) || Path <- ScanPaths].

preload_dir(Path) ->
    io:format("preloading code for dir ~s~n", [Path]),
    FileNames = filelib:wildcard("*.beam", Path),
    F = fun(FileName) ->
        code:load_file(list_to_atom(filename:rootname(FileName)))
        end,
    lists:foreach(F, FileNames).
