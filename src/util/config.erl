%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%
%%% @end
%%% Created : 03. 12月 2024 15:59
%%%-------------------------------------------------------------------
-module(config).

-include("common.hrl").
-include("ets_defines.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([
    init_env/0,
    reload_env/0
]).

% @doc 初始化环境变量
init_env() ->
    {ok, [[Path]]} = init:get_argument(config),
    {ok, #file_info{mtime = MTime}} = file:read_file_info(Path ++ ".config"),
    AllEnvFromConfig = [{mtime, MTime} | application:get_all_env(?APP_NAME)],
    ets:insert(?ETS_ENV_CONFIG, AllEnvFromConfig),
    ok.

% @doc 重载化环境变量
reload_env() ->
    {ok, [[Path]]} = init:get_argument(config),
    File = Path ++ ".config",
    {ok, #file_info{mtime = MTime}} = file:read_file_info(File),
    case ets:lookup(?ETS_ENV_CONFIG, mtime) of
        [{_, T}] when T =/= MTime ->
            {ok, [AllApplicationEnv]} = file:consult(File),
            EnvList = util:prop_get_value(?APP_NAME, AllApplicationEnv, []),
            [application:set_env(?APP_NAME, Key, Value) || {Key, Value} <- EnvList],
            AllEnvFromConfig = [{mtime, MTime} | EnvList],
            ets:insert(?ETS_ENV_CONFIG, AllEnvFromConfig),
            ok;
        _ ->
            skip
    end.