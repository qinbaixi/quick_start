%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%     ets define
%%% @end
%%% Created : 03. 12月 2024 15:28
%%%-------------------------------------------------------------------

%% ets表选项
-define(ETS_DEFAULT_OPTIONS, [set, public, named_table, {read_concurrency, true}]).
-define(ETS_PROTECTED_OPTIONS, [set, protected, named_table, {read_concurrency, true}]).
-define(ETS_KEYPOS_OPTIONS(KeyPos), [set, public, named_table, {keypos, KeyPos}, {read_concurrency, true}]).
-define(ETS_SET_PROTECTED_KEY(KeyPos), [set, protected, named_table, {keypos, KeyPos}]).
-define(ETS_SET_PROTECTED_KEY_READ(KeyPos), [set, protected, named_table, {keypos, KeyPos}, {read_concurrency, true}]).
-define(ETS_ORDER_SET_KEY_READ(KeyPos), [ordered_set, public, named_table, {keypos, KeyPos}, {read_concurrency, true}]).

-define(ETS_CURRENCY_OPTIONS, [set, public, named_table, {read_concurrency, true}, {write_concurrency, true}]).
-define(ETS_KEY_CURRENCY(KeyPos), [set, public, named_table, {keypos, KeyPos}, {read_concurrency, true}, {write_concurrency, true}]).

-define(ETS_DUP_BAG_OPTIONS, [duplicate_bag, public, named_table, {read_concurrency, true}]).
-define(ETS_DUP_BAG_KEYPOS_OPTIONS(KeyPos), [duplicate_bag, public, named_table, {keypos, KeyPos}, {read_concurrency, true}]).


%% 表名定义
-define(ETS_ENV_CONFIG, ets_env_config).        %% 环境变量

-define(ETS_PROCESS_PID_LOCAL, ets_process_pid_local).    %% 本地节点进程注册
-define(ETS_PROCESS_PID_REMOTE, ets_process_pid_remote).  %% 远端节点进程注册