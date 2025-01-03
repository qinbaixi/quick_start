%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2021
%%% @doc
%%% @end
%%% Created : 26. 8月 2021 10:22
%%%-------------------------------------------------------------------
-module(qmake).

-include_lib("kernel/include/file.hrl").

%% API
-export([
    compile/0,
    compile/1
]).

%% callback
-export([worker/1]).

%% 三元表达式
-define(iif(A, B, C), (case A of true -> B; false -> C end)).

-define(DETS_FILE, "./hot_code.dets").
-define(MODULE_PATH, "./src/tools/qmake.erl").
-define(APP_FILE_PATH, "./src/YourName.app.src").
-define(APP_NAME, "./ebin/YourName.app").
-define(TABLE, file_ver).
-define(DEFAULT_SLOTS, 2048 * 2 + 1024). %set默认插槽大小,根据实际项目大小调整
-define(MAX_SLOTS, 16384). %最大插槽
-define(SAVE_TIME, 30 * 60 * 1000). %未操作存到磁盘时间
-define(DEFAULT_PROCESS, 8). %默认进程
-define(PROCESS_TASK_NUM, 20). %全编译时子进程每次编译大小

-define(EMAKEFILE, "./Emakefile").
-define(PATTERN_HRL, "\"(\\S+.hrl)\"").
-define(PATTERN_BEHAVIOR, "-callback").
-define(PATTERN_TRANS, "parse_transform[(].*,.*[)].*->").
-define(MP(Pattern), element(2, re:compile(Pattern))).
-define(MD5(Bin), erlang:md5(Bin)).

%% dets参数
-define(OPEN_ARGS, [{file, ?DETS_FILE}, {auto_save, ?SAVE_TIME}, {min_no_slots, ?DEFAULT_SLOTS},
    {max_no_slots, ?MAX_SLOTS}, {keypos, 1}, {repair, true}, {type, set}
]).

-define(OUTPUT_DIR, "ebin").
-define(EMAKEFILE_ARGS, begin {ok, [{_, ArgsList}]} = file:consult(?EMAKEFILE), ArgsList end).

-type set() :: tuple().

%% @doc 编译方法
compile() ->
    compile(?DEFAULT_PROCESS).


-spec compile(MaxProcess) -> success | error when
    MaxProcess :: non_neg_integer().

compile(MaxProcess) ->
    try
        {ok, Bin} = file:read_file(?EMAKEFILE),
        EmakefileVer = ?MD5(Bin),
        open(),
        case dets:is_dets_file(?DETS_FILE) of
            true ->
                case dets:lookup(?TABLE, ?EMAKEFILE) of
                    [{_, EmakefileVer}] -> hot_update(MaxProcess);
                    _ -> compile_all(MaxProcess)
                end;
            _ ->
                compile_all(MaxProcess)
        end,
        insert_list([{?EMAKEFILE, EmakefileVer}]),
        close(),
        success
    catch
        _:_ ->
            close(),
            error
    end.


%% @doc 热更并更新版本库
-spec hot_update(MaxProcess) -> ok | {error, Reason} when
    MaxProcess :: non_neg_integer(),
    Reason :: term().

hot_update(MaxProcess) ->
    Files = get_src_files(),
    HrlFiles = get_include_files(),
    RelativePaths = get_include_relative_paths(),
    {Map, ChangeSet} = make_hrl_map(HrlFiles, #{}, sets:new(), RelativePaths),
    HrlChangeList = get_all_change_hrl(Map, ChangeSet),
    statistics(wall_clock),
    {NeedCompileFiles, DetsUpdateValues} = filter_for_hot_update(Files, HrlChangeList),
    {Trans, Behaviors, RestFiles} = filter_behaviors(NeedCompileFiles, [], [], []),
    {_, Time1} = statistics(wall_clock),

    length(Trans) > 0 andalso
        begin
            IsTransSuccess = compile_files(Trans, [{i, "include"}, {outdir, "ebin"}]),
            IsTransSuccess =:= error andalso erlang:throw({error, "compile trans fail"})
        end,
    length(Behaviors) > 0 andalso
        begin
            IsBehaviorsSuccess = compile_files(Behaviors),
            IsBehaviorsSuccess =:= error andalso erlang:throw({error, "compile bahavior fail"})
        end,
    DivideFiles = divide_list(RestFiles, ?PROCESS_TASK_NUM),

    IDList = init_worker(MaxProcess, self(), []),
    [Pid ! start || Pid <- IDList],
    IsSuccessful = manager(DivideFiles, IDList),
    case IsSuccessful of
        success ->
            InsertHrl = get_files_curr_ver(sets:to_list(ChangeSet), []),
            insert_list(DetsUpdateValues ++ InsertHrl),
            {_, Time2} = statistics(wall_clock),

            io:format("filter files use: ~w seconds ~n", [Time1 / 1000]),
            HrlL = length(HrlChangeList),
            HrlL > 0 andalso io:format("~w header files have been changed ~n", [HrlL]),
            io:format("hot update ~w modules in: ~w seconds~n", [length(NeedCompileFiles), Time2 / 1000]),
            ok;
        fail ->
            throw({error, "Hot compile fail"})
    end.


%% @doc 编译所有并存版本库
-spec compile_all(MaxProcess) -> ok|{error, Reason} when
    MaxProcess :: non_neg_integer(),
    Reason :: term().

compile_all(MaxProcess) ->
    FileList = get_src_files(),
    {Trans, Behaviors, RestFiles} = filter_behaviors(FileList, [], [], []),
    statistics(wall_clock),
    compile_first(),
    io:format("Trans ~p ~n", [Trans]),
    IsTransSuccess = compile_files(Trans, [{i, "include"}, {outdir, "ebin"}]),
    IsTransSuccess =:= error andalso erlang:throw({error, "compile trans fail"}),
    IsBehaviorsSuccess = compile_files(Behaviors),
    IsBehaviorsSuccess =:= error andalso erlang:throw({error, "compile bahavior fail"}),
    DiviedFles = divide_list(RestFiles, ?PROCESS_TASK_NUM),
    IDList = init_worker(MaxProcess, self(), []),
    [Pid ! start || Pid <- IDList],
    IsSuccessful = manager(DiviedFles, IDList),
    case IsSuccessful of
        success ->
            {TotalTime, _Time} = statistics(wall_clock),
            HrlFiles = get_include_files(),
            InsertFiles = get_files_curr_ver(FileList ++ HrlFiles, []),
            insert_list(InsertFiles),
%%            copy_file(?APP_FILE_PATH, ?APP_NAME),
            Sum = length(FileList),
            io:format("compiled ~p modules with ~p processes in ~p s~n", [Sum, MaxProcess, TotalTime / 1000]),
            io:format("compiled ~p behavior modules ~n", [length(Behaviors)]),
            io:format("compiled sum is ~w ~n", [Sum]),
            ok;
        fail ->
            throw({error, "compile fail~n"})
    end.


%% @doc 分发进程
-spec manager(FileList, IDList) -> success | fail when
    FileList :: [string()],
    IDList :: [pid()].

manager([], IDList) ->
    [Pid ! done || Pid <- IDList],
    manager(wait, length(IDList));
manager(wait, Len) ->
    receive
        {_From, done} ->
            NLen = Len - 1,
            case NLen > 0 of
                true -> manager(wait, NLen);
                _ -> success
            end;
        compile_error -> %等待可能也会收到
            fail;
        _Msg ->
            manager(wait, Len)
    end;
manager([HeadFile | RestFiles], IDList) ->
    receive
        {From, get_one} when is_pid(From) ->
            From ! {do, HeadFile},
            manager(RestFiles, IDList);
        compile_error ->
            [Pid ! done || Pid <- IDList], %提前结束子进程
            fail;
        Msg ->
            io:format("manger process receive unknown Msg ~p~n", [Msg]),
            manager([HeadFile | RestFiles], IDList)
    end.


%% @doc 编译进程
-spec worker(ManagerPid) -> ok when
    ManagerPid :: pid().
worker(ManagerPid) ->
    receive
        {do, Files} ->
            case compile_files(Files) of
                error ->
                    ManagerPid ! compile_error;
                _ ->
                    ManagerPid ! {self(), get_one},
                    worker(ManagerPid)
            end;
        done ->
            ManagerPid ! {self(), done},
            ok;
        start ->
            ManagerPid ! {self(), get_one},
            worker(ManagerPid);
        Msg ->
            io:format("worker receive unknown Msg ~w", [Msg]),
            worker(ManagerPid)
    end.


%% @doc 分裂N个Worker进程
-spec init_worker(Num, ManagerPid, Acc) -> IDList when
    Num :: non_neg_integer(),
    ManagerPid :: pid(),
    Acc :: pid(),
    IDList :: [pid()].
init_worker(-1, _, Acc) ->
    Acc;
init_worker(Num, ManagerPid, Acc) ->
%%    Pid = spawn_opt(?MODULE, worker, [ManagerPid], [{priority, high}, {min_heap_size, 4 * 1024 * 1024}, {min_bin_vheap_size,  4 *1024 * 1024}]),
    Pid = spawn(?MODULE, worker, [ManagerPid]),
    init_worker(Num - 1, ManagerPid, [Pid | Acc]).

%%=============================================================================================================
%% util
%%=============================================================================================================
%% @doc 批量插入dets
insert_list(List) ->
    dets:insert(?TABLE, List).

%% @doc 将列表拆分为长度为N的列表
divide_list(List, N) ->
    lists:reverse(divide_list_1(List, N, [])).

divide_list_1(List, N, Acc) when length(List) > N ->
    {HeadList, TailList} = lists:split(N, List),
    divide_list_1(TailList, N, [HeadList | Acc]);
divide_list_1(List, _N, Acc) ->
    [List | Acc].


%% @doc 批量获取文件md5数据
get_files_curr_ver([], Acc) ->
    Acc;
get_files_curr_ver([H | T], Acc) ->
    {ok, Bin} = file:read_file(H),
    CurrVer = ?MD5(Bin),
    get_files_curr_ver(T, [{H, CurrVer} | Acc]).


%% @doc 文件包含头文件是否改变
-spec is_hrl_changed(Bin, ChangeHrlList, RelativePaths) -> true | false when
    Bin :: iodata(),
    ChangeHrlList :: [string()],
    RelativePaths :: [string()].

is_hrl_changed(Bin, ChangeHrlList, RelativePaths) ->
    case re:run(Bin, ?MP(?PATTERN_HRL), [global, {capture, all, list}]) of
        {match, List} ->
            HrlList = format_hrl_list(List, [], RelativePaths),
            is_hrl_changed_1(HrlList, ChangeHrlList);
        _ ->
            false
    end.

is_hrl_changed_1([], _) ->
    false;
is_hrl_changed_1([H | T], ChangeHrlList) ->
    ?iif(lists:member(H, ChangeHrlList), true, is_hrl_changed_1(T, ChangeHrlList)).

get_include_relative_paths() ->
    Fun = fun
              ({'i', Path}, Acc) ->
                  RelativePath = "./" ++ Path ++ "/",
                  [RelativePath | Acc];
              (_, Acc) -> Acc
          end,
    lists:foldl(Fun, [], ?EMAKEFILE_ARGS).

%% @doc 拼装hrl文件路径
format_hrl_list([], Acc, _) ->
    Acc;
format_hrl_list([[_, Need] | T], Acc, RelativePaths) ->
    NAcc = [RP ++ Need || RP <- RelativePaths] ++ Acc,
    format_hrl_list(T, NAcc, RelativePaths).

%% @doc 获取包含头文件列表
-spec get_include_hrl_files(Bin, RelativePaths) -> HrlList when
    Bin :: iodata(),
    HrlList :: [] | [string()],
    RelativePaths :: [string()].

get_include_hrl_files(Bin, RelativePaths) ->
    case re:run(Bin, ?MP(?PATTERN_HRL), [global, {capture, all, list}]) of
        {match, List} ->
            lists:reverse(format_hrl_list(List, [], RelativePaths));
        _ -> []
    end.


%% @doc 返回列表，erl包含这个列表的任意头文件则需要重新编译
-spec get_all_change_hrl(CitiedMap, FileChangeSet) -> ChangeList when
    CitiedMap :: map(),
    FileChangeSet :: set(),
    ChangeList :: [] | [string()].

get_all_change_hrl(CitiedMap, FileChangeSet) ->
    case sets:size(FileChangeSet) > 0 of
        false ->
            [];
        _ ->
            Set = get_all_change_hrl_1(CitiedMap, FileChangeSet, FileChangeSet),
            sets:to_list(Set)
    end.

get_all_change_hrl_1(CitiedMap, ChangeSet, AccSet) ->
    CitiedSet = sets:fold(
        fun(Key, Acc) ->
            Set = maps:get(Key, CitiedMap, sets:new()),
            sets:union(Acc, Set) end,
        sets:new(),
        ChangeSet
    ),
    NextCheckSet = sets:subtract(CitiedSet, AccSet), %差集
    case sets:size(NextCheckSet) =:= 0 of
        true -> AccSet;
        _ -> get_all_change_hrl_1(CitiedMap, NextCheckSet, sets:union(CitiedSet, AccSet))
    end.

%% @doc 制作引用库
-spec make_hrl_map(HrlFileList, Map, FileChangeSet, RelativePaths) -> {CitiedMap, FileChangeSet} when
    HrlFileList :: [string()],
    Map :: map(),
    CitiedMap :: map(),
    FileChangeSet::set(),
    RelativePaths :: [string()].

make_hrl_map([], CitiedMap, FileChangeSet, _) ->
    {CitiedMap, FileChangeSet};
make_hrl_map([H | T], CitiedMap, FileChangeSet, RelativePaths) ->
    {ok, Bin} = file:read_file(H),
    CurrVer = ?MD5(Bin),
    L = get_include_hrl_files(Bin, RelativePaths),
    NMap = add_to_citied_set(L, H, CitiedMap),
    NFileChangeSet = case dets:lookup(?TABLE, H) of
                         [{H, V}] when V =:= CurrVer -> FileChangeSet;
                         _ -> sets:add_element(H, FileChangeSet)
                     end,
    make_hrl_map(T, NMap, NFileChangeSet, RelativePaths).

add_to_citied_set([], _, CitiedMap) ->
    CitiedMap;
add_to_citied_set([H | T], CurrFile, CitiedMap) ->
    Set = maps:get(H, CitiedMap, sets:new()),
    add_to_citied_set(T, CurrFile, CitiedMap#{H => sets:add_element(CurrFile, Set)}).

%%%% @doc 复制文件
%%copy_file(From, To) ->
%%    case filelib:is_file(From) of
%%        true -> file:copy(From, To, infinity);
%%        _ -> io:format("Copy error ~n")
%%    end.

%% @doc 获取src下所有文件
get_src_files() ->
    filelib:fold_files("./src/", ".*.erl$", true, fun(F, AccIn) -> [F | AccIn] end, []).

%% @doc 获取所有头文件
get_include_files() ->
    lists:foldl(fun
                    ({'i', Path}, Acc) ->
                        get_include_files_(Path) ++ Acc;
                    (_, Acc) ->
                        Acc end,
        [],
        ?EMAKEFILE_ARGS).

get_include_files_(Path) ->
    RelativePath = "./" ++ Path ++ "/",
    filelib:fold_files(RelativePath, ".*.hrl$", true, fun(F, AccIn) -> [F | AccIn] end, []).


%% 过滤非更新文件， 返回待更新文件列表，以及待更新dets元组列表
-spec filter_for_hot_update(List, ChangeHrlList) ->   {NeedCompileFiles, DetsUpdateErlFiles} when
    List :: [string()],
    ChangeHrlList :: [string()],
    NeedCompileFiles :: [string()],
    DetsUpdateErlFiles :: [] | [{string(), Digest}],
    Digest :: binary().

filter_for_hot_update(List, ChangeHrlList) when is_list(List) ->
    EMakeArgs = ?EMAKEFILE_ARGS,
    {_, OutDir} = lists:keyfind(outdir, 1, EMakeArgs),
    RelativePaths = get_include_relative_paths(),
    filter_for_hot_update_1(List, ChangeHrlList, [], [], OutDir, RelativePaths).

filter_for_hot_update_1([], _, NeedCompileFiles, DetsUpdateErlFiles, _OutDir, _RelativePaths) ->
    {NeedCompileFiles, DetsUpdateErlFiles};
filter_for_hot_update_1([H | T], ChangeHrlList, NeedCompileFiles, DetsUpdateErlFiles, OutDir, RelativePaths) ->
    {ok, Bin} = file:read_file(H),
    NowVer = ?MD5(Bin),
    case dets:lookup(?TABLE, H) of
        [] -> %新文件
            filter_for_hot_update_1(T, ChangeHrlList, [H | NeedCompileFiles], [{H, NowVer} | DetsUpdateErlFiles], OutDir, RelativePaths);
        [{H, V}] ->
            if
                NowVer =/= V -> %已修改文件
                    filter_for_hot_update_1(T, ChangeHrlList, [H | NeedCompileFiles], [{H, NowVer} | DetsUpdateErlFiles], OutDir, RelativePaths);
                H =/= ?MODULE_PATH -> %检查头文件,过滤自己是因为正则会导致额外的匹配
                    BaseName = filename:basename(H, ".erl"),
                    BeamPath = filename:join([OutDir, BaseName]) ++ ".beam",
                    case filelib:is_file(BeamPath) of
                        false ->
                            filter_for_hot_update_1(T, ChangeHrlList, [H | NeedCompileFiles], [{H, NowVer} | DetsUpdateErlFiles], OutDir, RelativePaths);
                        _ ->
                            case is_hrl_changed(Bin, ChangeHrlList, RelativePaths) of
                                false -> filter_for_hot_update_1(T, ChangeHrlList, NeedCompileFiles, DetsUpdateErlFiles, OutDir, RelativePaths);
                                true -> filter_for_hot_update_1(T, ChangeHrlList, [H | NeedCompileFiles], DetsUpdateErlFiles, OutDir, RelativePaths)
                            end
                    end;
                true ->
                    filter_for_hot_update_1(T, ChangeHrlList, NeedCompileFiles, DetsUpdateErlFiles, OutDir, RelativePaths)
            end
    end.


%% @doc 批量编译
compile_files(List) ->
    compile_files(List, ?EMAKEFILE_ARGS).
compile_files(List, Opts) ->
    {_, OutDir} = lists:keyfind(outdir, 1, Opts),
    lists:foreach(
        fun(Name) ->
            BaseName = filename:basename(Name, ".erl"),
            BeamPath = filename:join([OutDir, BaseName]) ++ ".beam",
            Flag =
                case file:delete(BeamPath) of
                    {error, Reason} -> Reason;
                    Other -> Other
                end,
            io:format("Delete: ~s ~p~n", [BeamPath, Flag])
        end, List
    ),
    make:files(List, Opts).

%% @doc 过滤出行为树文件
filter_behaviors([], Trans, Behaviors, RestFiles) ->
    {Trans, Behaviors, RestFiles};
filter_behaviors([?MODULE_PATH | T], Trans, Behaviors, RestFiles) ->
    filter_behaviors(T, Trans, Behaviors, [?MODULE_PATH | RestFiles]);
filter_behaviors([H | T], Trans, Behaviors, RestFiles) ->
    {ok, Bin} = file:read_file(H),
    case re:run(Bin, ?MP(?PATTERN_BEHAVIOR), [{capture, none}]) of
        match ->
            filter_behaviors(T, Trans, [H | Behaviors], RestFiles);
        _ ->
            case re:run(Bin, ?MP(?PATTERN_TRANS), [{capture, none}]) of
                match ->
                    filter_behaviors(T, [H | Trans], Behaviors, RestFiles);
                _ ->
                    filter_behaviors(T, Trans, Behaviors, [H | RestFiles])
            end
    end.

%% @doc 打开dets表，不存在则初始化表
open() ->
    case dets:open_file(?TABLE, ?OPEN_ARGS) of
        {ok, ?TABLE} -> ok;
        {error, Reason} -> io:format("cannot open dets table ~p ~p ~n", [?TABLE, Reason])
    end.

%% @doc 使用后关闭表
close() ->
    dets:close(?TABLE).

%% @doc 全编译先编译
%%  根据参数选出最高优先级进行编译 已解决
%% todo trans 依赖 trans
compile_first() ->
    CompileFirstL = [
        "./src/tools/lager/lager_util.erl"],
    Opts = [{i, "include"}, {outdir, "ebin"}],
    compile_files(CompileFirstL, Opts),
    code:load_file(lager_util),
    ok.

