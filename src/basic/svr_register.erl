%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%     自定义进程注册
%%% @end
%%% Created : 23. 12月 2024 22:56
%%%-------------------------------------------------------------------
-module(svr_register).
-behaviour(gen_server).

-include("common.hrl").
-include("ets_defines.hrl").
-include("register.hrl").

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    do_handle_call/3,
    do_handle_cast/2,
    do_handle_info/2
]).

%% API
-export([
    start_link/0,
    summary_info/0,
    register_node/1,
    unregister_node/1
]).

-export([
    register/2,
    register_name/2,
    unregister_name/1,
    whereis_name/1,
    send/2
]).

-define(SERVER, ?MODULE).
-define(FULL_SYNC_INTERVAL, 600000).

-record(state, {counter = 0}).

%% %%%===================================================================
%% %%% API
%% %%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

summary_info() ->
    gen_server:call(?SERVER, summary_info).

%% @doc 注册节点
-spec register_node(Node) -> ok when
    Node :: node().
register_node(Node) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Node}).

%% @doc 节点解除注册
-spec unregister_node(Node) -> ok when
    Node :: node().
unregister_node(Node) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Node}).

%% @doc 注册(新注册覆盖旧注册)
-spec register(Name, Pid) -> yes | no when
    Name :: term(),
    Pid :: pid().
register(Name, Pid) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Pid}).

%% @doc 注册(如已注册，则不操作)
-spec register_name(Name, Pid) -> yes | no when
    Name :: term(),
    Pid :: pid().
register_name(Name, Pid) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Pid}).

%% @doc 解除注册
-spec unregister_name({Module, Id}) -> ok when
    Module :: atom(),
    Id :: integer().
unregister_name(Name) ->
    case where(Name) of
        undefined -> ok;
        _ -> gen_server:cast(?MODULE, {?FUNCTION_NAME, Name})
    end.

%% @doc 查询Name对应的进程pid
-spec whereis_name(Name) -> Pid | undefined when
    Name :: atom() | tuple(),
    Pid :: pid().
whereis_name(Name) ->
    case ets:lookup(?ETS_PROCESS_PID_LOCAL, Name) of
        [#reg_info{pid = Pid}] -> Pid;
        _ ->
            case ets:lookup(?ETS_PROCESS_PID_REMOTE, Name) of
                [#reg_info{pid = Pid}] -> Pid;
                _ ->
                    sync({'look_for', Name, self()}),
                    undefined
            end
    end.

%% @doc 向对应进程发消息
-spec send(Name, Msg) -> Pid when
    Name :: term(),
    Msg :: term(),
    Pid :: pid().
send(Name, Msg) ->
    case whereis_name(Name) of
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid;
        undefined ->
            exit({badarg, {Name, Msg}})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% 进程名管理
    start_timer(),
    put_reg_nodes([]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?CATCH_HANDLE_CALL(Request, From, State).

handle_cast(Request, State) ->
    ?CATCH_HANDLE_CAST(Request, State).

handle_info(Info, State) ->
    ?CATCH_HANDLE_INFO(Info, State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 注册并同步单条数据
do_handle_call({'register_name', Name, Pid}, _From, State) ->
    Reply =
        case ets:lookup(?ETS_PROCESS_PID_LOCAL, Name) of
            [#reg_info{pid = LPid}] ->
                case is_process_alive(LPid) of
                    true -> no;
                    _ -> yes
                end;
            _ ->
                case ets:lookup(?ETS_PROCESS_PID_REMOTE, Name) of
                    [#reg_info{}] -> no;
                    _ -> yes
                end
        end,
    case Reply of
        yes ->
            Data = #reg_info{name = Name, pid = Pid, node = node()},
            local_reg(Data);
        _ ->
            skip
    end,
    {reply, Reply, State};

do_handle_call({'register', Name, Pid}, _From, State) ->
    Reply = case ets:lookup(?ETS_PROCESS_PID_REMOTE, Name) of
                [#reg_info{}] -> no;
                _ -> yes
            end,
    case Reply of
        yes ->
            Data = #reg_info{name = Name, pid = Pid, node = node()},
            local_reg(Data);
        _ ->
            throw({remote_registered, Name})
    end,
    {reply, Reply, State};


do_handle_call(summary_info, _From, State) ->
    SummaryInfo = [
        {local_num, ets:info(?ETS_PROCESS_PID_LOCAL, size)},
        {remote_num, ets:info(?ETS_PROCESS_PID_REMOTE, size)},
        {reg_nodes, get_reg_nodes()}
    ],
    {reply, SummaryInfo, State};

do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% 注册节点
do_handle_cast({'register_node', Node}, State) when Node =/= node() ->
    Nodes = get_reg_nodes(),
    case lists:member(Node, Nodes) of
        true ->
            skip;
        _ ->
            put_reg_nodes([Node | Nodes]),
            case ets:tab2list(?ETS_PROCESS_PID_LOCAL) of
                [] -> skip;
                SyncData -> sync_single_node(Node, {'batch_sync', SyncData, node()})
            end
    end,
    {noreply, State};
do_handle_cast({'register_node', _Node}, State) ->
    {noreply, State};

%% 解除注册节点
do_handle_cast({'unregister_node', Node}, State) ->
    Nodes = get_reg_nodes(),
    put_reg_nodes(lists:delete(Node, Nodes)),
    sync_single_node(Node, {'unregister_node_remote', node()}),
    {noreply, State};

%% 清除远程节点的本地节点数据
do_handle_cast({'unregister_node_remote', Node}, State) ->
    NeedDelete =
        ets:foldl(
            fun
                (#reg_info{node = FromNode} = Map, Acc) when FromNode =:= Node -> [Map | Acc];
                (_, Acc) -> Acc
            end, [], ?ETS_PROCESS_PID_REMOTE),
    [ets:delete_object(?ETS_PROCESS_PID_REMOTE, What) || What <- NeedDelete],
    {noreply, State};


%% 解除注册单条数据
do_handle_cast({'unregister_name', Name}, State) ->
    ets:delete(?ETS_PROCESS_PID_LOCAL, Name),
    sync({sync_unregister, Name}),
    {noreply, State};

%% 远程节点同步写入数据
do_handle_cast({'sync_register', #reg_info{} = Data}, State) ->
    ets:insert(?ETS_PROCESS_PID_REMOTE, Data),
    {noreply, State};

%% 远程节点同步批量写入数据
do_handle_cast({'sync_register', [#reg_info{} | _] = DataList}, State) ->
    lists:foreach(
        fun(Data) ->
            ets:insert(?ETS_PROCESS_PID_REMOTE, Data)
        end, DataList
    ),
    {noreply, State};

do_handle_cast({'sync_unregister', {_Module, _Id} = Name}, State) ->
    ets:delete(?ETS_PROCESS_PID_REMOTE, Name),
    {noreply, State};

do_handle_cast({'sync_unregister', [{_Module, _Id} | _] = NameList}, State) ->
    lists:foreach(
        fun(Name) ->
            ets:delete(?ETS_PROCESS_PID_REMOTE, Name)
        end, NameList
    ),
    {noreply, State};

do_handle_cast({'batch_sync', DataList, Node}, State) ->
    Delete = ets:foldl(
        fun
            (#reg_info{name = Name, node = FromNode}, Acc) when Node =:= FromNode ->
                [Name | Acc];
            (_, Acc) ->
                Acc
        end, [], ?ETS_PROCESS_PID_REMOTE),
    [ets:delete(?ETS_PROCESS_PID_REMOTE, Name) || Name <- Delete],
    [ets:insert(?ETS_PROCESS_PID_REMOTE, Data) || Data <- DataList],
    {noreply, State};

do_handle_cast({'look_for', Name, _From}, State) ->
    case ets:lookup(?ETS_PROCESS_PID_LOCAL, Name) of
        [#reg_info{} = Data] ->
            sync({sync_register, Data});
        _ -> skip
    end,
    {noreply, State};

do_handle_cast(_Request, State) ->
    {noreply, State}.


do_handle_info('loop', #state{counter = Counter} = State) ->
    start_timer(),
    Counter rem 60 =:= 0 andalso clear_invalid_record(),                %% 清除失效数据
    Counter rem 3600 =:= 0 andalso batch_sync(),                        %% 全同步
    {noreply, State#state{counter = Counter + 1}};


do_handle_info({'DOWN', _MRef, process, Pid, _OtherReason}, State) ->
    ets:foldl(
        fun
            (#reg_info{pid = PID, name = Name}, Acc) when Pid =:= PID ->
                gen_server:cast(self(), {unregister_name, Name}),
                Acc;
            (_, Acc) -> Acc
        end, [], ?ETS_PROCESS_PID_LOCAL),
    {noreply, State};

do_handle_info(_Request, State) ->
    {noreply, State}.

start_timer() ->
    erlang:send_after(1000, self(), loop).

batch_sync() ->
    SyncData = ets:tab2list(?ETS_PROCESS_PID_LOCAL),
    SyncData =/= [] andalso sync({'batch_sync', SyncData, node()}).


clear_invalid_record() ->
    InvalidNames =
        ets:foldl(
            fun(#reg_info{name = Name, pid = Pid}, Acc) ->
                case is_process_alive(Pid) of
                    true -> Acc;
                    _ -> [Name | Acc]
                end
            end, [], ?ETS_PROCESS_PID_LOCAL
        ),
    lists:foreach(
        fun(Name) ->
            ets:delete(?ETS_PROCESS_PID_LOCAL, Name)
        end, InvalidNames
    ),
    InvalidNames =/= [] andalso sync({sync_unregister, InvalidNames}).

sync(Cmd) ->
    [
        sync_single_node(Node, Cmd) || Node <- get_reg_nodes()
    ].

sync_single_node(Node, Cmd) ->
    rpc:cast(Node, gen_server, cast, [?MODULE, Cmd]).

get_reg_nodes() ->
    case erlang:get(reg_nodes) of
        undefined -> [];
        Any -> Any
    end.

put_reg_nodes(Nodes) ->
    erlang:put(reg_nodes, Nodes).

where(Name) ->
    case ets:lookup(?ETS_PROCESS_PID_LOCAL, Name) of
        [#reg_info{pid = Pid}] ->
            if node(Pid) == node() ->
                case is_process_alive(Pid) of
                    true -> Pid;
                    false -> undefined
                end;
                true ->
                    Pid
            end;
        [] -> undefined
    end.

local_reg(#reg_info{pid = PId} = Data) ->
    erlang:monitor(process, PId),
    ets:insert(?ETS_PROCESS_PID_LOCAL, Data),
    sync({sync_register, Data}).