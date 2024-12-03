%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%     load
%%% @end
%%% Created : 03. 12月 2024 14:16
%%%-------------------------------------------------------------------
-module(svr_node_basic).

-behaviour(gen_server).

-include("log.hrl").
-include("code_def_generic.hrl").

%% API
-export([
    logic_ready/0
]).

%% gen_server callbacks
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 节点就绪
logic_ready() ->
    ?cast(?FUNCTION_NAME).

p() ->
    whereis(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(Msg, From, State) ->
    ?CATCH_HANDLE_CALL(Msg, From, State).

handle_cast(Msg, State) ->
    ?CATCH_HANDLE_CAST(Msg, State).

handle_info(Msg, State) ->
    ?CATCH_HANDLE_CAST(Msg, State).

terminate(Reason, _State) ->
    not lists:member(Reason, [shutdown, normal]) andalso
        ?ERROR("~s ~s unknow reason ~p", [?SERVER, ?FUNCTION_NAME, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle_call(Msg, _From, State) ->
    ?INFO("Recv unexpected call msg: ~p", [Msg]),
    {reply, undefined, State}.

do_handle_cast(logic_ready, State) ->
    node_logic:init_ets(),
    node_logic:logic_ready(),
    todo,
    {noreply, State};
do_handle_cast(Msg, State) ->
    ?INFO("Recv unexpected cast msg: ~p", [Msg]),
    {noreply, State}.