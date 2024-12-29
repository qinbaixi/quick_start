%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%
%%% @end
%%% Created : 25. 12月 2024 22:39
%%%-------------------------------------------------------------------
-module(svr_mfa).


-behaviour(gen_server).

-include("register.hrl").
-include("common.hrl").

%% API
-export([
    sync/4,
    async_mfa/4
]).

%% gen_server callbacks
-export([
    start_link/1,
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
start_link(Name) when is_atom(Name)->
    gen_server:start_link({local, Name}, ?MODULE, [], []);
start_link({global, Name}) when is_atom(Name) ->
    gen_server:start_link({global, Name}, ?MODULE, [], []);
start_link({via, Mod, _} = Name) when is_atom(Mod) ->
    gen_server:start_link(Name, ?MODULE, [], []);
start_link(Name) ->
    ?ERROR("unsupport name ：~p", [Name]),
    skip.

sync(Name, M, F, A) ->
    gen_server:call(Name, {mfa, M, F, A}).

async_mfa(Name, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    gen_server:cast(Name, {mfa, M, F, A}).

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
do_handle_call({mfa, Mod, F, A}, _From, State) ->
    Ret = apply(Mod, F, A),
    {reply, Ret, State};
do_handle_call(Msg, _From, State) ->
    ?INFO("Recv unexpected call msg: ~p", [Msg]),
    {reply, undefined, State}.

do_handle_cast({mfa, Mod, F, A}, State) ->
    apply(Mod, F, A),
    {noreply, State};
do_handle_cast(Msg, State) ->
    ?INFO("Recv unexpected cast msg: ~p", [Msg]),
    {noreply, State}.