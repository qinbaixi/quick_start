%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12月 2024 22:41
%%%-------------------------------------------------------------------
-module(sup_mfa).


-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Name) ->
    supervisor:start_child(?MODULE, [Name]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
        intensity => 5,
        period => 30},

    Children = [
        #{
            id => 'svr_mfa',
            start => {'svr_mfa', start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => ['svr_mfa']
        }
    ],

    {ok, {SupFlags, Children}}.