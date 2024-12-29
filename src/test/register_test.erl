%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2024, <1377519410@qq.com>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12月 2024 21:47
%%%-------------------------------------------------------------------
-module(register_test).

-compile(export_all).
-compile(nowarn_export_all).

-include("common.hrl").
-include("register.hrl").

%% API
-export([]).

register_test() ->
    Node = 'qbx_10@127.0.0.1',
    register_test(Node).

register_test(Node) ->
    case net_adm:ping(Node) of
        pong ->
            io:format("Connect Node ~w ~n", [Node]),
            svr_register:register_node(Node),
            rpc:cast(Node, svr_register, register_node, [node()]),

            sup_mfa:start_child(svr_mfa_local),
            sup_mfa:start_child({global, svr_mfa_global}),
            sup_mfa:start_child(?VIA_NAME({mfa, via})),
            ok;
        _ ->
            io:format("Cant connect Node ~w ~n", [Node])
    end,
    ok.

clean() ->
    PidA = whereis(svr_mfa_local),
    PidB = global:whereis_name(svr_mfa_global),
    PidC = svr_register:whereis_name(?VIA_NAME({mfa, via})),

    is_pid(PidA) andalso exit(PidA, normal),
    is_pid(PidB) andalso exit(PidB, normal),
    is_pid(PidC) andalso exit(PidC, normal),
    ok.




