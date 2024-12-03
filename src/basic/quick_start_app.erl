%%%-------------------------------------------------------------------
%% @doc quick_start public API
%% @end
%%%-------------------------------------------------------------------

-module(quick_start_app).

-behaviour(application).

-include("common.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case quick_start_sup:start_link() of
        {ok, Pid} ->
            start_nodes(),
            {ok, Pid};
        Error ->
            Error
   end.

stop(_State) ->
    stop_node(),
    ok.

start_nodes() ->
    [NodeIDStr | _] = init:get_plain_arguments(),
    NodeID = list_to_integer(NodeIDStr),
    ?INFO("starting node ~w:~w", [NodeID, node()]),
    if
        NodeID =:= 10 ->
            node_logic:start([NodeID]);
        true ->
            ignore
    end,
    ok.

stop_node() ->
    [NodeIDStr | _] = init:get_plain_arguments(),
    NodeID = list_to_integer(NodeIDStr),
    ?INFO("stoping node ~w:~w", [NodeID, node()]),
    if
        NodeID =:= 10 ->
            node_logic:stop();
        true ->
            ignore
    end.