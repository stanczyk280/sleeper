-module(sleeper).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([sleep/1, update_sleepers_list/3]).
-import(gb_trees, [empty/0, enter/3, smallest/1, delete/2]).

-record(state, {tree = empty(), port}).

sleep(MicroSeconds) ->
    Now = os:system_time(microsecond),
    WakeupTime = Now + MicroSeconds,
    gen_server:call(sleeper, {wakeupTime, WakeupTime}).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    Port = open_port({spawn, "./priv/sleeper"}, [{packet, 4}, binary]),
    process_flag(trap_exit, true),
    {ok, #state{tree = empty(), port = Port}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({wakeupTime, WakeupTime}, From, State = #state{tree = Tree}) ->

    SleepersList = update_sleepers_list(Tree, WakeupTime, From),
    NewTree = gb_trees:enter(WakeupTime, SleepersList, Tree),

    logger:debug("Tree: ~p~n", [NewTree]),
    {noreply, State#state{tree = NewTree}, 0}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State = #state{tree = Tree, port = Port}) ->
    {WakeupTime, Sleepers} = get_sleepers(Tree),
    logger:debug("WakeupTime: ~p~n", [WakeupTime]),
    Now = os:system_time(microsecond),
    if WakeupTime =< Now ->
        logger:debug("Waking up"),
        lists:foreach(fun(From) -> gen_server:reply(From, ok) end, Sleepers),
        NewTree = gb_trees:delete_any(WakeupTime, Tree),
        {noreply, State#state{tree = NewTree}};
    true ->
        logger:debug("Sleeping for ~p~n", [WakeupTime - Now]),
        SleepTime = WakeupTime - Now,
        port_command(Port, <<SleepTime:32>>),
        {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_sleepers(Tree) ->
    case gb_trees:is_empty(Tree) of
        true -> {0,[]};
        false -> 
            {Time, SleepersList} = gb_trees:smallest(Tree),
            {Time, SleepersList}
    end.

update_sleepers_list(Tree, WakeupTime, From) ->
    case gb_trees:lookup(WakeupTime, Tree) of
        none -> 
            SleepersList = [From],
            logger:debug("SleepersList init: ~p~n", [SleepersList]),
            SleepersList;
        _ -> 
            SleepersList = [From | gb_trees:get(WakeupTime, Tree)],
            logger:debug("SleepersList update: ~p~n", [SleepersList]),
            SleepersList
    end.