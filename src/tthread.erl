%%-------------------------------------------------------------------------------------
%% Threading
%%-------------------------------------------------------------------------------------
-module(tthread).

-export([spawn_n/2, join/8, join_tuple/8]).
-export([evaluator/1]).

%%-------------------------------------------------------------------------------------
%% @doc Spawn N ordered processes responsible for evaluating an expression.
%% Erlang may allocate a Pid which is less than the next pid spawned.
%% Theoretically we need a model which follows the evaluation tree, but for now this is
%% the most efficient way of handling the problem.
%% The processes must be ordered for the cache to work. 
%%-------------------------------------------------------------------------------------
spawn_n(Su, N) ->
  tv:pass({creating_n_threads, N}),
  spawn_n(Su, N, []).

spawn_n(Su, 0, Pids) ->
  lists:sort(Pids);
spawn_n(Su, N, Pids) ->
  Pid = spawn(tthread, evaluator, [Su]),
  tv:pass({thread_created, Pid}),
  spawn_n(Su, N-1, [Pid|Pids]).

%%-------------------------------------------------------------------------------------
%% @doc Join sorted threads to ordered expressions.
%%-------------------------------------------------------------------------------------
join(Pids, Xs, I, E, K, D, W, T) when length(Pids) == length(Xs) ->
  join(Pids, Xs, I, E, K, D, W, T, length(Xs)).

join([], [], I, E, K, D, W, T, Lim) ->
  sync(Lim);
join([Pid|Pids], [X|Xs], I, E, K, D, W, T, Lim) ->
  Pid ! {Pid, X, I, E, K, D, W, T},
  tv:pass({joining_thread,Pid}),
  join(Pids, Xs, I, E, K, D, W, T, Lim).

from_tuple_list(Xs) ->
  from_tuple_list(Xs, []).

from_tuple_list([], TL) ->
  TL;
from_tuple_list([{Xi,Ei}|XiEis], TL) ->
  from_tuple_list(XiEis, TL ++ [Xi, Ei]).

join_tuple(Pids, Xs, I, E, K, D, W, T) ->
  Xs1 = from_tuple_list(Xs),
  join(Pids, Xs1, I, E, K, D, W, T).

%%-------------------------------------------------------------------------------------
%% @doc Synchronize thread W with threads W + 1 to Wn - 1 by receiving their results.
%%-------------------------------------------------------------------------------------
sync(N) ->
  sync(N, []).

sync(0, []) ->
  {[], 0};
sync(0, Acc) ->
  Ord = lists:keysort(1, Acc),
  {_, Es} = lists:unzip(Ord),
  {Vs0, Ts0} = lists:unzip(Es),
  {Vs0, lists:max(Ts0)};
sync(N, Acc) ->
  R = receive X -> X end,
  sync(N-1, [R|Acc]).

%%-------------------------------------------------------------------------------------
%% Internal
%%-------------------------------------------------------------------------------------
evaluator(Su) ->
  {Pid, X, I, E, K, D, W, T} = receive M -> M end,
  evaluator(Su, X, I, E, K, D, Pid, T).
				    
evaluator(Su, X, I, E, K, D, Wi, T) ->
  {D0, T0} = tcore:eval(X, I, E, K, D, Wi, T),
  tv:pass({thread_evaluated, Wi, {X,D,T}, {D0,T0}}),
  Su ! {Wi, {D0, T0}}.

