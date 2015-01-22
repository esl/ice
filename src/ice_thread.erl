%%-------------------------------------------------------------------------------------
%% Threading
%%-------------------------------------------------------------------------------------
-module(ice_thread).

-export([spawn_n/2, join/8]).
-export([spawn_n_debug/2]).
-export([evaluator/1]).

%%-------------------------------------------------------------------------------------
%% @doc Spawn N ordered processes responsible for evaluating an expression.
%% Erlang may allocate a Pid which is less than the next pid spawned.
%% Theoretically we need a model which follows the evaluation tree, but for now this is
%% the most efficient way of handling the problem.
%% The processes must be ordered for the cache to work. 
%%-------------------------------------------------------------------------------------

spawn_n({_N, _Su} = W, Lim) ->
  spawn_n(W, Lim, gb_trees:empty()).

spawn_n(_W, 0, Tree) ->
  Tree;
spawn_n({_N, Su} = W, Lim, Tree) ->
  Pid = spawn(ice_thread, evaluator, [Su]),
  Id = ice_counter:inc(),
  spawn_n(W, Lim-1, gb_trees:insert(Id, Pid, Tree)).

%%------------------------------------------------------------------------------
%% @doc The following spawn_n creates prefix lists which can later be used to 
%% ensure that cache writes are done in a sane manner.
%%------------------------------------------------------------------------------

spawn_n_debug({_P, _Su} = W, Lim) ->
  spawn_n_debug(W, 0, Lim, []).

spawn_n_debug(_W, N, Lim, Pids) when N >= Lim ->
  lists:reverse(Pids);
spawn_n_debug({P, Su} = W, N, Lim, Pids) ->
  Pid = spawn(ice_thread, evaluator, [Su]),
  spawn_n_debug(W, N+1, Lim, [{P ++ [N], Pid}|Pids]).

%%-------------------------------------------------------------------------------------
%% @doc Join sorted processes into ordered evaluated values.
%%-------------------------------------------------------------------------------------

join(Tree, Xs, I, E, K, D, W, T) ->
  %% FIXME: Maybe add an assertion here that checks that length(Xs) == length(Tree)
  Iter = gb_trees:iterator(Tree),
  join(Iter, Xs, I, E, K, D, W, T, length(Xs)).

join([], _Xs, _I, _E, _K, _D, _W, _T, Lim) ->
  sync(Lim);
join(Iter, [X|Xs], I, E, K, D, W, T, Lim) ->
  {N, Pid, NextIter} = gb_trees:next(Iter),
  Pid ! {{N, Pid}, X, I, E, K, D, W, T},
  join(NextIter, Xs, I, E, K, D, W, T, Lim).

%%-------------------------------------------------------------------------------------
%% @doc Synchronize thread W with threads W + 1 to Wn - 1 by receiving their results.
%%-------------------------------------------------------------------------------------

sync(N) ->
  sync(N, gb_trees:empty()).

sync(0, Tree) ->
  case gb_trees:is_empty(Tree) of
    true ->
      {[], 0};
    false ->
      Ord = gb_trees:values(Tree),
      %% FIXME: Since we do not care about time, this should also be removed
      {Vs0, Ts0} = lists:unzip(Ord),
      {Vs0, lists:max(Ts0)}
  end;
sync(N, Tree) ->
  {Id, Value} = receive M -> M end,
  sync(N-1, gb_trees:insert(Id, Value, Tree)).

%%-------------------------------------------------------------------------------------
%% Internal
%%-------------------------------------------------------------------------------------
evaluator(Su) ->
  {W1, X, I, E, K, D, _, T} = receive M -> M end,
  evaluator(Su, X, I, E, K, D, W1, T).
				    
evaluator(Su, X, I, E, K, D, Wi, T) ->
  {D0, T0} = ice_core:eval(X, I, E, K, D, Wi, T),
  Su ! {Wi, {D0, T0}}.

