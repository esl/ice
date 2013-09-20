%%-------------------------------------------------------------------------------------
%% Parallel Evaluation
%%-------------------------------------------------------------------------------------
-module(tpar).

-export([eval/7, eval_seq/7, eval_tuple/7]).

%%------------------------------------------------------------------------------
%% @doc Evaluate expressions in parallel
%%------------------------------------------------------------------------------
eval(Xs, I, E, K, D, W, T) ->
  Lim = length(Xs),
  Pids = tthread:spawn_n(self(), Lim),
  tthread:join(Pids, Xs, I, E, K, D, W, T).

%%-------------------------------------------------------------------------------------
%% @doc Evaluate tuples in parallel
%%-------------------------------------------------------------------------------------
eval_tuple(Xs, I, E, K, D, W, T) ->
  Lim = length(Xs) * 2,
  Pids = tthread:spawn_n(self(), Lim),
  tthread:join_tuple(Pids, Xs, I, E, K, D, W, T).

%%-------------------------------------------------------------------------------------
%% @doc Evaluate expressions sequentially (useful for debugging)
%%-------------------------------------------------------------------------------------
eval_seq(Xs, I, E, K, D, W, T) ->
  eval_seq(Xs, I, E, K, D, W, T, []).

eval_seq([], _I, _E, _K, _D, _W, T, Acc) ->
  {lists:reverse(Acc), T};
eval_seq([X|Xs], I, E, K, D, W, T, Acc) ->
  {D0, T1} = tcore:eval(X, I, E, K, D, W, T),
  case T1 > T of
    true ->
      eval_seq(Xs, I, E, K, D, W, T1, [D0|Acc]);
    false ->
      eval_seq(Xs, I, E, K, D, W, T, [D0|Acc])
  end.




