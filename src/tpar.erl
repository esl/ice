%%-------------------------------------------------------------------------------------
%% Parallel Evaluation
%%-------------------------------------------------------------------------------------
-module(tpar).

-export([eval/7, eval_seq/7]).

%%------------------------------------------------------------------------------
%% @doc Evaluate expressions in parallel
%%
%% The order of the evaluated results is guaranteed to be the same as
%% the order of the expressions specified.
%% ------------------------------------------------------------------------------
eval(Xs, I, E, K, D, W, T) ->
  Lim = length(Xs),
  Pids = tthread:spawn_n(self(), Lim),
  tthread:join(Pids, Xs, I, E, K, D, W, T).

%%-------------------------------------------------------------------------------------
%% @doc Evaluate expressions sequentially (useful for debugging)
%%-------------------------------------------------------------------------------------
eval_seq(Xs, I, E, K, D, W, T) ->
  eval_seq(Xs, I, E, K, D, W, T, []).

eval_seq([], I, E, K, D, W, T, Acc) ->
  {lists:reverse(Acc), T};
eval_seq([X|Xs], I, E, K, D, W, T, Acc) ->
  {D0, T1} = tcore:eval(X, I, E, K, D, W, T),
  case T1 > T of
    true ->
      eval_seq(Xs, I, E, K, D, W, T1, [D0|Acc]);
    false ->
      eval_seq(Xs, I, E, K, D, W, T, [D0|Acc])
  end.




