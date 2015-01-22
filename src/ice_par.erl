%%-------------------------------------------------------------------------------------
%% Parallel Evaluation
%%-------------------------------------------------------------------------------------
-module(ice_par).

-export([eval/7, eval_seq/7]).

%%------------------------------------------------------------------------------
%% @doc Evaluate expressions in parallel
%%
%% The order of the evaluated results is guaranteed to be the same as
%% the order of the expressions specified.
%% ------------------------------------------------------------------------------
eval(Xs, _I, _E, _K, _D, _W, _T) when length(Xs) == 0 ->
  {Xs, 0};
eval(Xs, I, E, K, D, W, T) ->
  %%------------------------------------------------------------------------------
  %% Note: Passing self() here means W is self()
  %%------------------------------------------------------------------------------
  Pids = ice_thread:spawn_n(W, length(Xs)),
  ice_thread:join(Pids, Xs, I, E, K, D, W, T).

%%-------------------------------------------------------------------------------------
%% @doc Evaluate a sequence expressions 
%%-------------------------------------------------------------------------------------
eval_seq(Xs, I, E, K, D, W, T) ->
  eval_seq(Xs, I, E, K, D, W, T, []).

eval_seq([], I, E, K, D, W, T, Acc) ->
  {lists:reverse(Acc), T};
eval_seq([X|Xs], I, E, K, D, W, T, Acc) ->
  {D0, T1} = ice_core:eval(X, I, E, K, D, W, T),
  case T1 > T of
    true ->
      eval_seq(Xs, I, E, K, D, W, T1, [D0|Acc]);
    false ->
      eval_seq(Xs, I, E, K, D, W, T, [D0|Acc])
  end.





