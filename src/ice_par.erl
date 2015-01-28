%%-------------------------------------------------------------------------------------
%% Parallel Evaluation
%%-------------------------------------------------------------------------------------
-module(ice_par).

-export([eval/6, eval_seq/6]).

%%------------------------------------------------------------------------------
%% @doc Evaluate expressions in parallel
%%
%% The order of the evaluated results is guaranteed to be the same as
%% the order of the expressions specified.
%% ------------------------------------------------------------------------------
eval(Xs, _I, _E, _K, _D, _W) when length(Xs) == 0 ->
  Xs;
eval(Xs, I, E, K, D, W) ->
  %%------------------------------------------------------------------------------
  %% Note: Passing self() here means W is self()
  %%------------------------------------------------------------------------------
  Pids = ice_thread:spawn_n(W, length(Xs)),
  ice_thread:join(Pids, Xs, I, E, K, D, W).

%%-------------------------------------------------------------------------------------
%% @doc Evaluate a sequence expressions 
%%-------------------------------------------------------------------------------------
eval_seq(Xs, I, E, K, D, W) ->
  eval_seq(Xs, I, E, K, D, W, []).

eval_seq([], _I, _E, _K, _D, _W, Acc) ->
  lists:reverse(Acc);
eval_seq([X|Xs], I, E, K, D, W, Acc) ->
  D0 = ice_core:eval(X, I, E, K, D, W),
  eval_seq(Xs, I, E, K, D, W, [D0|Acc]).





