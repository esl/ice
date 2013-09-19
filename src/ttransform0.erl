%%------------------------------------------------------------------------------
%% AST Transformation
%%------------------------------------------------------------------------------
-module(ttransform0).

-export([transform0/1]).
-export([test/0]).

%%------------------------------------------------------------------------------
%% This transform0 module is responsible for transforming functions into the
%% appropriate abstractions (base / value / name) and in transforming where
%% clauses into wheredim / wherevar clauses.
%% We define the transformation pass over the entire parse tree for the sake of
%% completeness, but clearly some transformations can result in illegal code.
%% These cases could be specialized in the future if necessary to give us better
%% error reporting.
%%
%% Function AST should look like this:
%% {fn, X, [{b_param, BArg} | {v_param, VArg} | {n_param, NArg}], E}
%%
%% Where AST should look like this:
%% {where, E0, [{dim, DName, DVal}, {var, VName, VVal}]}
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
transform0(Const) when is_number(Const) ->
  Const;

transform0(Bool) when is_boolean(Bool) ->
  Bool;

transform0({string, Str}) ->
  {string, Str};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
transform0({primop, F, Eis}) ->
  {primop, F, lists:map(fun (Exp) -> transform0(Exp) end, Eis)};

%%------------------------------------------------------------------------------
%% Tuple Expression
%%------------------------------------------------------------------------------
transform0({t, E0E1is}) ->
  {t, lists:map(fun ({E0,E1}) -> {transform0(E0), transform0(E1)} end, E0E1is)};

%%------------------------------------------------------------------------------
%% Context Perturbation
%%------------------------------------------------------------------------------
transform0({'@', E0, E1}) ->
  {'@', transform0(E0), transform0(E1)};

%%------------------------------------------------------------------------------
%% Conditional
%%------------------------------------------------------------------------------
transform0({'if', E0, E1, E2}) ->
  {'if', transform0(E0), transform0(E1), transform0(E2)};

%%------------------------------------------------------------------------------
%% Where
%%------------------------------------------------------------------------------
transform0({where, E0, VDisEis}) ->
  %% We should probably signal an error when the body contains other elements..
  Vars = [{Xi,transform0(Ei)} || {var,Xi,Ei} <- VDisEis],
  Dims = [{Xi,transform0(Ei)} || {dim,Xi,Ei} <- VDisEis],
  {wheredim, 
   {wherevar, transform0(E0), Vars},
   Dims};

%%------------------------------------------------------------------------------
%% Dimensional Query
%%------------------------------------------------------------------------------
transform0({'#', E0}) ->
  {'#', transform0(E0)};

%%------------------------------------------------------------------------------
%% Function Transformation
%%------------------------------------------------------------------------------
transform0({fn, X, Params, E}) ->
  {var, X, transform0_prime(lists:reverse(Params), transform0(E))};

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
transform0({b_abs, Is, Params, E}) ->
  {b_abs, Is, Params, transform0(E)};

transform0({b_apply, E0, E1}) ->
  {b_apply, transform0(E0), transform0(E1)};

%%------------------------------------------------------------------------------
%% Identifiers
%%------------------------------------------------------------------------------
transform0(Xi) when is_list(Xi) orelse is_atom(Xi) ->
  Xi.

%%------------------------------------------------------------------------------
%% Transform0 prime is responsible for transforming a list of function parameters
%% into base, value and named abstractions, given a body E.
%%------------------------------------------------------------------------------
transform0_prime([], E) ->
  E;
transform0_prime([{b_param, Param}|Ps], E) ->
  transform0_prime(Ps, {b_abs, [], [Param], E});
transform0_prime([{v_param, Param}|Ps], E) ->
  transform0_prime(Ps, {v_abs, [], [Param], E});
transform0_prime([{n_param, Param}|Ps], E) ->
  transform0_prime(Ps, {n_abs, [], [Param], E}).

%%------------------------------------------------------------------------------
%% Instant Tests - Please improve these
%%------------------------------------------------------------------------------
d1_tournament() ->
  %%------------------------------------------------------------------------------
  %% The following function should be equivalent to the following:
  %%
  %% fun tournament.d.lim X = Y
  %% where
  %%   var Y = 
  %%     if #.t <= 0 then 
  %%       X 
  %%     else
  %%       (Y @ [d <- #.d * 2 + 1] + Y @ [d <- #.d * 2]) @ [t <- #.t - 1]
  %%     fi
  %% end
  %%------------------------------------------------------------------------------

  {fn, "tournament", [{b_param, d}, {b_param, lim}, {n_param, "X"}],
   {where, "Y",
    [{var, "Y",
      {'if', tprimop:lte({'#', time}, 0),
       "X",
       {'@',
	tprimop:plus({'@', "Y", {t, [{d, tprimop:plus(tprimop:times({'#', d}, 2), 1)}]}},
		     {'@', "Y", {t, [{d, tprimop:times({'#', d}, 2)}]}}),
	{t, [{time, tprimop:minus({'#', time}, 1)}]}}}}]}}.


test() ->
  transform0(d1_tournament()).








