%%------------------------------------------------------------------------------
%% AST Transformation
%%------------------------------------------------------------------------------
-module(ttransform0).

-export([transform0/1]).
-export([test/0]).

%%------------------------------------------------------------------------------
%% Transform0 is responsible for transforming functions only. We define the
%% transformation pass over the entire parse tree 'just in case', but 
%% realistically defining functions in certain places should not be legal.
%% These cases could be specialized in the future if necessary to give us better
%% error reporting.
%%
%% Function AST should look like this:
%% {fn, X, [{b_param, BArg} | {v_param, VArg} | {n_param, NArg}], E}
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
%% Wherevar
%%------------------------------------------------------------------------------
transform0({wherevar, E0, XiEis}) ->
  {wherevar, transform0(E0), lists:map(fun ({Xi,Ei}) -> {Xi, transform0(Ei)} end,
					    XiEis)};

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
%% Wheredim
%%------------------------------------------------------------------------------
transform0({wheredim, E0, XiEis}) ->
  {wheredim, transform0(E0), lists:map(fun ({Xi,Ei}) -> {Xi, transform0(Ei)} end,
				      XiEis)};

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
  {fn, "tournament", [{b_param, d}, {b_param, lim}, {n_param, "X"}],
   {wherevar, "Y",
    [{"Y", 
      {'if', tprimop:lte({'#', time}, 0),
       "X",
       {'@',
	tprimop:plus({'@', "Y", {t, [{d, tprimop:plus(tprimop:times({'#', d}, 2), 1)}]}},
		     {'@', "Y", {t, [{d, tprimop:times({'#', d}, 2)}]}}),
	{t, [{time, tprimop:minus({'#', time}, 1)}]}}}}]}}.

test() ->
  transform0(d1_tournament()).








