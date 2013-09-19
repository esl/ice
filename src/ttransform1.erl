%%------------------------------------------------------------------------------
%% AST Transformation - Pass 1
%%------------------------------------------------------------------------------
-module(ttransform1).

-export([test/0]).

%%------------------------------------------------------------------------------
%% This transformation module transforms wheredim identifiers into hidden 
%% dimensions (e.g: t -> {dim, t}) and references to these identifiers as 
%% constant queries to the context ({'?', {dim, t}}).
%%
%% Transformed expressions are wheredim, wherevar, intension abstractions, 
%% intension applications, base and value abstractions / applications.
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
transform1(Const, _H) when is_number(Const) orelse is_boolean(Const) ->
  Const;

transform1({string, Str}, _H) ->
  {string, Str};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
transform1({primop, F, Eis}, H) ->
  {primop, F, lists:map(fun (Ei) -> transform1(Ei, H) end, Eis)};

%%------------------------------------------------------------------------------
%% Tuple Expression
%%------------------------------------------------------------------------------
transform1({t, E0E1is}, H) ->
  {t, lists:map(fun ({E0,E1}) -> {transform1(E0,H),transform1(E1,H)} end, E0E1is)};

%%------------------------------------------------------------------------------
%% Context Perturbation
%%------------------------------------------------------------------------------
transform1({'@', E0, E1}, H) ->
  {'@', transform1(E0, H), transform1(E1, H)};

%%------------------------------------------------------------------------------
%% Conditional
%%------------------------------------------------------------------------------
transform1({'if', E0, E1, E2}, H) ->
  {'if', transform1(E0, H), transform1(E1, H), transform1(E2, H)};

%%------------------------------------------------------------------------------
%% Dimensional Query
%%------------------------------------------------------------------------------
transform1({'#', E0}, H) ->
  {'#', transform1(E0, H)};

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
transform1({b_abs, Is, Params, E, P}, H) ->
  Dims = [{dim, Param} || Param <- Params],
  %% Here we expect P to be a runtime assigned constant
  {b_abs, tset:union(H, [transform1(I, H) || I <- Is]),
   Dims,
   transform1(E, tset:union(H, Dims)),
   P};

transform1({b_apply, E0, E1}, H) ->
  {b_apply, transform1(E0, H), transform1(E1, H)};

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
transform1({v_abs, Is, Params, E}, H) ->
  Dims = [{dim, Param} || Param <- Params],
  {v_abs, tset:union(H, [transform1(I, H) || I <- Is]),
   Dims,
   transform1(E, tset:union(H, Dims))};

transform1({v_apply, E0, E1}, H) ->
  {v_apply, transform1(E0, H), transform1(E1, H)};

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
transform1({i_abs, Is, E}, H) ->
  {i_abs, tset:union(H, [transform1(I, H) || I <- Is]),
   transform1(E, H)};

transform1({i_apply, E}, H) ->
  {i_apply, transform1(E, H)};

%%------------------------------------------------------------------------------
%% Wherevar
%%------------------------------------------------------------------------------
transform1({wherevar, E0, XiEis}, H) ->
  {wherevar, transform1(E0, H),
   [{Xi, transform1(Ei, H)} || {Xi, Ei} <- XiEis]};

%%------------------------------------------------------------------------------
%% Wheredim
%%------------------------------------------------------------------------------
transform1({wheredim, E0, XiEis}, H) ->
  DimsEis = [{{dim, Xi}, transform1(Ei, H)} || {Xi, Ei} <- XiEis],
  Dims = [Dim || {Dim, _} <- DimsEis],
  {wheredim, transform1(E0, tset:union(H, Dims)),
   DimsEis};

%%------------------------------------------------------------------------------
%% Identifiers
%%------------------------------------------------------------------------------
transform1(Xi, H) when is_list(Xi) orelse is_atom(Xi) ->
  case lists:keyfind(Xi, 2, H) of
    {dim, Xi} ->
      {'?', {dim, Xi}};
    _ ->
      Xi
  end.

%%------------------------------------------------------------------------------
%% Insta-test
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
  R = ttransform0:transform0(d1_tournament()),
  transform1(R, []).





			    


