%%------------------------------------------------------------------------------
%% AST Transformation - Pass 0
%%------------------------------------------------------------------------------
-module(ice_t0).

-export([transform/1]).

%%------------------------------------------------------------------------------
%% @doc Transform functions and where clauses.
%%
%% Transform functions into their corresponding abstractions (. base /
%% ! value / (space) name) and where clauses into wheredim / wherevar
%% clauses.
%%
%% We define the transformation pass over the entire parse tree for the sake of
%% completeness, but clearly some transformations can result in illegal code.
%% These cases could be specialized in the future if necessary to give us better
%% error reporting.
%%
%% Function AST should look like this:
%% {fn, [FrozenDimName], [{b_param, BArg} | {v_param, VArg} | {n_param, NArg}], E}
%% {fn_call, E, [{b_param, BArg} | {v_param, VArg} | {n_param, NArg}]}
%%
%% Where AST should look like this:
%% {where, E0, [{dim, DName, DVal}, {var, VName, VVal}]}
%%
%% Intension abstraction/application AST should look like this:
%% {i_abs, [FrozenDimName], E}
%% {i_apply, E}
%%------------------------------------------------------------------------------
transform(E) -> t0(E, []).

%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
t0({bool, _} = Bool, _) ->
  Bool;

t0({char, _} = Char, _) ->
  Char;

t0({int, _} = Int, _) ->
  Int;

t0({float, _} = Float, _) ->
  Float;

t0({string, _} = String, _) ->
  String;

%%------------------------------------------------------------------------------
%% Sequence
%%------------------------------------------------------------------------------
t0({seq, E0, E1}, NPs) ->
  {seq, t0(E0, NPs), t0(E1, NPs)};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
t0({primop, F, Eis}, NPs) ->
  {primop, F, lists:map(fun(Ei) -> t0(Ei, NPs) end, Eis)};

%%------------------------------------------------------------------------------
%% Tuple Expression
%%------------------------------------------------------------------------------
t0({t, E0E1is}, NPs) ->
  {t, lists:map(fun({E0,E1}) -> {t0(E0, NPs), t0(E1, NPs)} end, E0E1is)};

%%------------------------------------------------------------------------------
%% Context Perturbation
%%------------------------------------------------------------------------------
t0({'@', E0, E1}, NPs) ->
  {'@', t0(E0, NPs), t0(E1, NPs)};

%%------------------------------------------------------------------------------
%% Conditional
%%------------------------------------------------------------------------------
t0({'if', E0, E1, E2}, NPs) ->
  {'if', t0(E0, NPs), t0(E1, NPs), t0(E2, NPs)};

%%------------------------------------------------------------------------------
%% Dimensional Query
%%------------------------------------------------------------------------------
t0({'#', E0}, NPs) ->
  {'#', t0(E0, NPs)};

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
t0({b_abs, Is, Params, E}, NPs) ->
  {b_abs, [t0(I, NPs) || I <- Is], Params, t0(E, NPs)};

t0({b_apply, E0, Eis}, NPs) ->
  {b_apply, t0(E0, NPs), lists:map(fun(Ei) -> t0(Ei, NPs) end, Eis)};

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
t0({v_abs, Is, Params, E}, NPs) ->
  {v_abs, [t0(I, NPs) || I <- Is], Params, t0(E, NPs)};

t0({v_apply, E0, Eis}, NPs) ->
  {v_apply, t0(E0, NPs), lists:map(fun(Ei) -> t0(Ei, NPs) end, Eis)};

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
t0({i_abs, Is, E}, NPs) ->
  {i_abs, [t0(I, NPs) || I <- Is], t0(E, NPs)};

t0({i_apply, E}, NPs) ->
  {i_apply, t0(E, NPs)};

%%------------------------------------------------------------------------------
%% Function
%%------------------------------------------------------------------------------
t0({fn, Is, Params, E}, NPs) ->
  t0_fn(Is, Params, E, NPs);

t0({fn_call, FnE, Params}, NPs) ->
  t0_fn_call(
    t0(FnE, NPs),
    lists:reverse(lists:map(fun({Type, P}) -> {Type, t0(P, NPs)} end, Params)));

%%------------------------------------------------------------------------------
%% Where
%%------------------------------------------------------------------------------
t0({where, E0, VDisEis}, NPs0) ->
  Dims = [{Xi, t0(Ei, NPs0)} || {dim,Xi,Ei} <- VDisEis],
  NPs1 = ice_sets:difference(NPs0, element(1, lists:unzip(Dims))),
  Vars0 = [{Xi,Ei} || {var,Xi,Ei} <- VDisEis],
  NPs2 = ice_sets:difference(NPs1, element(1, lists:unzip(Vars0))),
  Vars1 = [{Xi, t0(Ei, NPs2)} || {Xi,Ei} <- Vars0],
  %% We should probably signal an error when the body contains other elements..
  t0_where(Vars1, Dims, E0, NPs2);

%%------------------------------------------------------------------------------
%% Identifiers
%%------------------------------------------------------------------------------
t0({id, _} = Xi, NamedParams) ->
  case lists:member(Xi, NamedParams) of
    true  -> {i_apply, Xi};
    false ->           Xi
  end.

%%------------------------------------------------------------------------------
%% @doc Transform function declaration.
%%
%% Transform the list of formal parameters in a function declaration
%% into base, value and named abstractions, given a body E.
%%
%% "Base functions [...] take as arguments a tuple, and cannot be
%% curried."
%% Ref: 4.5.2 "Base functions" in paper "Higher-order Multidimensional
%% Programming", Aug 2012
%% @private
%%------------------------------------------------------------------------------
t0_fn([], [], E, NPs) ->
  t0(E, NPs);
t0_fn(Is, [{b_param,_}|_]=Params, E, NPs) ->
  %% Group consecutive initial base params
  {BPs0, Ps} = lists:splitwith(fun({Type,_}) -> Type == b_param end, Params),
  {_, BPs1} = lists:unzip(BPs0),
  {b_abs, Is, BPs1, t0_fn([], Ps, E, ice_sets:difference(NPs, BPs1))};
t0_fn(Is, [{v_param, Param}|Ps], E, NPs) ->
  {v_abs, Is, [Param], t0_fn([], Ps, E, ice_sets:difference(NPs, [Param]))};
t0_fn(Is, [{n_param, Param}|Ps], E, NamedParams) ->
  %%------------------------------------------------------------------------------
  %% Replace Param in E with an intension application.  Ref
  %% proposition 9 in Aug 2012 semantics paper:
  %%   [ \\ {Ei} x -> E0 ] == [ \ {Ei} x -> E0[x/↓x] ]
  %%------------------------------------------------------------------------------
  {v_abs, Is, [Param], t0_fn([], Ps, E, ice_sets:union(NamedParams, [Param]))}.

%%------------------------------------------------------------------------------
%% @doc Transform function call.
%%
%% Transform the list of actual parameters in a function call into
%% base, value and named applications, given a function FnE.
%% @private
%%------------------------------------------------------------------------------
t0_fn_call(FnE, []) ->
  FnE;
t0_fn_call(FnE, [{b_param,_}|_]=Params) ->
  %% Group consecutive initial base params
  {BPs, Ps} = lists:splitwith(fun({Type,_}) -> Type == b_param end, Params),
  {b_apply, t0_fn_call(FnE, Ps),
   lists:reverse(lists:map(fun({b_param, BP}) -> BP end, BPs))};
t0_fn_call(FnE, [{v_param, Param}|Ps]) ->
  {v_apply, t0_fn_call(FnE, Ps), [Param]};
t0_fn_call(FnE, [{n_param, Param}|Ps]) ->
  %%------------------------------------------------------------------------------
  %% Replace the n_param with a v_param that is an intension
  %% abstraction of Param. Ref proposition 8 in Aug 2012 semantics
  %% paper:
  %%   [ E0 E1 ] == [ E0 ! (↑{} E1) ]
  %%------------------------------------------------------------------------------
  {v_apply, t0_fn_call(FnE, Ps), [{i_abs, [], Param}]}.

%%------------------------------------------------------------------------------
%% @doc Transform a where clause into wherevar / wheredims.
%% @private
%%------------------------------------------------------------------------------
t0_where([], [], E0, NPs) ->
  t0(E0, NPs);
t0_where([], Dims, E0, NPs) ->
  {wheredim, t0(E0, NPs), Dims};
t0_where(Vars, [], E0, NPs) ->
  {wherevar, t0(E0, NPs), Vars};
t0_where(Vars, Dims, E0, NPs) ->
  {wheredim, {wherevar, t0(E0, NPs), Vars}, Dims}.
