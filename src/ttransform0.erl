%%------------------------------------------------------------------------------
%% AST Transformation - Pass 0
%%------------------------------------------------------------------------------
-module(ttransform0).

-export([transform0/1]).
-export([test/0]).

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
%% {fn, [{b_param, BArg} | {v_param, VArg} | {n_param, NArg}], E}
%% {fn_call, E, [{b_param, BArg} | {v_param, VArg} | {n_param, NArg}]}
%%
%% Where AST should look like this:
%% {where, E0, [{dim, DName, DVal}, {var, VName, VVal}]}
%%
%% Unnamed function AST should look like this:
%% {b_abs, [FrozenDimName], [BArg], E}
%% {v_abs, [FrozenDimName], [VArg], E}
%% {n_abs, [FrozenDimName], [NArg], E}
%%
%% Intension abstraction/application AST should look like this:
%% {i_abs, [FrozenDimName], E}
%% {i_apply, E}
%%------------------------------------------------------------------------------
transform0(E) -> t0(E).

%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
t0(Const) when is_number(Const) orelse is_boolean(Const) ->
  Const;

t0({string, Str}) ->
  {string, Str};

t0({char, Char}) ->
  {char, Char};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
t0({primop, F, Eis}) ->
  {primop, F, lists:map(fun t0/1, Eis)};

%%------------------------------------------------------------------------------
%% Tuple Expression
%%------------------------------------------------------------------------------
t0({t, E0E1is}) ->
  {t, lists:map(fun({E0,E1}) -> {t0(E0), t0(E1)} end, E0E1is)};

%%------------------------------------------------------------------------------
%% Context Perturbation
%%------------------------------------------------------------------------------
t0({'@', E0, E1}) ->
  {'@', t0(E0), t0(E1)};

%%------------------------------------------------------------------------------
%% Conditional
%%------------------------------------------------------------------------------
t0({'if', E0, E1, E2}) ->
  {'if', t0(E0), t0(E1), t0(E2)};

%%------------------------------------------------------------------------------
%% Dimensional Query
%%------------------------------------------------------------------------------
t0({'#', E0}) ->
  {'#', t0(E0)};

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
t0({b_abs, Is, Params, E}) ->
  {b_abs, [t0(I) || I <- Is], Params, t0(E)};

t0({b_apply, E0, Eis}) ->
  {b_apply, t0(E0), lists:map(fun t0/1, Eis)};

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
t0({v_abs, Is, Params, E}) ->
  {v_abs, [t0(I) || I <- Is], Params, t0(E)};

t0({v_apply, E0, Eis}) ->
  {v_apply, t0(E0), lists:map(fun t0/1, Eis)};

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
t0({i_abs, Is, E}) ->
  {i_abs, [t0(I) || I <- Is], t0(E)};

t0({i_apply, E}) ->
  {i_apply, t0(E)};

%%------------------------------------------------------------------------------
%% Function
%%------------------------------------------------------------------------------
t0({fn, Params, E}) ->
  t0_fn(Params, E);

t0({fn_call, FnE, Params}) ->
  t0_fn_call(
    t0(FnE),
    lists:reverse(lists:map(fun({Type,P}) -> {Type,t0(P)} end, Params)));

%%------------------------------------------------------------------------------
%% Where
%%------------------------------------------------------------------------------
t0({where, E0, VDisEis}) ->
  %% We should probably signal an error when the body contains other elements..
  Vars = [{Xi, t0(Ei)} || {var,Xi,Ei} <- VDisEis],
  Dims = [{Xi, t0(Ei)} || {dim,Xi,Ei} <- VDisEis],
  t0_where(Vars, Dims, E0);

%%------------------------------------------------------------------------------
%% Identifiers
%%------------------------------------------------------------------------------
t0(Xi) when is_list(Xi) orelse is_atom(Xi) ->
  Xi.

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
t0_fn([], E) ->
  t0(E);
t0_fn([{b_param,_}|_]=Params, E) ->
  %% Group consecutive initial base params
  {BPs, Ps} = lists:splitwith(fun({Type,_}) -> Type == b_param end, Params),
  {b_abs, [], lists:map(fun({b_param, BP}) -> BP end, BPs), t0_fn(Ps, E)};
t0_fn([{v_param, Param}|Ps], E) ->
  {v_abs, [], [Param], t0_fn(Ps, E)};
t0_fn([{n_param, Param}|Ps], E) ->
  %%------------------------------------------------------------------------------
  %% FIXME -- Here we need to replace Param in E with an intension
  %% application.  Proposition 9 in Aug 2012 semantics paper:
  %%   [ \\ {Ei} x -> E0 ] == [ \ {Ei} x -> E0[x/↓x] ]
  %% FIXME -- Do the same in anonymous v_abs
  %%------------------------------------------------------------------------------
  {v_abs, [], [Param], t0_fn(Ps, E)}.

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
  %% FIXME -- Here we need to replace the n_param with a v_param that
  %% is an intension abstraction of Param.  Proposition 8 in Aug 2012
  %% semantics paper:
  %%   [ E0 E1 ] == [ E0 ! (↑{} E1) ]
  %%------------------------------------------------------------------------------
  t0_fn_call(FnE, Ps).

%%------------------------------------------------------------------------------
%% @doc Transform a where clause into wherevar / wheredims.
%% @private
%%------------------------------------------------------------------------------
t0_where([], [], E0) ->
  t0(E0);
t0_where([], Dims, E0) ->
  {wheredim, t0(E0), Dims};
t0_where(Vars, [], E0) ->
  {wherevar, t0(E0), Vars};
t0_where(Vars, Dims, E0) ->
  {wheredim, {wherevar, t0(E0), Vars}, Dims}.

%%------------------------------------------------------------------------------
%% Instant Tests - Please improve these
%%------------------------------------------------------------------------------
fby() ->
  %%------------------------------------------------------------------------------
  %% The fby function
  %%
  %% fun fby.d A B = if #.d <= 0 then A else B @ [d <- #.d - 1] fi
  %%------------------------------------------------------------------------------ 
  {fn, "fby", [{b_param, d}, {n_param, "A"}, {n_param, "B"}],
   {'if', tprimop:lte({'#', {dim,d}}, 0),
    "A",
    {'@', "B", {t, [{{dim,d}, tprimop:minus({'#',{dim,d}}, 1)}]}}}}.

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
      {'if', tprimop:lte({'#', {dim,time}}, 0),
       "X",
       {'@',
	tprimop:plus({'@', "Y", {t, [{{dim,d}, tprimop:plus(tprimop:times({'#', {dim,d}}, 2), 1)}]}},
		     {'@', "Y", {t, [{{dim,d}, tprimop:times({'#', {dim,d}}, 2)}]}}),
	{t, [{{dim,time}, tprimop:minus({'#', {dim,time}}, 1)}]}}}}]}}.


test() ->
  transform0(d1_tournament()),
  transform0(fby()).
