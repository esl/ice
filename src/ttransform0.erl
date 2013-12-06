%%------------------------------------------------------------------------------
%% AST Transformation - Pass 0
%%------------------------------------------------------------------------------
-module(ttransform0).

-export([transform0/1]).
-export([test/0]).

%%------------------------------------------------------------------------------
%% This transformation module transforms functions into their corresponding
%% abstractions (. base / ! value / (space) name) and in transforming where
%% clauses into wheredim / wherevar clauses.
%%
%% We define the transformation pass over the entire parse tree for the sake of
%% completeness, but clearly some transformations can result in illegal code.
%% These cases could be specialized in the future if necessary to give us better
%% error reporting.
%%
%% Function AST should look like this:
%% {fn, X, [{b_param, BArg} | {v_param, VArg} | {n_param, NArg}], E}
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

%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
transform0(Const) when is_number(Const) orelse is_boolean(Const) ->
  Const;

transform0({string, Str}) ->
  {string, Str};

transform0({char, Char}) ->
  {char, Char};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
transform0({primop, F, Eis}) ->
  {primop, F, lists:map(fun transform0/1, Eis)};

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
%% Dimensional Query
%%------------------------------------------------------------------------------
transform0({'#', E0}) ->
  {'#', transform0(E0)};

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
transform0({b_abs, Is, Params, E}) ->
  {b_abs, lists:map(fun transform0/1, Is), Params, transform0(E)};

transform0({b_apply, E0, Eis}) ->
  {b_apply, transform0(E0), lists:map(fun transform0/1, Eis)};

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
transform0({v_abs, Is, Params, E}) ->
  {v_abs, lists:map(fun transform0/1, Is), Params, transform0(E)};

transform0({v_apply, E0, Eis}) ->
  {v_apply, transform0(E0), lists:map(fun transform0/1, Eis)};

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
transform0({i_abs, Is, E}) ->
  {i_abs, lists:map(fun transform0/1, Is), transform0(E)};

transform0({i_apply, E}) ->
  {i_apply, transform0(E)};

%%------------------------------------------------------------------------------
%% Function Call
%%------------------------------------------------------------------------------
transform0({fn_call, FnE, Params}) ->
  transform0_fn_call(transform0(FnE),
                     lists:reverse(lists:keymap(fun transform0/1, 2, Params)));

%%------------------------------------------------------------------------------
%% Where
%%------------------------------------------------------------------------------
transform0({where, E0, VDisEis}) ->
  %% We should probably signal an error when the body contains other elements..
  Vars =
    [{Xi,transform0(Ei)} || {var,Xi,Ei} <- VDisEis] ++
    [{Fi,transform0_prime(Params,transform0(E))} %% Function transformation
     || {fn,Fi,Params,E} <- VDisEis],
  Dims = [{Xi,transform0(Ei)} || {dim,Xi,Ei} <- VDisEis],
  transform0_where(Vars, Dims, E0);

%%-------------------------------------------------------------------------------------
%% Identifiers
%%-------------------------------------------------------------------------------------
transform0(Xi) when is_list(Xi) orelse is_atom(Xi) ->
  Xi.

%%------------------------------------------------------------------------------
%% Transform0 prime is responsible for transforming the list of formal
%% parameters in a function declaration into base, value and named
%% abstractions, given a body E.
%%
%% "Base functions [...] take as arguments a tuple, and cannot be
%% curried."
%% Ref: 4.5.2 "Base functions" in paper "Higher-order Multidimensional
%% Programming", Aug 2012
%%------------------------------------------------------------------------------
transform0_prime([], E) ->
  E;
transform0_prime([{b_param,_}|_]=Params, E) ->
  {BPs, Ps} = lists:splitwith(fun({Type,_}) -> Type == b_param end, Params),
  {b_abs, [], lists:map(fun({b_param, BP}) -> BP end, BPs),
   transform0_prime(Ps, E)};
transform0_prime([{v_param, Param}|Ps], E) ->
  {v_abs, [], [Param], transform0_prime(Ps, E)};
transform0_prime([{n_param, Param}|Ps], E) ->
  %%------------------------------------------------------------------------------
  %% FIXME -- Here we need to replace Param in E with an intension
  %% application.  Proposition 9 in Aug 2012 semantics paper:
  %%   [ \\ {Ei} x -> E0 ] == [ \ {Ei} x -> E0[x/↓x] ]
  %% FIXME -- Do the same in anonymous v_abs
  %%------------------------------------------------------------------------------
  {v_abs, [], [Param], transform0_prime(Ps, E)}.

%%------------------------------------------------------------------------------
%% Transform0 fn_call is responsible for transforming the list of
%% actual parameters in a function call into base, value and named
%% applications, given a function FnE.
%%------------------------------------------------------------------------------
transform0_fn_call(FnE, []) ->
  FnE;
transform0_fn_call(FnE, [{b_param,_}|_]=Params) ->
  %% Group base params
  {BPs, Ps} = lists:splitwith(fun({Type,_}) -> Type == b_param end, Params),
  {b_apply, transform0_fn_call(FnE, Ps),
   lists:reverse(lists:map(fun({b_param, BP}) -> BP end, BPs))};
transform0_fn_call(FnE, [{v_param, Param}|Ps]) ->
  {v_apply, transform0_fn_call(FnE, Ps), [Param]};
transform0_fn_call(FnE, [{n_param, Param}|Ps]) ->
  %%------------------------------------------------------------------------------
  %% FIXME -- Here we need to replace the n_param with a v_param that
  %% is an intension abstraction of Param.  Proposition 8 in Aug 2012
  %% semantics paper:
  %%   [ E0 E1 ] == [ E0 ! (↑{} E1) ]
  %%------------------------------------------------------------------------------
  transform0_fn_call(FnE, Ps).

%%------------------------------------------------------------------------------
%% Transform0 where transforms a where clause into wherevar / wheredims.
%%------------------------------------------------------------------------------
transform0_where([], [], E0) ->
  transform0(E0);
transform0_where([], Dims, E0) ->
  {wheredim, transform0(E0), Dims};
transform0_where(Vars, [], E0) ->
  {wherevar, transform0(E0), Vars};
transform0_where(Vars, Dims, E0) ->
  {wheredim,
   {wherevar, transform0(E0), Vars},
   Dims}.

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
