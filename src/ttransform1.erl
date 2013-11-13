%%------------------------------------------------------------------------------
%% AST Transformation - Pass 1
%%------------------------------------------------------------------------------
-module(ttransform1).

-include_lib("eunit/include/eunit.hrl").

-export([transform1/1]).
-export([test/0]).

%%------------------------------------------------------------------------------
%% This transformation module transforms local dimension identifiers
%% and formal parameters.
%%
%% Expressions whose transformation is not straightforward are:
%% wheredim, intension / base / value abstractions, identifiers.
%%
%% Local dimension identifiers are in wheredim clauses. Such
%% identifiers (e.g. t) are transformed into respective hidden
%% dimensions (e.g. {dim, {Pos,Idx}, t}). References to such
%% dimensions in the wheredim's body are transformed to equivalent
%% references to the respective hidden dimensions (e.g. {dim, t} ->
%% {dim, {Pos,Idx}, t}).
%%
%% Formal parameters are in abstractions; such parameters (e.g. t) can
%% be considered as local variable identifiers and are transformed
%% into respective dimensions (e.g. {phi, t}). References to such
%% variables in the abstraction's body are transformed to context
%% queries of the respective dimensions (e.g. t -> {'#', {phi, t}}).
%%
%% Local dimension identifiers and formal parameters are considered
%% belonging to separate domains, i.e. local wheredim dimension t and
%% formal parameter t do not shadow each other when wheredim clauses
%% and base / value abstractions are nested.
%%------------------------------------------------------------------------------

transform1(E) ->
  transform1(E, root_expr_pos(), [], []).


-spec transform1(AstIn :: term(), P :: pos(), HD, HV) -> AstOut :: term() when
    %% HD is the list of hidden dimensions allocated for replacing
    %% local dimensions in wheredim clauses
    HD :: [{dim, hidden_dim(), VarId}],
    %% HV is the list of hidden dimensions allocated for replacing
    %% formal parameters in abstractions
    HV :: [{phi, VarId}],
    VarId :: nonempty_string() | atom().

%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
transform1(Const, _P, _HD, _HV) when is_number(Const) orelse is_boolean(Const) ->
  Const;

transform1({string, Str}, _P, _HD, _HV) ->
  {string, Str};

transform1({char, Char}, _P, _HD, _HV) ->
  {char, Char};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
transform1({primop, F, Eis}, P, HD, HV) ->
  Ns = lists:seq(1, length(Eis)), %% 1,2,...
  NewEis =
    lists:map(
      fun({Ei, N}) ->
          transform1(Ei, subexpr_pos(N,P), HD, HV) %% Pos 1,2,...
      end,
      lists:zip(Eis, Ns)),
  {primop, F, NewEis};

%%------------------------------------------------------------------------------
%% Tuple Expression
%%------------------------------------------------------------------------------
transform1({t, E0E1is}, P, HD, HV) ->
  Ns = lists:seq(1, length(E0E1is)), %% 1,2,...
  NewE0E1is =
    lists:map(
      fun({{E0,E1}, N}) ->
          {transform1(E0, subexpr_pos(N*2  ,P), HD, HV), %% Pos 2,4,...
           transform1(E1, subexpr_pos(N*2+1,P), HD, HV)} %% Pos 3,5,...
      end,
      lists:zip(E0E1is, Ns)),
  {t, NewE0E1is};

%%------------------------------------------------------------------------------
%% Context Perturbation
%%------------------------------------------------------------------------------
transform1({'@', E0, E1}, P, HD, HV) ->
  {'@',
   transform1(E0, subexpr_pos(0,P), HD, HV),
   transform1(E1, subexpr_pos(1,P), HD, HV)};

%%------------------------------------------------------------------------------
%% Conditional
%%------------------------------------------------------------------------------
transform1({'if', E0, E1, E2}, P, HD, HV) ->
  {'if',
   transform1(E0, subexpr_pos(0,P), HD, HV),
   transform1(E1, subexpr_pos(1,P), HD, HV),
   transform1(E2, subexpr_pos(2,P), HD, HV)};

%%------------------------------------------------------------------------------
%% Dimensional Query
%%------------------------------------------------------------------------------
transform1({'#', E0}, P, HD, HV) ->
  %% Changing the position in the evaluation tree is not needed as:
  %% * There is only one subexpression
  %% * No hidden dimensions are created in the current expression
  {'#', transform1(E0, P, HD, HV)};

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
transform1({b_abs, Is, Params, E}, P, HD, HV) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  ParamsAsDims = lists:map(fun(Param) -> {phi, Param} end, Params),
  {b_abs, transform1_frozen_dims(Is, Ps, HD, HV),
   ParamsAsDims,
   transform1(E, subexpr_pos(0,P), HD, tset:union(HV, ParamsAsDims))};

transform1({b_apply, E0, Eis}, P, HD, HV) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Eis))),
  {b_apply, transform1(E0, subexpr_pos(0,P), HD, HV),
   transform1_actual_params(Eis, Ps, HD, HV)};

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
transform1({v_abs, Is, Params, E}, P, HD, HV) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  ParamsAsDims = lists:map(fun(Param) -> {phi, Param} end, Params),
  {v_abs, transform1_frozen_dims(Is, Ps, HD, HV),
   ParamsAsDims,
   %% XXX Pos 0?
   transform1(E, subexpr_pos(0,P), HD, tset:union(HV, ParamsAsDims))};

transform1({v_apply, E0, Eis}, P, HD, HV) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Eis))),
  {v_apply, transform1(E0, subexpr_pos(0,P), HD, HV),
   %% XXX Shouldn't another context application be here somewhere?
   transform1_actual_params(Eis, Ps, HD, HV)};

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
transform1({i_abs, Is, E}, P, HD, HV) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  {i_abs, transform1_frozen_dims(Is, Ps, HD, HV),
   %% XXX Pos 0?
   transform1(E, subexpr_pos(0,P), HD, HV)};

transform1({i_apply, E}, P, HD, HV) ->
  %% XXX Is subexpression with position really needed? And BTW -
  %% shouldn't another context application be here somewhere?
  {i_apply, transform1(E, subexpr_pos(0,P), HD, HV)};

%%------------------------------------------------------------------------------
%% Wherevar
%%------------------------------------------------------------------------------
transform1({wherevar, E0, XiEis}, P, HD, HV) ->
  Ns = lists:seq(1, length(XiEis)), %% 1,2,...
  NewXiEis =
    [{Xi, transform1(Ei, subexpr_pos(N,P), HD, HV)} %% Pos 1,2,...
     || {{Xi, Ei}, N} <- lists:zip(XiEis, Ns)],
  %% XXX The way position is assigned here is completely different
  %% from literature.
  {wherevar, transform1(E0, subexpr_pos(0,P), HD, HV),
   NewXiEis};

%%------------------------------------------------------------------------------
%% Wheredim
%%------------------------------------------------------------------------------
transform1({wheredim, E0, XiEis}, P, HD, HV) ->
  Ns = lists:seq(1, length(XiEis)),
  DimsEis =
    [{ {dim, hidden_dim(N,P), Xi},
       transform1(Ei, subexpr_pos(N,P), HD, HV) }
     || {{Xi, Ei}, N} <- lists:zip(XiEis, Ns)],
  Dims = [Dim || {Dim, _} <- DimsEis],
  {wheredim,
   transform1(E0, subexpr_pos(0,P),
              %% Deal with e.g. nested wheredims with same local dim id
              set_union_w_dim_shadowing(HD, Dims), HV),
   DimsEis};

%%-------------------------------------------------------------------------------------
%% Dimension Identifiers
%%-------------------------------------------------------------------------------------
transform1({dim, Xi}=Di, _P, HD, _HV) when is_list(Xi) orelse is_atom(Xi) ->
  case lists:keyfind(Xi, 3, HD) of
    {dim, _, Xi} = HDim ->
      %% Replace local dimension of wheredim clause with previously
      %% allocated hidden dimension
      HDim;
    false ->
      Di
  end;

%%-------------------------------------------------------------------------------------
%% Variable Identifiers
%%-------------------------------------------------------------------------------------
transform1(Xi, _P, _HD, HV) when is_list(Xi) orelse is_atom(Xi) ->
  case lists:keyfind(Xi, 2, HV) of
    {phi, Xi} = HDim ->
      %% Replace formal parameter of abstraction with context query of
      %% previously allocated hidden dimension
      {'?', HDim};
    false ->
      Xi
  end.


%%-------------------------------------------------------------------------------------
%% Internal - Helpers for transforming abstractions and applications
%%-------------------------------------------------------------------------------------
transform1_frozen_dims(Is, Ps, HD, HV) ->
  tset:union(
    tset:union(HD, HV),
    [transform1(I, P, HD, HV) || {I, P} <- lists:zip(Is, Ps)]).

transform1_actual_params(Eis, Ps, HD, HV) ->
  lists:map(fun({Ei, P}) -> transform1(Ei, P, HD, HV) end, lists:zip(Eis, Ps)).


%%-------------------------------------------------------------------------------------
%% Internal - Set helpers
%%-------------------------------------------------------------------------------------
set_union_w_dim_shadowing(DimSetToBeShadowed, DimSetShadowing) ->
  tset:union(
    lists:filter(
      fun({dim, _, Xi}) -> not lists:keymember(Xi, 3, DimSetShadowing) end,
      DimSetToBeShadowed),
    DimSetShadowing).


%%-------------------------------------------------------------------------------------
%% Internal - Deterministic generation of hidden dimensions using
%% position of expression in evaluation tree
%%-------------------------------------------------------------------------------------
-type n() :: non_neg_integer().
-type pos() :: [n()].

-type index() :: pos_integer().
-type hidden_dim() :: {pos(), index()}.

%%-------------------------------------------------------------------------------------
%% @doc Return position of the root expression in the evaluation tree.
%% @private
%%-------------------------------------------------------------------------------------
-spec root_expr_pos() -> RootP :: pos().
root_expr_pos() ->
  [].

%%-------------------------------------------------------------------------------------
%% @doc Return position of subexpression N while in position P.
%% @private
%%-------------------------------------------------------------------------------------
-spec subexpr_pos(N :: n(), P :: pos()) -> SubP :: pos().
subexpr_pos(N, P) ->
  [N | P].

%%-------------------------------------------------------------------------------------
%% @doc Return I-th hidden dimension in position P.
%% @private
%%-------------------------------------------------------------------------------------
-spec hidden_dim(I :: index(), P :: pos()) -> HD :: hidden_dim().
hidden_dim(I, P) ->
  {P, I}.


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
  transform1(R).


rules_test_() ->
  Consts = [46,
            false,
            {string,"ciao"}],
  ConstTests = lists:zip(Consts, Consts),
  WheredimTest = {WheredimTree, WheredimExpected} =
    { {where,    {'#',{dim,       "t"}}, [{ dim,       "t", 46}]},
      {wheredim, {'#',{dim,{[],1},"t"}}, [{{dim,{[],1},"t"},46}]} },
  WheredimTreeF =
    fun(DimName) when is_list(DimName) ->
        {where, {'#',{dim,DimName}}, [{dim,DimName,46}]}
    end,
  WheredimExpectedF =
    fun(DimName, Pos) when is_list(DimName), is_list(Pos) ->
        {wheredim, {'#',{dim,{Pos,1},DimName}}, [{{dim,{Pos,1},DimName},46}]}
    end,
  WheredimTree     = WheredimTreeF(    "t"),
  WheredimExpected = WheredimExpectedF("t", []),
  WherevarTest =
    { {where,    WheredimTree,
       [{var,"X",WheredimTree              },
        {var,"Y",WheredimTree              }]},
      {wherevar, WheredimExpectedF("t",[0]),
       [{    "X",WheredimExpectedF("t",[1])},
        {    "Y",WheredimExpectedF("t",[2])}]} },
  %% Testing expression in dimensional query, even if it does not make
  %% sense as dims are not ground values atm
  DimQueryTest =
    { {'#', WheredimTree    },
      {'#', WheredimExpected} },
  TupleTests =
    [{ {t, [{"lhs",46}]},
       {t, [{"lhs",46}]} },
     %% XXX How can lhs of tuple be expression if dims cannot be ground values atm? Not testing it.
     { {t, [{"lhs1",WheredimTree              },
            {"lhs2",WheredimTree              }]},
       {t, [{"lhs1",WheredimExpectedF("t",[3])},
            {"lhs2",WheredimExpectedF("t",[5])}]} }
    ],
  %% TODO (not important) test perturbation, primop, if-then-else
  TreeExpectedTuples =
    ConstTests ++ [WheredimTest, WherevarTest, DimQueryTest] ++ TupleTests,
  [?_test(begin
            AstAfterT0 = ttransform0:transform0(Tree),
            Actual = transform1(AstAfterT0),
            %io:format(user, "AstAfterT0: ~1000p~nActual: ~1000p~n", [AstAfterT0, Actual]),
            ?assertEqual(Expected, Actual)
          end)
   || {Tree, Expected} <- TreeExpectedTuples].


nested_wheredims_test() ->
  T =
    {where,
     {where,
      {'#',{dim,"t"}},
      [{dim,"t",58}]},
     [{dim,"t",46}]},
  R = ttransform0:transform0(T),
  Expected =
    {wheredim,
     {wheredim,
      {'#',{dim,{[0],1},"t"}},
      [ {{dim,{[0],1},"t"},58} ]},
     [ {{dim,{[],1},"t"},46} ]},
  ?assertEqual(Expected, transform1(R)).

var_id_in_wheredim_is_not_renamed_test() ->
  T =
    {where,
     "t",
     [{dim,"t",46},{var,"t",58}]},
  R = ttransform0:transform0(T),
  Expected =
    {wheredim,
     {wherevar,
      "t",
      [ {"t",58} ]},
     [ {{dim,{[],1},"t"},46} ]},
  ?assertEqual(Expected, transform1(R)).
