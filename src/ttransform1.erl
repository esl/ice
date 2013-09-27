%%------------------------------------------------------------------------------
%% AST Transformation - Pass 1
%%------------------------------------------------------------------------------
-module(ttransform1).

-include_lib("eunit/include/eunit.hrl").

-export([test/0]).

%%------------------------------------------------------------------------------
%% This transformation module transforms wheredim identifiers into hidden
%% dimensions (e.g: {dim, t} -> {dim, {Pos,Idx}, t}) and references to these
%% identifiers to references to the hidden dimensions (e.g: {dim, t} ->
%% {dim, {Pos,Idx}, t}).
%%
%% Transformed expressions are wheredim, wherevar, intension abstractions,
%% intension applications, base and value abstractions / applications.
%%------------------------------------------------------------------------------

transform1(E) ->
  transform1(E, root_expr_pos(), []).

%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
transform1(Const, _P, _H) when is_number(Const) orelse is_boolean(Const) ->
  Const;

transform1({string, Str}, _P, _H) ->
  {string, Str};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
transform1({primop, F, Eis}, P, H) ->
  Ns = lists:seq(1, length(Eis)), %% 1,2,...
  NewEis =
    lists:map(
      fun({Ei, N}) ->
          transform1(Ei, subexpr_pos(N,P), H) %% Pos 1,2,...
      end,
      lists:zip(Eis, Ns)),
  {primop, F, NewEis};

%%------------------------------------------------------------------------------
%% Tuple Expression
%%------------------------------------------------------------------------------
transform1({t, E0E1is}, P, H) ->
  Ns = lists:seq(1, length(E0E1is)), %% 1,2,...
  NewE0E1is =
    lists:map(
      fun({{E0,E1}, N}) ->
          {transform1(E0, subexpr_pos(N*2  ,P), H), %% Pos 2,4,...
           transform1(E1, subexpr_pos(N*2+1,P), H)} %% Pos 3,5,...
      end,
      lists:zip(E0E1is, Ns)),
  {t, NewE0E1is};

%%------------------------------------------------------------------------------
%% Context Perturbation
%%------------------------------------------------------------------------------
transform1({'@', E0, E1}, P, H) ->
  {'@',
   transform1(E0, subexpr_pos(0,P), H),
   transform1(E1, subexpr_pos(1,P), H)};

%%------------------------------------------------------------------------------
%% Conditional
%%------------------------------------------------------------------------------
transform1({'if', E0, E1, E2}, P, H) ->
  {'if',
   transform1(E0, subexpr_pos(0,P), H),
   transform1(E1, subexpr_pos(1,P), H),
   transform1(E2, subexpr_pos(2,P), H)};

%%------------------------------------------------------------------------------
%% Dimensional Query
%%------------------------------------------------------------------------------
transform1({'#', E0}, P, H) ->
  {'#', transform1(E0, P, H)};

%% %%------------------------------------------------------------------------------
%% %% Base Abstraction
%% %%------------------------------------------------------------------------------
%% %% TODO
%% transform1({b_abs, Is, Params, E, P}, H) ->
%%   Dims = [{dim, Param} || Param <- Params],
%%   %% Here we expect P to be a runtime assigned constant
%%   {b_abs, tset:union(H, [transform1(I, H) || I <- Is]),
%%    Dims,
%%    transform1(E, tset:union(H, Dims)),
%%    P};

%% transform1({b_apply, E0, E1}, H) ->
%%   {b_apply, transform1(E0, H), transform1(E1, H)};

%% %%------------------------------------------------------------------------------
%% %% Value Abstraction
%% %%------------------------------------------------------------------------------
%% %% TODO
%% transform1({v_abs, Is, Params, E}, H) ->
%%   Dims = [{dim, Param} || Param <- Params],
%%   {v_abs, tset:union(H, [transform1(I, H) || I <- Is]),
%%    Dims,
%%    transform1(E, tset:union(H, Dims))};

%% transform1({v_apply, E0, E1}, H) ->
%%   {v_apply, transform1(E0, H), transform1(E1, H)};

%% %%------------------------------------------------------------------------------
%% %% Intension Abstraction
%% %%------------------------------------------------------------------------------
%% transform1({i_abs, Is, E}, H) ->
%%   {i_abs, tset:union(H, [transform1(I, H) || I <- Is]),
%%    transform1(E, H)};

%% transform1({i_apply, E}, H) ->
%%   {i_apply, transform1(E, H)};

%%------------------------------------------------------------------------------
%% Wherevar
%%------------------------------------------------------------------------------
transform1({wherevar, E0, XiEis}, P, H) ->
  Ns = lists:seq(1, length(XiEis)), %% 1,2,...
  NewXiEis =
    [{Xi, transform1(Ei, subexpr_pos(N,P), H)} %% Pos 1,2,...
     || {{Xi, Ei}, N} <- lists:zip(XiEis, Ns)],
  {wherevar, transform1(E0, subexpr_pos(0,P), H),
   NewXiEis};

%%------------------------------------------------------------------------------
%% Wheredim
%%------------------------------------------------------------------------------
transform1({wheredim, E0, XiEis}, P, H) ->
  Ns = lists:seq(1, length(XiEis)),
  DimsEis =
    [{ {dim, hidden_dim(N,P), Xi},
       transform1(Ei, subexpr_pos(N,P), H) }
     || {{Xi, Ei}, N} <- lists:zip(XiEis, Ns)],
  Dims = [Dim || {Dim, _} <- DimsEis],
  HWoShadowedDims = %% e.g. nested wheredims with same local dim identifier
    lists:filter(fun({dim, _, Xi}) -> not lists:keymember(Xi, 3, Dims) end, H),
  NewE0 = transform1(E0, subexpr_pos(0,P), tset:union(HWoShadowedDims, Dims)),
  {wheredim, NewE0,
   DimsEis};

%%-------------------------------------------------------------------------------------
%% Dimension Identifiers
%%-------------------------------------------------------------------------------------
transform1({dim, Xi}=Di, _P, H) when is_list(Xi) orelse is_atom(Xi) ->
  case lists:keyfind(Xi, 3, H) of
    {dim, _, Xi} = DimXi ->
      DimXi;
    false ->
      Di
  end;

%%-------------------------------------------------------------------------------------
%% Variable Identifiers
%%-------------------------------------------------------------------------------------
transform1(Xi, _P, _H) when is_list(Xi) orelse is_atom(Xi) ->
  Xi.


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

wheredim_eval_test_() ->
  T =
    {where,
     {'#',{dim,"t"}},
     [{dim,"t",46}]},
  ExpectedT0 =
    {wheredim,
     {'#',{dim,"t"}},
     [{"t",46}]},
  T0 = ttransform0:transform0(T),
  ?assertEqual(ExpectedT0, T0),
  ExpectedT1 =
    {wheredim,
     {'#',{dim,{[],1},"t"}},
     [ {{dim,{[],1},"t"},46} ]},
  T1 = transform1(T0),
  ?assertEqual(ExpectedT1, T1),
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({46,_},
                  tcore:eval(T1, [],[], [], [], [0], 0))
   ]}.

where_eval_test_() ->
  T =
    {where,
     "X",
     [{dim,"t",46},
      {var,"X",{'#',{dim,"t"}}}]},
  ExpectedT0 =
    {wheredim,
     {wherevar,
      "X",
      [{"X",{'#',{dim,"t"}}}]},
     [{"t",46}]},
  T0 = ttransform0:transform0(T),
  ?assertEqual(ExpectedT0, T0),
  ExpectedT1 =
    {wheredim,
     {wherevar,
      "X",
      [{"X",{'#',{dim,{[],1},"t"}}}]},
     [{{dim,{[],1},"t"},46}]},
  T1 = transform1(T0),
  ?assertEqual(ExpectedT1, T1),
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({46,_},
                  tcore:eval(T1, [],[], [], [], [0], 0))
   ]}.

tcache_stop(Pid) ->
  catch tcache:stop(),
  case is_process_alive(Pid) of
    false ->
      ok;
    true ->
      tcache_stop(Pid)
  end.
