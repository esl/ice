%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(extra_tests).

%% Tests not strictly needed.
%%
%% If updating all the unit tests becomes a burden, delete this test
%% suite first.

-include_lib("eunit/include/eunit.hrl").


%% API tests.
transform1_rules_test_() ->
  Consts = [{int, 46},
            {bool, false},
            {string,"ciao"}],
  ConstTests = lists:zip(Consts, Consts),
  WheredimTest = {WheredimTree, WheredimExpected} =
    { {where,    {'#',            {id,"t"}}, [{ dim,       {id,"t"} ,{int,46}}]},
      {wheredim, {'#',{dim,{[],1},    "t"}}, [{{dim,{[],1},    "t" },{int,46}}]} },
  WheredimTreeF =
    fun(DimName) when is_list(DimName) ->
        {where,    {'#',             {id,DimName}}, [ {dim,        {id,DimName} ,{int,46}}]}
    end,
  WheredimExpectedF =
    fun(DimName, Pos) when is_list(DimName), is_list(Pos) ->
        {wheredim, {'#',{dim,{Pos,1},    DimName}}, [{{dim,{Pos,1},    DimName },{int,46}}]}
    end,
  WheredimTree     = WheredimTreeF(    "t"),
  WheredimExpected = WheredimExpectedF("t", []),
  WherevarTest =
    { {where,    WheredimTree,
       [{var,{id,"X"},WheredimTree              },
        {var,{id,"Y"},WheredimTree              }]},
      {wherevar, WheredimExpectedF("t",[0]),
       [{    {id,"X"},WheredimExpectedF("t",[1])},
        {    {id,"Y"},WheredimExpectedF("t",[2])}]} },
  %% Testing expression in dimensional query, even if it does not make
  %% sense as dims are not ground values atm
  DimQueryTest =
    { {'#', WheredimTree    },
      {'#', WheredimExpected} },
  TupleTests =
    [{ {t, [{{id,"lhs"},{int,46}}]},
       {t, [{{id,"lhs"},{int,46}}]} },
     { {t, [{{id,"lhs1"},WheredimTree              },
            {{id,"lhs2"},WheredimTree              }]},
       {t, [{{id,"lhs1"},WheredimExpectedF("t",[3])},
            {{id,"lhs2"},WheredimExpectedF("t",[5])}]} }
    ],
  %% TODO (not important) test perturbation, primop, if-then-else
  TreeExpectedTuples =
    ConstTests ++ [WheredimTest, WherevarTest, DimQueryTest] ++ TupleTests,
  [?_test(begin
            AstAfterT0 = t0(Tree),
            Actual = t1(AstAfterT0),
            %io:format(user, "AstAfterT0: ~1000p~nActual: ~1000p~n", [AstAfterT0, Actual]),
            ?assertEqual(Expected, Actual)
          end)
   || {Tree, Expected} <- TreeExpectedTuples].


%% Internals

t0(T) ->
  ice_t0:transform(T).

t1(T) ->
  ice_t1:transform(T).

%% End of Module.
