%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(id_tests).

%% Tests for relationship between dim ids, var ids and function parameters.

-include_lib("eunit/include/eunit.hrl").

-define(_testMockingTpar(Expr),
        {setup, fun mock_tpar/0, fun(_) -> unmock_tpar() end,
         ?_test(Expr)}).


%% API tests.

regression_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({46,_}, eval("#.d where dim d <- 46 end")),
    ?_assertMatch({{te,[{{dim,_,"d"},46}]},_},
                  eval("[d <- 46] where dim d <- 0 end")),
    ?_assertMatch({46,_}, eval("#.d @ [d <- 46] where dim d <- 0 end")),
    %%
    ?_assertMatch({46,_}, eval("#.d where dim d <- 46;; var A =  58 end")),
    ?_assertMatch({46,_}, eval("#.d where var A =  58;; dim d <- 46 end")),
    %%
    ?_assertMatch({46,_},
                  eval("(#.d where var A =  58 end) where dim d <- 46 end")),
    ?_assertMatch({46,_},
                  eval("(#.d where dim d <- 46 end) where var A =  58 end"))
   ]}.

dim_id_must_be_declared_before_being_queried_test_() ->
  UndefIdD = {badmatch, {error, undefined_identifier, "d"}},
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertError(UndefIdD, eval("#.d")), %% Upstream TL returns spdim
    ?_testMockingTpar(
       ?assertError(
          UndefIdD, eval("[d <- 0]"))), %% Upstream TL returns spundef
    ?_testMockingTpar(
       ?assertError(
          UndefIdD, eval("#.d @ [d <- 0]"))), %% Upstream TL returns spundef
    %%
    ?_assertMatch(
       {[0],_}, %% Upstream TL returns spdim
       eval("#.d where var d = 0 end")),
    ?_assertMatch(
       {[{string,"t"}],_}, %% Upstream TL returns spdim
       eval("#.d where var d = `t`;; dim t <- 0 end")),
    %%
    ?_assertMatch(
       {1,_}, %% Upstream TL returns 1
       %% According to the Feb 2013 cache semantics paper, '1' is the
       %% correct result as:
       %% * Wherevar clause defines id "d" as 0;
       %% * Context perturbation '@' evaluates "d" to 0, and defines
       %%   dimension 0 (!) to be equal to 1;
       %% * Context query '#' evaluates "d" to 0, and queries
       %%   dimension 0, obtaining 1 as ordinate.
       %%
       %% XXX Considering that, in order to simplify the
       %% implementation of the C evaluator, dimensions may be
       %% statically substituted with integers, the possibility that
       %% an integer (or a char or a string) is used as a dimension is
       %% dangerous as it could introduce the possibility to refer to
       %% a dimension in two different ways: (1) its id (i.e. the
       %% correct way) and (2) a variable that refers to an integer
       %% that happens to be the same used for representing the
       %% dimension.
       %%
       %% TODO Find a solution without making logic on the internal
       %% representation of dimensions (i.e. tuple having phi or dim
       %% as first element), potentially diverging from the semantics
       %% papers (e.g. Feb 2013 cache semantics paper), and flexible
       %% enough for being able to sustain potential modifications to
       %% rule (9) in the Feb 2013 cache semantics paper.  A potential
       %% solution is changing the rule evaluating tuple expressions
       %% so that the lhs of each tuple element must be a dimension
       %% present in the domain of the context (but not necessarily in
       %% the set of known dimensions); this solution is resilient to
       %% the potential change in rule 9 that would pass to rule 9 not
       %% the context but the context restricted to the set of known
       %% dimensions.
       eval("#.d @ [d <- 1] where var d = 0 end"))
   ]}.

dim_id_is_ground_value_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(dim_id_can_be_assigned_to_var()),
    ?_test(dim_id_can_be_passed_as_actual_param())
   ]}.

id_cannot_be_defined_twice_in_same_scope_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       {1,_}, %% TODO This should give a compilation error "Error, dim t already defined."
       eval("#.t where dim t <- 0;; dim t <- 1 end")),
    ?_assertMatch(
       {1,_}, %% TODO This should give a compilation error "Error, var A already defined."
       eval("A where var A = 0;; var A = 1 end")),
    ?_assertMatch(
       {2,_}, %% TODO This should give a compilation error "Error, param x already defined."
       eval("F.2.1 where fun F.x.x = x*2 - x end"))
   ]}.

lexical_scoping_between_dim_id_and_var_id_test_() ->
  [
   wherevar_nested_in_wheredim(),
   wherevar_implicitly_nested_in_wheredim(),
   wheredim_nested_in_wherevar(),
   %%
   wheredim_nested_in_wheredim(),
   wherevar_nested_in_wherevar()
  ].

lexical_scoping_between_dim_id_and_formal_param_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("(F.46 where fun F.x = x       end) where dim x <- 1 end")),
    ?_assertMatch(
       {[46],_}, %% Upstream TL returns spdim
       eval("(F.46 where fun F.x =     #.x end) where dim x <- 1 end")),
    ?_testMockingTpar(
       ?assertError(
          badarith, %% Upstream TL returns spundef %% TODO Improve semantics, as tpar:eval in evaluation of primop returns [46, [46]], but [46] is not recognized as a dimension by tset:is_d
          eval("(F.46 where fun F.x = x + #.x end) where dim x <- 1 end"))),
    %%
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("F.1 where fun F.x =      #.x  where dim x <- 46 end end")),
    ?_assertMatch(
       {{dim,_,"x"},_}, %% Upstream TL returns 'unknown typename"I don\'t know how to print this type"' %% TODO Consider masking as maybe dimension shall not be returned
       eval("F.1 where fun F.x =  x        where dim x <- 46 end end")),
    ?_testMockingTpar(
       ?assertError(
          badarith, %% Upstream TL returns spundef %% TODO Improve semantics, as tpar:eval in evaluation of primop returns [46, [46]], but [46] is not recognized as a dimension by tset:is_d
          eval("F.1 where fun F.x = (x + #.x) where dim x <- 46 end end")))
   ]}.

complex_ids_test_() ->
  S =
    "#.x + F.2 // x here is the dim declared just below
    where
      dim x <- 1
      fun F.x =
        x + // x here is the formal param of F
        (a + b
        where
          var a = x // x here is the var declared just below
          var x = 4
          var b =
            #.x + G.16 // x here is the dim declared just below
            where
              dim x <- 8
              fun G.x = x // x here is the formal param of F
            end
        end)
    end",
  {setup, fun setup/0, fun cleanup/1,
   ?_assertMatch({31,_}, eval(S))}. %% Upstream TL returns 31

transform1_rules_test_() ->
  Consts = [46,
            false,
            {string,"ciao"}],
  ConstTests = lists:zip(Consts, Consts),
  WheredimTest = {WheredimTree, WheredimExpected} =
    { {where,    {'#',            "t" }, [{ dim,       "t", 46}]},
      {wheredim, {'#',{dim,{[],1},"t"}}, [{{dim,{[],1},"t"},46}]} },
  WheredimTreeF =
    fun(DimName) when is_list(DimName) ->
        {where,    {'#',             DimName }, [ {dim,        DimName,46}]}
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
     { {t, [{"lhs1",WheredimTree              },
            {"lhs2",WheredimTree              }]},
       {t, [{"lhs1",WheredimExpectedF("t",[3])},
            {"lhs2",WheredimExpectedF("t",[5])}]} }
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


dim_id_can_be_assigned_to_var() ->
  ?assertMatch({46,_}, eval("#.A where var A = d;; dim d <- 46 end")).

dim_id_can_be_passed_as_actual_param() ->
  S = "A
      where
        var A = B where var B = F!d;; dim d <- 46 end
        fun F!x = #.x
      end",
  ?assertMatch({46,_}, eval(S)).

wherevar_nested_in_wheredim() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       {[46],_}, %% Upstream TL returns spdim
       eval("(#.a where var a =              46 end) where dim a <- 58 end")),
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("(#.a where var a = b;; dim b <- 46 end) where dim a <- 58 end")),
    %%
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("(X where var X = a;; var a = 46 end) where dim a <- 58 end"))
   ]}.

wherevar_implicitly_nested_in_wheredim() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("#.a where var a = b;; dim b <- 46;; dim a <- 58 end")),
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("#.a where dim a <- 58;; var a = b;; dim b <- 46 end")),
    %%
    ?_assertMatch(
       {[46],_}, %% Upstream TL returns spdim
       eval("#.a where var a =  46;; dim a <- 58 end")),
    ?_assertMatch(
       {[46],_}, %% Upstream TL returns spdim
       eval("#.a where dim a <- 58;; var a =  46 end"))
   ]}.

wheredim_nested_in_wherevar() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("(#.a where dim a <- 46 end) where var a =              58 end")),
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("(#.a where dim a <- 46 end) where var a = b;; dim b <- 58 end")),
    %%
    ?_assertMatch(
       {46,_}, %% Upstream TL returns 46
       eval("(#.a where dim a <- a end) where var a = 46 end"))
   ]}.

wheredim_nested_in_wheredim() ->
  S = "(#.d where dim d <- 46 end) where dim d <- 58 end",
  [
   {setup, fun setup/0, fun cleanup/1,
    ?_assertMatch({46,_}, eval(S))}, %% Upstream TL returns 46
   ?_assertMatch(
      {wheredim,
       {wheredim, _,
        [{InnerDimD,46}]},
       [{OuterDimD,58}]} when InnerDimD /= OuterDimD,
      t1(t0(s(S))))
  ].

wherevar_nested_in_wherevar() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(var_redefined_in_nested_wherevar_hangs_cache()),
    ?_test(var_redefined_in_nested_wherevar_hangs_cache2()),
    ?_test(var_redefined_in_nested_wherevar_hangs_cache3()),
    %%
    ?_test(var_redefined_in_nested_wherevar_is_shadowed_by_outer_if_outer_already_queried())
   ]}.

var_redefined_in_nested_wherevar_hangs_cache() ->
  S = "A
      where
        var B = C
        where
          var A = 46 // Inner A
          var C = A
        end
        var A = B // Outer A
      end",
  ?assertError({badmatch, hang}, eval(S)). %% Upstream TL returns 46 XXX Probably same var name in different wherevar clauses shall belong to distinct namespaces, therefore being different variables

var_redefined_in_nested_wherevar_hangs_cache2() ->
  S = "A
      where
        var B = C
        where
          var A = 46 // Inner A
          var C = D
          where
            var D = A
          end
        end
        var A = B // Outer A
      end",
  ?assertError({badmatch, hang}, eval(S)). %% Upstream TL returns 46 %% XXX Probably same var name in different wherevar clauses shall belong to distinct namespaces, therefore being different variables

var_redefined_in_nested_wherevar_hangs_cache3() ->
  S = "X
      where
        var X = A
        where
          var B = C
          where
            var A = 46 // Inner A
            var C = D
            where
              var D = A
            end
          end
          var A = B // Outer A
        end
      end",
  ?assertError({badmatch, hang}, eval(S)). %% Upstream TL returns 46 %% XXX Probably same var name in different wherevar clauses shall belong to distinct namespaces, therefore being different variables

var_redefined_in_nested_wherevar_is_shadowed_by_outer_if_outer_already_queried() ->
  S = "A
      where
        var B = 58
        var C =
          if B == 58 then // Force evaluation of B
            D
            where
              var B = 46 // Programmer intends to shadow outer B
              var D = B
            end
          else
            1
          fi
        var A = C
      end",
  ?assertMatch({58,_}, eval(S)). %% Upstream TL returns 46 %% XXX This is really confusing for the developer


%% Internals - Mocking

mock_tpar() ->
  ok = meck:new(tpar, [passthrough]),
  ok = meck:expect(tpar, eval,
                   fun(Xs, I, E, K, D, W, T) ->
                       tpar:eval_seq(Xs, I, E, K, D, W, T)
                   end).

unmock_tpar() ->
  ok = meck:unload(tpar).

%% Internals

setup() ->
  {ok, Pid} = tcache:start_link(100),
  Pid.

cleanup(Pid) ->
  tcache_stop(Pid).

tcache_stop(Pid) ->
  catch tcache:stop(),
  case is_process_alive(Pid) of
    false ->
      ok;
    true ->
      tcache_stop(Pid)
  end.

s(S) ->
  {ok, T} = tea:string(S),
  T.

t0(T) ->
  ttransform0:transform0(T).

t1(T) ->
  ttransform1:transform1(T).

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T).

%% End of Module.
