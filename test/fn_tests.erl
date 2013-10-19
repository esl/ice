%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(fn_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

b_test_() ->
  [
   basic_b_abs(),
   basic_b_apply(),
   b_abs_w_two_formal_params(),
   b_abs_nested_in_wheredim_does_not_cause_wrong_substitution(),
   wheredim_nested_in_b_abs_does_not_cause_wrong_substitution(),
   b_abs_can_return_b_abs_and_formal_params_are_not_confused(),
   b_abs_cannot_access_dims_not_in_creation_context(), %% E.g. formal params of outer b_abs
   b_abs_can_use_argument_for_querying_creation_context(),
   b_abs_can_use_argument_for_querying_creation_context2(),
   creation_of_b_abs_in_multiple_contexts_plays_nicely_w_cache(),
   toplevel_fun()
  ].

%% TODO: integration with parser for sequence of function declarations and calls

basic_b_abs() ->
  T = s("F where fun F.argAsVarId = argAsVarId end"),
  ?assertEqual({where, "F",
                [{fn, "F", [{b_param,"argAsVarId"}], "argAsVarId"}]},
               T),
  T0 = t0(T),
  ?assertEqual({wherevar, "F",
                [{"F", {b_abs, [], ["argAsVarId"], "argAsVarId"}}]},
               T0),
  T1 = t1(T0),
  ArgAsPhiDim = {phi,"argAsVarId"},
  ?assertEqual({wherevar, "F",
                [{"F", {b_abs, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}}]},
               T1),
  %% Eval
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    begin
      ExpectedResult = {frozen_b_abs, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}},
      ?_assertMatch({ExpectedResult,_}, tcore_eval(T1))
    end
   ]}.

basic_b_apply() ->
  S = "F.46 where fun F.argAsVarId = argAsVarId end",
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [ ?_assertMatch({46,_}, eval(S)) ]}.

b_abs_w_two_formal_params() ->
  S = "F.46.1 where fun F.x.y = x - y end", %% Minus is not commutative
  ?assertMatch({wherevar, {b_apply, "F", [46, 1]},
                [{"F", {b_abs, [], ["x", "y"],
                        {primop, _, ["x", "y"]}}}]},
               _BAbsT0 = t0(s(S))),
  %% Eval
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [ ?_assertMatch({45,_}, eval(S)) ]}.

b_abs_nested_in_wheredim_does_not_cause_wrong_substitution() ->
  S = "(F.1 where fun F.x = x + #.x end) where dim x <- 46 end",
  WheredimX = {dim,{[],1},"x"},
  BAbsX = {phi,"x"},
  ?assertMatch(
      {wheredim,
       {wherevar,
        {b_apply, "F", [1]},
        [{"F",
          {b_abs,
           %% "No wheredim clause can return an abstraction that
           %% varies in a local dimension identifier defined in that
           %% wheredim clause. To ensure that this is the case, if a
           %% local dimension identifier appears in the rank of the
           %% body of an abstraction, then that local dimension
           %% identifier must appear in the list of frozen dimensions
           %% for that abstraction."
           %%
           %% Ref: 14.1 "Assumptions" in paper "Multidimensional
           %% Infinite Data in the Language Lucid", Feb 2013
           [WheredimX],
           [BAbsX],
           {primop, _, [{'?',BAbsX},
                        {'#',WheredimX}]}
          }}]},
       [{WheredimX,46}]},
     t1(t0(s(S)))),
  %% Eval
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [ ?_assertMatch({47,_}, eval(S)) ]}.

wheredim_nested_in_b_abs_does_not_cause_wrong_substitution() ->
  T0 = t0(s("F.1 where fun F.x = (x + #.x) where dim x <- 46 end end")),
  WheredimX = {dim,{[0,1],1},"x"},
  BAbsX = {phi,"x"},
  T1 = t1(T0),
  ?assertMatch(
     {wherevar,
      {b_apply, "F", [1]},
      [{"F",
        {b_abs, [], [BAbsX],
         {wheredim,
          {primop, _, [{'?',BAbsX},
                       {'#',WheredimX}]},
          [{WheredimX,46}]}}
       }]},
     T1),
  %% Eval
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [ ?_assertMatch({47,_}, tcore_eval(T1)) ]}.

b_abs_can_return_b_abs_and_formal_params_are_not_confused() ->
  S =
    "(G.1).46.3
    where
      fun F.x.y = x - y
      fun G.x = F
    end",
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [ ?_assertMatch({43,_}, eval(S)) ]}.

b_abs_cannot_access_dims_not_in_creation_context() ->
  NestedS =
    "G.1
    where
      fun G.y = F
      where
        fun F.x = x - y
      end
    end",
  S =
    "(G.1).46
    where
      fun G.y = F
      where
        fun F.x = x - y
      end
    end",
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({{frozen_b_abs,[],[{phi,"x"}],{primop,_,_}},_},
                  eval(NestedS)),
    ?_assertMatch({[{phi,"y"}],_}, eval(S))
   ]}.

b_abs_can_use_argument_for_querying_creation_context() ->
  DimT = {dim,"t"},
  %% XXX How to create from the parser a base abstraction with frozen dims?
  BAbsT0 =
    {b_abs, [DimT], ["x"],
     %% In body, query dimension specified as formal parameter
     {'#',
      %% XXX Having "t" after '#' would not be possible using the
      %% parser ATM as expression "t" after context query "#." would
      %% be considered dimension identifier
      "x"}},
  %% Create an AST as if it were generated by transformation 0
  T0 =
    {wherevar,
     {b_apply,
      t0(s("F @ [t <- 46]")),
      %% XXX Passing dimensions as base parameters is not possible in
      %% the parser ATM. It should be possible only having ground
      %% values as dimensions.
      [DimT]},
     [{"F",BAbsT0}]},
  T1 = t1(T0),
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({46,_}, tcore_eval(T1)),
    ?_assertMatch({46,_}, tcore_eval(T1, _K=[{DimT,58}], _D=[DimT]))
   ]}.

b_abs_can_use_argument_for_querying_creation_context2() ->
  %% Same as test
  %% b_abs_can_use_argument_for_querying_creation_context, simply
  %% writing body of b_abs differently in order to ensure that rules
  %% play together
  DimT = {dim,"t"},
  BAbsT0 =
    {b_abs, [DimT], ["x"],
     %% Specify a more complex body, that should be equivalent to {'#',"x"}
     t0({where,
         {'#',
          %% XXX Nesting "#.(#.y)" cannot be expressed with current parser
          {'#',{dim,"y"}}},
         [{dim,"y","x"}]})},
  T0 =
    {wherevar,
     {b_apply,
      t0(s("F @ [t <- 46]")),
      [DimT]},
     [{"F",BAbsT0}]},
  T1 = t1(T0),
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({46,_}, tcore_eval(T1)),
    ?_assertMatch({46,_}, tcore_eval(T1, _K=[{DimT,58}], _D=[DimT]))
   ]}.

creation_of_b_abs_in_multiple_contexts_plays_nicely_w_cache() ->
  %% Test similar to
  %% b_abs_can_use_argument_for_querying_creation_context but with
  %% multiple creations of b_abs
  DimT = {dim,"t"},
  BAbsT0 = {b_abs, [DimT], ["x"], {'#',"x"}},
  T0 =
    {wherevar,
     tprimop:minus(
       {b_apply, t0(s("F @ [t <- 46]")), [DimT]},
       {b_apply, t0(s("F @ [t <-  1]")), [DimT]}),
     [{"F",BAbsT0}]},
  T1 = t1(T0),
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({45,_}, tcore_eval(T1)),
    ?_assertMatch({45,_}, tcore_eval(T1, _K=[{DimT,58}], _D=[DimT]))
   ]}.

toplevel_fun() ->
  E = t1(t0(s("square.3 where fun square.x = x * x end"))),
  {setup,
   _S = fun ()  -> {ok, P} = tcache:start_link(100), P end,
   _C = fun (P) -> tcache_stop(P) end,
   [ ?_assertMatch({9,_}, tcore_eval(E)) ]}.


phi_is_recognized_as_a_dim_test_() ->
  %% Check that hidden dimensions replacing formal parameters are
  %% represented as e.g. {phi,"x"}...
  %%
  %% HACK: Get the b_abs node without the wherevar
  BAbsT0 = t0(s("F where fun F.x = x end")),
  BAbsX = {phi,"x"},
  ?assertMatch({wherevar, _,
                [{_, {b_abs, _, [BAbsX], _}}]},
               t1(BAbsT0)),
  %% ... then check that such dimensions are treated as all other
  %% dimensions as far as missing dimensions are concerned.
  {setup,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({[BAbsX],_},
                  tcore_eval({'if',{'?',BAbsX},46,58}))
   ]}.


%% Internals

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

tcore_eval(T) ->
  tcore_eval(T, [], []).

tcore_eval(T, K, D) ->
  tcore:eval(T,[],[],K,D,{[],self()},0).

eval(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T).

%% End of Module.
