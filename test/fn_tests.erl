%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(fn_tests).

%% Tests for functions not fitting in other test suites.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

phi_is_recognized_as_a_dim_test_() ->
  %% Check that hidden dimensions replacing formal parameters are
  %% represented as e.g. {phi,"x"}...
  BAbsS = "F where fun F.x = x end",
  BAbsX = {phi,"x"},
  ?assertMatch({wherevar, _,
                [{_, {b_abs, _, [BAbsX], _}}]},
               t1(t0(s(BAbsS)))),
  %% ... then check that such dimensions are treated in evaluator as
  %% all other dimensions as far as missing dimensions are concerned.
  {setup, fun setup/0, fun cleanup/1,
   ?_assertMatch({[BAbsX],_}, tcore_eval({'if',{'?',BAbsX},46,58}))}.

b_test_() ->
  [
   basic_b_abs(),
   toplevel_base_fun(),
   %%
   b_fun_w_two_formal_params_is_represented_as_one_b_abs_and_b_apply(),
   %%
   %% TODO reconsider b_abs_nested_in_wheredim_does_not_cause_wrong_substitution(),
   %% TODO reconsider wheredim_nested_in_b_abs_does_not_cause_wrong_substitution(),
   b_abs_can_return_b_abs_and_formal_params_w_same_name_are_not_confused(),
   b_abs_can_access_formal_params_of_outer_b_abs(),
   b_abs_can_access_local_dims_of_outer_wheredim(), %% ... differently from upstream TL
   b_abs_can_access_formal_params_of_outer_b_abs_and_local_dims_of_outer_wheredim(),
   b_abs_can_use_argument_for_querying_creation_context(),
   b_abs_can_use_argument_for_querying_creation_context2(),
   b_abs_cannot_access_dims_in_application_context(),
   %%
   creation_of_b_abs_in_multiple_contexts_plays_nicely_w_cache()
  ].

v_test_() ->
  [
   v_fun_w_two_formal_params_is_represented_as_nested_v_abs_and_v_apply(),
   %%
   v_abs_can_access_dims_in_application_context()
  ].

fn_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({-5,_}, eval("F.1!2.4!8
                               where
                                 fun F.b1!v1.b2!v2 = b1 - v1 + b2 - v2
                               end"))
   ]}.

dims_frozen_in_abs_by_transform1_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       {45,_}, %% BTW upstream TL returns spundef.
       %% Value is 45 as the t dim (of the outer wheredim) ends up in
       %% the frozen dims of F, and that t has value 1 in the context
       %% where F is evaluated.
       eval("X where var X = (F where fun F.x = x - #.t              end).46 @ [t <- 1];; dim t <- 0 end")),
    ?_assertMatch(
       {45,_},
       eval("X where var X = (F where fun F!x = x - #.t              end)!46 @ [t <- 1];; dim t <- 0 end")),
    %%
    ?_assertMatch(
       {43,_},
       %% Return value is 43 and not 45 as references to local
       %% dimensions defined in wheredim clauses have lexical scoping
       %% in the current implementation (similarly to the Feb 2013
       %% cache semantics paper), not dynamic scoping (as in the Aug
       %% 2012 semantics paper).
       %%
       %% BTW upstream TL returns spundef.
       eval("X where var X = (F where fun F.x = x - #.t;; dim t <- 3 end).46 @ [t <- 1];; dim t <- 0 end")),
    ?_assertMatch(
       {43,_},
       %% Return value is 43 and not 45 as references to local
       %% dimensions defined in wheredim clauses have lexical scoping
       %% in the current implementation (similarly to the Feb 2013
       %% cache semantics paper), not dynamic scoping (as in the Aug
       %% 2012 semantics paper).
       %%
       %% BTW upstream TL returns spundef.
       eval("X where var X = (F where fun F!x = x - #.t;; dim t <- 3 end)!46 @ [t <- 1];; dim t <- 0 end")),
    %%
    %%
    ?_assertMatch(
       {45,_}, %% BTW upstream TL returns spundef.
       eval("X where var X = (F.46 where fun F.x = x - #.t              end) @ [t <- 1];; dim t <- 0 end")),
    ?_assertMatch(
       {45,_},
       eval("X where var X = (F!46 where fun F!x = x - #.t              end) @ [t <- 1];; dim t <- 0 end")),
    %%
    ?_assertMatch(
       {43,_}, %% BTW upstream TL returns spundef.
       eval("X where var X = (F.46 where fun F.x = x - #.t;; dim t <- 3 end) @ [t <- 1];; dim t <- 0 end")),
    ?_assertMatch(
       {43,_},
       eval("X where var X = (F!46 where fun F!x = x - #.t;; dim t <- 3 end) @ [t <- 1];; dim t <- 0 end")),
    %%
    %%
    ?_assertMatch(
       {45,_}, %% BTW upstream TL returns spundef.
       eval("X where var X = (F.46 @ [t <- 1] where fun F.x = x - #.t              end);; dim t <- 0 end")),
    ?_assertMatch(
       {45,_},
       eval("X where var X = (F!46 @ [t <- 1] where fun F!x = x - #.t              end);; dim t <- 0 end")),
    %%
    ?_assertMatch(
       {45,_}, %% BTW upstream TL returns spundef.
       eval("X where var X = (F.46 @ [t <- 1] where fun F.x = x - #.t;; dim t <- 3 end);; dim t <- 0 end")),
    ?_assertMatch(
       {45,_},
       eval("X where var X = (F!46 @ [t <- 1] where fun F!x = x - #.t;; dim t <- 3 end);; dim t <- 0 end"))
   ]}.


basic_b_abs() ->
  S = "F where fun F.argAsVarId = argAsVarId end",
  ?assertEqual({where, "F",
                [{fn, "F", [{b_param,"argAsVarId"}], "argAsVarId"}]},
               s(S)),
  ?assertEqual({wherevar, "F",
                [{"F", {b_abs, [], ["argAsVarId"], "argAsVarId"}}]},
               t0(s(S))),
  ArgAsPhiDim = {phi,"argAsVarId"},
  ?assertEqual({wherevar, "F",
                [{"F", {b_abs, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}}]},
               t1(t0(s(S)))),
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       { {frozen_closed_b_abs, _I, _E, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}, _},
       eval(S))
   ]}.

toplevel_base_fun() ->
  S = "square.3 where fun square.x = x * x end",
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({9,_}, eval(S)) ]}.

b_fun_w_two_formal_params_is_represented_as_one_b_abs_and_b_apply() ->
  S = "F.46.1 where fun F.x.y = x - y end", %% Minus is not commutative
  ?assertMatch({wherevar, {b_apply, "F", [46, 1]},
                [{"F", {b_abs, [], ["x", "y"],
                        {primop, _, ["x", "y"]}}}]},
               _BAbsT0 = t0(s(S))),
  {foreach, fun setup/0, fun cleanup/1,
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
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({47,_}, eval(S)) ]}.

wheredim_nested_in_b_abs_does_not_cause_wrong_substitution() ->
  S = "F.1 where fun F.x = (x + #.x) where dim x <- 46 end end",
  WheredimX = {dim,{[0,1],1},"x"},
  BAbsX = {phi,"x"},
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
     t1(t0(s(S)))),
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({47,_}, eval(S)) ]}.

b_abs_can_return_b_abs_and_formal_params_w_same_name_are_not_confused() ->
  S =
    "(G.1).46.3
    where
      fun F.x.y = x - y
      fun G.x = F
    end",
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({43,_}, eval(S)) ]}.

b_abs_can_access_formal_params_of_outer_b_abs() ->
  S =
    "(G.1).46
    where
      fun G.y = F
      where
        fun F.x = x - y
      end
    end",
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({45,_}, eval(S)) ]}.

b_abs_can_access_local_dims_of_outer_wheredim() ->
  S =
    "F.46
    where
      dim t <- 3
      fun F.x = x + #.t
    end",
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({49,_}, eval(S)) ]}.

b_abs_can_access_formal_params_of_outer_b_abs_and_local_dims_of_outer_wheredim() ->
  S =
    "(G.1).46
    where
      dim t <- 3
      fun G.y = F
      where
        fun F.x = x - y + #.t
      end
    end",
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({48,_}, eval(S)) ]}.

b_abs_can_use_argument_for_querying_creation_context() ->
  S =
    "(F @ [t <- 46]).t
    where
      fun F.x = #.x
      dim t <- 0
    end",
  %% Ensure that dimension t is among the frozen dimensions of function F
  {wheredim,
   {wherevar, _,
    [ {_, {b_abs,[DimT],[_],_}} ]},
   [ {DimT,0} ]} =
    t1(t0(s(S))),
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({46,_}, eval(S)) %% BTW upstream TL returns spdim
   ]}.

b_abs_can_use_argument_for_querying_creation_context2() ->
  %% Same as test
  %% b_abs_can_use_argument_for_querying_creation_context, simply
  %% writing body of base function differently in order to ensure that
  %% rules play together
  S =
    "(F @ [t <- 46]).t
    where
      fun F.x = #.(#.y) where dim y <- x end // Equivalent to 'fun F.x = #.x'
      dim t <- 0
    end",
  %% Ensure that dimension t is among the frozen dimensions of function F
  {wheredim,
   {wherevar, _,
    [ {_, {b_abs,[DimT],[_],{wheredim,_,_}}} ]},
   [ {DimT,0} ]} =
    t1(t0(s(S))),
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({46,_}, eval(S)) %% BTW upstream TL returns spdim
   ]}.

b_abs_cannot_access_dims_in_application_context() ->
  S = "(F.t where dim t <- 0 end) where fun F.x = #.x end",
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({[{dim,_,"t"}],_}, eval(S)) %% BTW upstream TL returns spdim
   ]}.

creation_of_b_abs_in_multiple_contexts_plays_nicely_w_cache() ->
  %% Test similar to
  %% b_abs_can_use_argument_for_querying_creation_context but with
  %% multiple creations of b_abs
  S =
    "(F @ [t <- 46]).t - (F @ [t <- 1]).t
    where
      fun F.x = #.x
      dim t <- 0
    end",
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({45,_}, eval(S)) %% BTW upstream TL returns spundef
   ]}.


v_fun_w_two_formal_params_is_represented_as_nested_v_abs_and_v_apply() ->
  S = "F!46!1 where fun F!x!y = x - y end", %% Minus is not commutative
  ?assertMatch({wherevar,
                {v_apply,
                 {v_apply, "F", [46]},
                 [1]},
                [{"F", {v_abs, [], ["x"],
                        {v_abs, [], ["y"],
                         {primop, _, ["x", "y"]}}}}]},
               t0(s(S))),
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({45,_}, eval(S)) ]}.

v_abs_can_access_dims_in_application_context() ->
  S = "(F!t where dim t <- 46 end) where fun F!x = #.x end",
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({46,_}, eval(S)) %% BTW upstream TL returns 46
   ]}.


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

tcore_eval(T) ->
  tcore_eval(T, [], []).

tcore_eval(T, K, D) ->
  tcore:eval(T,[],[],K,D,{[],self()},0).

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T);
eval(T) ->
  tea:eval(T).

%% End of Module.
