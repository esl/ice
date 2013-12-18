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
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(basic_b_abs()),
    ?_test(toplevel_base_fun()),
    %%
    ?_test(b_fun_w_two_formal_params_is_represented_as_one_b_abs_and_b_apply()),
    %%
    ?_test(b_abs_can_return_b_abs_and_formal_params_w_same_name_are_not_confused()),
    %%
    ?_test(b_abs_can_access_formal_params_of_outer_b_abs()),
    ?_test(b_abs_cannot_access_local_dims_of_outer_wheredim()),
    %%
    ?_test(b_abs_cannot_use_argument_for_querying_creation_context()),
    %%
    ?_test(b_abs_cannot_access_dims_in_application_context())
   ]}.

v_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(v_fun_w_two_formal_params_is_represented_as_nested_v_abs_and_v_apply()),
    %%
    ?_test(v_abs_can_access_dims_in_application_context())
   ]}.

n_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(n_fun_is_represented_as_v_fun()),
    ?_test(n_fun_w_two_formal_params_is_represented_as_v_fun()),
    %%
    ?_test(n_fun_is_represented_as_v_fun_complex1()),
    ?_test(n_fun_is_represented_as_v_fun_complex2()),
    %%
    ?_test(n_abs_can_access_dims_in_application_context())
   ]}.

dims_frozen_in_abs_by_transform1_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertError(
       {badmatch,
        {error, loop_detected,
         {already_known_dimensions, [{dim,_,"t"}]}}}, %% BTW upstream TL returns spundef.
       eval("X where var X = (F where fun F.x = x - #.t              end).46 @ [t <- 1];; dim t <- 0 end")),
    ?_assertMatch(
       {45,_},
       eval("X where var X = (F where fun F!x = x - #.t              end)!46 @ [t <- 1];; dim t <- 0 end")),
    %%
    ?_assertMatch(
       {[{dim,_,"t"}],_},
       eval("X where var X = (F where fun F.x = x - #.t;; dim t <- 3 end).46 @ [t <- 1];; dim t <- 0 end")),
    ?_assertMatch(
       {[{dim,_,"t"}],_},
       eval("X where var X = (F where fun F!x = x - #.t;; dim t <- 3 end)!46 @ [t <- 1];; dim t <- 0 end")),
    %%
    %%
    ?_assertError(
       {badmatch,
        {error, loop_detected,
         {already_known_dimensions, [{dim,_,"t"}]}}}, %% BTW upstream TL returns spundef.
       eval("X where var X = (F.46 where fun F.x = x - #.t              end) @ [t <- 1];; dim t <- 0 end")),
    ?_assertMatch(
       {45,_},
       eval("X where var X = (F!46 where fun F!x = x - #.t              end) @ [t <- 1];; dim t <- 0 end")),
    %%
    ?_assertMatch(
       {[{dim,_,"t"}],_},
       eval("X where var X = (F.46 where fun F.x = x - #.t;; dim t <- 3 end) @ [t <- 1];; dim t <- 0 end")),
    ?_assertMatch(
       {43,_},
       eval("X where var X = (F!46 where fun F!x = x - #.t;; dim t <- 3 end) @ [t <- 1];; dim t <- 0 end")),
    %%
    %%
    ?_assertError(
       {badmatch,
        {error, loop_detected,
         {already_known_dimensions, [{dim,_,"t"}]}}}, %% BTW upstream TL returns spundef.
       eval("X where var X = (F.46 @ [t <- 1] where fun F.x = x - #.t              end);; dim t <- 0 end")),
    ?_assertMatch(
       {45,_},
       eval("X where var X = (F!46 @ [t <- 1] where fun F!x = x - #.t              end);; dim t <- 0 end")),
    %%
    ?_assertMatch(
       {[{dim,_,"t"}],_},
       eval("X where var X = (F.46 @ [t <- 1] where fun F.x = x - #.t;; dim t <- 3 end);; dim t <- 0 end")),
    ?_assertMatch(
       {45,_},
       eval("X where var X = (F!46 @ [t <- 1] where fun F!x = x - #.t;; dim t <- 3 end);; dim t <- 0 end"))
   ]}.


basic_b_abs() ->
  S = "F where fun F.argAsVarId = argAsVarId end",
  ?assertEqual({where, "F",
                [{var, "F", {fn, [], [{b_param,"argAsVarId"}], "argAsVarId"}}]},
               s(S)),
  ?assertEqual({wherevar, "F",
                [{"F", {b_abs, [], ["argAsVarId"], "argAsVarId"}}]},
               t0(s(S))),
  ArgAsPhiDim = {phi,"argAsVarId"},
  ?assertEqual({wherevar, "F",
                [{"F", {b_abs, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}}]},
               t1(t0(s(S)))),
  ?assertMatch(
     { {frozen_closed_b_abs, _I, _E, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}, _},
     eval(S)).

toplevel_base_fun() ->
  S = "square.3 where fun square.x = x * x end",
  ?assertMatch({9,_}, eval(S)).

b_fun_w_two_formal_params_is_represented_as_one_b_abs_and_b_apply() ->
  S = "F.46.1 where fun F.x.y = x - y end", %% Minus is not commutative
  ?assertMatch({wherevar, {b_apply, "F", [46, 1]},
                [{"F", {b_abs, [], ["x", "y"],
                        {primop, _, ["x", "y"]}}}]},
               _BAbsT0 = t0(s(S))),
  ?assertMatch({45,_}, eval(S)).

b_abs_can_return_b_abs_and_formal_params_w_same_name_are_not_confused() ->
  S =
    "(G.1).46.3
    where
      fun F.x.y = x - y
      fun G.x = F
    end",
  ?assertMatch({43,_}, eval(S)).

b_abs_can_access_formal_params_of_outer_b_abs() ->
  S =
    "(G.1).46
    where
      fun G.y = F
      where
        fun F.x = x - y
      end
    end",
   ?assertMatch({45,_}, eval(S)).

b_abs_cannot_access_local_dims_of_outer_wheredim() ->
  S =
    "F.46
    where
      dim t <- 3
      fun F.x = x + #.t
    end",
  ?assertMatch({[{dim,_,"t"}],_}, eval(S)).

b_abs_cannot_use_argument_for_querying_creation_context() ->
  S =
    "(F @ [t <- 46]).t
    where
      fun F.x = #.x
      dim t <- 0
    end",
  ?assertMatch({[{dim,_,"t"}],_}, eval(S)).

b_abs_cannot_access_dims_in_application_context() ->
  S = "(F.t where dim t <- 0 end) where fun F.x = #.x end",
  ?assertMatch({[{dim,_,"t"}],_}, eval(S)). %% BTW upstream TL returns spdim

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
  ?assertMatch({45,_}, eval(S)).

v_abs_can_access_dims_in_application_context() ->
  S = "(F!t where dim t <- 46 end) where fun F!x = #.x end",
  ?assertMatch({46,_}, eval(S)). %% BTW upstream TL returns 46

n_fun_is_represented_as_v_fun() ->
  SN = "F      46  where fun F x =  x end",
  SV = "F!(↑{} 46) where fun F!x = ↓x end",
  ?assertMatch({wherevar,
                {v_apply, "F", [{i_abs, [], 46}]},
                [{"F", {v_abs, [], ["x"],
                        {i_apply, "x"}}}]},
               t0(s(SN))),
  ?assertEqual(t0(s(SN)), t0(s(SV))),
  ?assertMatch({46,_}, eval(SN)).

n_fun_w_two_formal_params_is_represented_as_v_fun() ->
  SN = "F     46       1  where fun F x y =  x -  y end",
  SV = "F!(↑{}46)!(↑{} 1) where fun F!x!y = ↓x - ↓y end",
  ?assertEqual(t0(s(SN)), t0(s(SV))).

n_fun_is_represented_as_v_fun_complex1() ->
  SN = "B @ [t <- 2] where var A = #.t;; var B = next.t     A ;; dim t <- 0;; fun next.d X =   X  @ [d <- #.d + 1];; end;;",
  SV = "B @ [t <- 2] where var A = #.t;; var B = next.t!(↑{}A);; dim t <- 0;; fun next.d!X = (↓X) @ [d <- #.d + 1];; end;;",
  ?assertEqual(t0(s(SN)), t0(s(SV))).

n_fun_is_represented_as_v_fun_complex2() ->
  SN = "(B @ [t <- 2] where var A = #.t;; var B = next.t     A ;; dim t <- 0;; end) where fun next.d X =   X  @ [d <- #.d + 1];; end;;",
  SV = "(B @ [t <- 2] where var A = #.t;; var B = next.t!(↑{}A);; dim t <- 0;; end) where fun next.d!X = (↓X) @ [d <- #.d + 1];; end;;",
  ?assertEqual(t0(s(SN)), t0(s(SV))).

n_abs_can_access_dims_in_application_context() ->
  S = "(F t where dim t <- 46 end) where fun F x = #.x end",
  ?assertMatch({46,_}, eval(S)). %% BTW upstream TL returns 46


%% Interesting draft tests. TODO Integrate them better in the test suite

next_in_wheredim_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({3,_}, eval("(next.t(     #.t)) @ [t <- 2] where dim t <- 0;; fun next.d X =   X  @ [d <- #.d + 1];; end;;")), %% Upstream TL returns 3
    ?_assertMatch({3,_}, eval("(next.t!(↑{} #.t)) @ [t <- 2] where dim t <- 0;; fun next.d!X = (↓X) @ [d <- #.d + 1];; end;;")), %% Upstream TL returns 3
    ?_assertMatch({2,_}, eval("(next.t!(    #.t)) @ [t <- 2] where dim t <- 0;; fun next.d!X =   X  @ [d <- #.d + 1];; end;;")), %% Upstream TL returns 2
    ?_assertMatch({[{dim,_,"t"}],_}, eval("(next.t.(↑{} #.t)) @ [t <- 2] where dim t <- 0;; fun next.d.X = (↓X) @ [d <- #.d + 1];; end;;")), %% Upstream TL returns spundef
    ?_assertMatch({[{dim,_,"t"}],_}, eval("(next.t.(    #.t)) @ [t <- 2] where dim t <- 0;; fun next.d.X =   X  @ [d <- #.d + 1];; end;;")) %% Upstream TL returns spundef
   ]}.

next_out_of_wheredim_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({3,_}, eval("((next.t (    #.t)) @ [t <- 2] where dim t <- 0;; end) where fun next.d X =   X  @ [d <- #.d + 1];; end;;")), %% Upstream TL returns 3
    ?_assertMatch({3,_}, eval("((next.t!(↑{} #.t)) @ [t <- 2] where dim t <- 0;; end) where fun next.d!X = (↓X) @ [d <- #.d + 1];; end;;")), %% Upstream TL returns 3
    ?_assertMatch({2,_}, eval("((next.t!(    #.t)) @ [t <- 2] where dim t <- 0;; end) where fun next.d!X =   X  @ [d <- #.d + 1];; end;;")), %% Upstream TL returns 2
    ?_assertMatch({[{dim,_,"t"}],_}, eval("((next.t.(↑{} #.t)) @ [t <- 2] where dim t <- 0;; end) where fun next.d.X = (↓X) @ [d <- #.d + 1];; end;;")), %% Upstream TL returns spundef
    ?_assertMatch({[{dim,_,"t"}],_}, eval("((next.t.(    #.t)) @ [t <- 2] where dim t <- 0;; end) where fun next.d.X =   X  @ [d <- #.d + 1];; end;;")) %% Upstream TL returns spundef
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
