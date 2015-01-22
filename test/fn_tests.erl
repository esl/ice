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
   ?_assertMatch({[BAbsX],_},
                 ice_core_eval({'if',{'?',BAbsX},{int,46},{int,58}}))}.

query_application_context_for_dim_passed_as_arg_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({[{dim,_,"t"}],_}, eval("F.t where fun F.x = #.x;; dim t <- 46 end")), %% Upstream TL returns spdim.
    ?_assertMatch({46           ,_}, eval("F!t where fun F!x = #.x;; dim t <- 46 end")),
    ?_assertMatch({46           ,_}, eval("F t where fun F x = #.x;; dim t <- 46 end")),
    %%
    ?_assertMatch({[{dim,_,"t"}],_}, eval("(F.t where dim t <- 46 end) where fun F.x = #.x end")), %% Upstream TL returns spdim.
    ?_assertMatch({46           ,_}, eval("(F!t where dim t <- 46 end) where fun F!x = #.x end")),
    ?_assertMatch({46           ,_}, eval("(F t where dim t <- 46 end) where fun F x = #.x end")),
    %%
    ?_assertMatch({[{dim,_,"t"}],_}, eval("F.(t where dim t <- 46 end) where fun F.x = #.x end")), %% Upstream TL returns spdim.
    ?_assertMatch({[{dim,_,"t"}],_}, eval("F!(t where dim t <- 46 end) where fun F!x = #.x end")), %% Upstream TL returns spdim.
    ?_assertMatch({[{dim,_,"t"}],_}, eval("F (t where dim t <- 46 end) where fun F x = #.x end")) %% Upstream TL returns spdim.
   ]}.

query_application_context_for_dim_hardcoded_in_body_test_() ->
  UndefIdT = {badmatch, {error, undefined_identifier, {id,"t"}}},
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({[{dim,_,"t"}],_}, eval("F.1 where dim t <- 46;; fun F.x = #.t end")), %% Upstream TL returns spdim.
    ?_assertMatch({46           ,_}, eval("F!1 where dim t <- 46;; fun F!x = #.t end")),
    ?_assertMatch({46           ,_}, eval("F 1 where dim t <- 46;; fun F x = #.t end")),
    %%
    ?_assertError(UndefIdT, eval("(F.1 where dim t <- 46 end) where fun F.x = #.t end")), %% Upstream TL returns spdim.
    ?_assertError(UndefIdT, eval("(F!1 where dim t <- 46 end) where fun F!x = #.t end")), %% Upstream TL returns spdim.
    ?_assertError(UndefIdT, eval("(F 1 where dim t <- 46 end) where fun F x = #.t end")), %% Upstream TL returns spdim.
    %%
    ?_assertError(UndefIdT, eval("(F where dim t <- 46 end).1 where fun F.x = #.t end")), %% Upstream TL returns spdim.
    ?_assertError(UndefIdT, eval("(F where dim t <- 46 end)!1 where fun F!x = #.t end")), %% Upstream TL returns spdim.
    ?_assertError(UndefIdT, eval("(F where dim t <- 46 end) 1 where fun F x = #.t end")) %% Upstream TL returns spdim.
   ]}.

b_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(basic_b_abs()),
    ?_test(b_fun_w_two_formal_params_is_represented_as_one_b_abs_and_b_apply()),
    %%
    ?_test(b_abs_can_return_b_abs_and_formal_params_w_same_name_are_not_confused()),
    %%
    ?_test(b_abs_can_access_formal_params_of_outer_b_abs()),
    %%
    ?_test(toplevel_base_fun())
   ]}.

v_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(basic_v_abs()),
    ?_test(v_fun_w_two_formal_params_is_represented_as_nested_v_abs_and_v_apply())
   ]}.

n_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(n_fun_is_represented_as_v_fun()),
    ?_test(n_fun_w_two_formal_params_is_represented_as_v_fun())
   ]}.


basic_b_abs() ->
  S = "F where fun F.argAsVarId = argAsVarId end",
  ?assertEqual({where, {id,"F"},
                [{var, {id,"F"}, 
		  {fn, [], [{b_param,{id,"argAsVarId"}}], 
		   {id,"argAsVarId"}}}]},
               s(S)),
  ?assertEqual({wherevar, {id,"F"},
                [{{id,"F"}, 
		  {b_abs, [], [{id,"argAsVarId"}], {id,"argAsVarId"}}}]},
               t0(s(S))),
  ArgAsPhiDim = {phi,"argAsVarId"},
  ?assertEqual({wherevar, {id,"F"},
                [{{id,"F"}, 
		  {b_abs, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}}]},
               t1(t0(s(S)))),
  ?assertMatch(
     { {frozen_closed_b_abs, _I, _E, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}, _},
     eval(S)).

b_fun_w_two_formal_params_is_represented_as_one_b_abs_and_b_apply() ->
  S = "F.46.1 where fun F.x.y = x - y end", %% Minus is not commutative
  ?assertMatch({wherevar, {b_apply, {id,"F"}, [{int,46}, {int,1}]},
                [{{id,"F"}, 
		  {b_abs, [], [{id,"x"}, {id,"y"}],
		   {primop, _, [{id,"x"}, {id,"y"}]}}}]},
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

basic_v_abs() ->
  S = "F where fun F!argAsVarId = argAsVarId end",
  ?assertEqual({where, {id,"F"},
                [{var, {id,"F"},
                  {fn, [], [{v_param,{id,"argAsVarId"}}], {id,"argAsVarId"}}}]},
               s(S)),
  ?assertEqual({wherevar, {id,"F"},
                [{{id,"F"}, {v_abs, [], [{id,"argAsVarId"}], {id,"argAsVarId"}}}]},
               t0(s(S))),
  ArgAsPhiDim = {phi,"argAsVarId"},
  ?assertEqual({wherevar, {id,"F"},
                [{{id,"F"}, {v_abs, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}}]},
               t1(t0(s(S)))),
  ?assertMatch(
     { {frozen_closed_v_abs, _I, _E, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}, _},
     eval(S)).

toplevel_base_fun() ->
  S = "square.3 where fun square.x = x * x end",
  ?assertMatch({9,_}, eval(S)).

v_fun_w_two_formal_params_is_represented_as_nested_v_abs_and_v_apply() ->
  S = "F!46!1 where fun F!x!y = x - y end", %% Minus is not commutative
  ?assertMatch({wherevar,
                {v_apply,
                 {v_apply, {id,"F"}, [{int,46}]},
                 [{int,1}]},
                [{{id,"F"}, 
		  {v_abs, [], [{id,"x"}],
		   {v_abs, [], [{id,"y"}],
		    {primop, _, [{id,"x"}, {id,"y"}]}}}}]},
               t0(s(S))),
  ?assertMatch({45,_}, eval(S)).

n_fun_is_represented_as_v_fun() ->
  SN = "F      46  where fun F x =  x end",
  SV = "F!(↑{} 46) where fun F!x = ↓x end",
  ?assertMatch({wherevar,
                {v_apply, {id,"F"}, [{i_abs, [], {int,46}}]},
                [{{id,"F"}, {v_abs, [], [{id,"x"}],
			     {i_apply, {id,"x"}}}}]},
               t0(s(SN))),
  ?assertEqual(t0(s(SN)), t0(s(SV))),
  ?assertMatch({46,_}, eval(SN)).

n_fun_w_two_formal_params_is_represented_as_v_fun() ->
  SN = "F     46       1  where fun F x y =  x -  y end",
  SV = "F!(↑{}46)!(↑{} 1) where fun F!x!y = ↓x - ↓y end",
  ?assertEqual(t0(s(SN)), t0(s(SV))),
  ?assertMatch({45,_}, eval(SN)).


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
  ice:start().

cleanup(_) ->
  ice:stop().

s(S) ->
  ice_string:parse(S).

t0(T) ->
  ice_t0:transform(T).

t1(T) ->
  ice_t1:transform(T).

ice_core_eval(T) ->
  ice_core_eval(T, [], []).

ice_core_eval(T, K, D) ->
  ice_core:eval(T,[],[],K,D,{[],self()},0).

eval(S) when is_list(S) ->
  T = ice_string:parse(S),
  ice:eval(T);
eval(T) ->
  ice:eval(T).

%% End of Module.
