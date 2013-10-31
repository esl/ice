%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(fn_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

b_test_() ->
  [
   basic_b_abs(),
   toplevel_base_fun(),
   %%
   b_fun_w_two_formal_params_is_represented_as_one_b_abs_and_b_apply(),
   %%
   b_abs_nested_in_wheredim_does_not_cause_wrong_substitution(),
   wheredim_nested_in_b_abs_does_not_cause_wrong_substitution(),
   b_abs_can_return_b_abs_and_formal_params_w_same_name_are_not_confused(),
   b_abs_can_access_formal_params_of_outer_b_abs(),
   b_abs_can_access_local_dims_of_outer_wheredim(), %% ... differently from upstream TL
   b_abs_can_access_formal_params_of_outer_b_abs_and_local_dims_of_outer_wheredim(),
   b_abs_cannot_access_dims_in_application_context(),
   b_abs_cannot_access_local_dims_in_application_context(),
   %%
   b_abs_can_use_argument_for_querying_creation_context(),
   b_abs_can_use_argument_for_querying_creation_context2(),
   %%
   creation_of_b_abs_in_multiple_contexts_plays_nicely_w_cache()
  ].

v_test_() ->
  [
   v_fun_w_two_formal_params_is_represented_as_nested_v_abs_and_v_apply(),
   %%
   v_abs_can_access_dims_in_application_context(),
   v_abs_cannot_access_local_dims_in_application_context()
  ].

fn_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({-5,_}, eval("F.1!2.4!8
                               where
                                 fun F.b1!v1.b2!v2 = b1 - v1 + b2 - v2
                               end"))
   ]}.

%% TODO: integration with parser for sequence of function declarations and calls

basic_b_abs() ->
  T = s("F where fun F.argAsVarId = argAsVarId end"),
  ?assertEqual({where, "F",
                [{fn, "F", [{b_param,"argAsVarId"}], "argAsVarId"}]},
               T),
  ?assertEqual({wherevar, "F",
                [{"F", {b_abs, [], ["argAsVarId"], "argAsVarId"}}]},
               t0(T)),
  ArgAsPhiDim = {phi,"argAsVarId"},
  ?assertEqual({wherevar, "F",
                [{"F", {b_abs, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}}]},
               t1(t0(T))),
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       { {frozen_closed_b_abs, _I, _E, [], [ArgAsPhiDim], {'?',ArgAsPhiDim}}, _},
       eval(T))
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

b_abs_cannot_access_dims_in_application_context() ->
  {where, "F", [FnF]} = s("F where fun F.x = x - #.t end"),
  T = {where, {'@', s("F.46"), s("[t <- 1]")}, [FnF]},
  %% "F.46 @ [t <- 1] where fun F.x = x - #.t end end"
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({[{dim,"t"}],_}, eval(T)) ]}.

b_abs_cannot_access_local_dims_in_application_context() ->
  S =
    "(F.46 where dim t <- 1 end)
    where
      fun F.x = x - #.t
    end",
  {where, {where, {fn_call, "F", [{b_param, 46}]}, _}, _} = s(S),
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({[{dim,"t"}],_}, eval(S)) ]}.

b_abs_can_use_argument_for_querying_creation_context() ->
  DimT = {dim,"t"},
  %% XXX How to create from the parser a base abstraction with frozen dims?
  BAbsT0 =
    {b_abs, [DimT], ["x"],
     %% In body, query dimension specified as formal parameter
     {'#',
      %% XXX Having "t" after '#' would not be possible using the
      %% current integration with the parser as expression "t" after
      %% context query "#." would be considered dimension identifier
      "x"}},
  %% Create an AST as if it were generated by transformation 0
  T0 =
    {wherevar,
     {b_apply,
      t0(s("F @ [t <- 46]")),
      %% XXX Passing dimensions as base parameters is not possible in
      %% the current integration with the parser. It should be
      %% possible only having ground values as dimensions, or allowing
      %% variable and dimension identifier to share the same domain
      %% (as in upstream TL).
      [DimT]},
     [{"F",BAbsT0}]},
  T1 = t1(T0),
  {foreach, fun setup/0, fun cleanup/1,
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
          %% XXX Nesting "#.(#.y)" cannot be expressed with current
          %% integration with parser
          {'#',{dim,"y"}}},
         [{dim,"y","x"}]})},
  T0 =
    {wherevar,
     {b_apply,
      t0(s("F @ [t <- 46]")),
      [DimT]},
     [{"F",BAbsT0}]},
  T1 = t1(T0),
  {foreach, fun setup/0, fun cleanup/1,
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
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({45,_}, tcore_eval(T1)),
    ?_assertMatch({45,_}, tcore_eval(T1, _K=[{DimT,58}], _D=[DimT]))
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
  {where, "F", [FnF]} = s("F where fun F!x = x - #.t end"),
  T = {where, {'@', s("F!46"), s("[t <- 1]")}, [FnF]},
  %% "F!46 @ [t <- 1] where fun F!x = x - #.t end"
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({45,_}, eval(T)) ]}.

v_abs_cannot_access_local_dims_in_application_context() ->
  S =
    "(F!46 where dim t <- 1 end)
    where
      fun F!x = x - #.t
    end",
  {where, {where, {fn_call, "F", [{v_param, 46}]}, _}, _} = s(S),
  {foreach, fun setup/0, fun cleanup/1,
   %% XXX Is this test valid? Shall v_abs be able to access local
   %% dimensions with the same name of dimensions references in the
   %% body of the abs? Local dimensions of wheredim are replaced with
   %% hidden dims by transformation 1, and function body cannot know
   %% about that.
   [ ?_assertMatch({[{dim,"t"}],_}, eval(S)) ]}.


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
  {foreach, fun setup/0, fun cleanup/1,
   [ ?_assertMatch({[BAbsX],_}, tcore_eval({'if',{'?',BAbsX},46,58})) ]}.


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
