%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(fn_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

b_test_() ->
  [
   ?_test(basic_b_abs())
   %% TODO ?_test(b_abs_w_two_formal_params()),
   %% TODO ?_test(nested_b_abs_shadow_formal_params_correctly()),
   %% TODO ?_test(b_abs_nested_in_wheredim_does_not_cause_wrong_substitution()),
   %% TODO ?_test(wheredim_nested_in_b_abs_does_not_cause_wrong_substitution()),
   %% TODO ?_test(basic_b_apply())
  ].

basic_b_abs() ->
  %% TODO: parser
  T =
    {fn, "F",
     [{b_param,"argAsVarId"}],
     "argAsVarId"},
  ExpectedT0 =
    {wherevar, "F",
     [{"F",
       {b_abs, [],
        ["argAsVarId"],
        "argAsVarId"}}]},
  T0 = t0(T),
  ?assertEqual(ExpectedT0, T0),
  ArgAsPhiDim = {phi,"argAsVarId"},
  ExpectedT1 =
    {wherevar, "F",
     [{"F",
       {b_abs, [],
        [ArgAsPhiDim],
        {'#',ArgAsPhiDim}}}]},
  T1 = t1(T0),
  ?assertEqual(ExpectedT1, T1),
  %% TODO: eval
  ok.

%% Internals

t0(T) ->
  ttransform0:transform0(T).

t1(T) ->
  ttransform1:transform1(T).

%% End of Module.
