%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(fn_string_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

b_abs_test() ->
  {ok, T} = tea:string("fun base.b = 46"),
  ExpectedFn = {fn, "base", [{b_param,"b"}], 46},
  ?assertEqual([ExpectedFn], T).

n_abs_test() ->
  {ok, T} = tea:string("fun named N = 46"),
  ExpectedFn = {fn, "named", [{n_param,"N"}], 46},
  ?assertEqual([ExpectedFn], T).

v_abs_test() ->
  {ok, T} = tea:string("fun value!v = 46"),
  ExpectedFn = {fn, "value", [{v_param,"v"}], 46},
  ?assertEqual([ExpectedFn], T).

%% Internals

%% End of Module.
