%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(fn_string_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

b_abs_test() ->
  ?assertMatch({where, _,
                [{fn, "base", [{b_param,"b"}], 46}]},
               s("58 where fun base.b = 46 end")).

n_abs_test() ->
  ?assertMatch({where, _,
                [{fn, "named", [{n_param,"N"}], 46}]},
               s("58 where fun named N = 46 end")).

v_abs_test() ->
  ?assertMatch({where, _,
                [{fn, "value", [{v_param,"v"}], 46}]},
               s("58 where fun value!v = 46 end")).

%% Internals

s(S) ->
  {ok, T} = tea:string(S),
  T.

%% End of Module.
