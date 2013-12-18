%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(fn_string_tests).

%% Tests for parsing of functions.
%%
%% TODO: After functions with named parameters are supported, merge
%% these tests in other test suites (e.g. fn_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

b_abs_test_() ->
  [?_assertMatch({where, _,
                  [{var, "base", {fn, [], [{b_param,"b"}], 46}}]},
                 s("58 where fun base.b = 46 end")),
   ?_assertMatch({where, _,
                  [{var, "base", {fn, [], [{b_param,"b1"},
                                           {b_param,"b2"}], 46}}]},
                 s("58 where fun base.b1.b2 = 46 end")) ].

n_abs_test_() ->
  [?_assertMatch({where, _,
                  [{var, "named", {fn, [], [{n_param,"N"}], 46}}]},
                 s("58 where fun named N = 46 end")),
   ?_assertMatch({where, _,
                  [{var, "named", {fn, [], [{n_param,"N1"},
                                            {n_param,"N2"}], 46}}]},
                 s("58 where fun named N1 N2 = 46 end"))].

v_abs_test_() ->
  [?_assertMatch({where, _,
                  [{var, "value", {fn, [], [{v_param,"v"}], 46}}]},
                 s("58 where fun value!v = 46 end")),
   ?_assertMatch({where, _,
                  [{var, "value", {fn, [], [{v_param,"v1"},
                                            {v_param,"v2"}], 46}}]},
                 s("58 where fun value!v1 !v2 = 46 end"))].

misc_abs_test_() ->
  [
   ?_assertMatch({where, _,
                  [{var, "f", {fn, [], [{n_param,"N"},
                                        {b_param,"b"},
                                        {v_param,"v"}], 46}}]},
                 s("58 where fun f N .b !v = 46 end")),
   ?_assertMatch({where, _,
                  [{var, "f", {fn, [], [{n_param,"N"},
                                        {v_param,"v"},
                                        {b_param,"b"}], 46}}]},
                 s("58 where fun f N !v .b = 46 end")),
   ?_assertMatch({where, _,
                  [{var, "f", {fn, [], [{v_param,"v"},
                                        {n_param,"N"},
                                        {b_param,"b"}], 46}}]},
                 s("58 where fun f !v N .b = 46 end")),
   ?_assertMatch(
      {where, {primop, _, [{primop, _, [_,_]}, _]},
       [{var, "g", _},
        {var, "h", _},
        {var, "f", {fn, [], [{n_param,"a"},
                             {v_param,"b"},
                             {b_param,"c"},
                             {n_param,"d"},
                             {v_param,"e"},
                             {b_param,"f"},
                             {n_param,"g"}], 46}}]},
      s("f 1!2.3 4!5.6 7 + ((f 1!2.3) 4!5.6) 7 + h 7
        where
          fun f a!b.c d!e.f g = 46
          var g = f 1!2.3
          var h = g 4!5.6
        end"))
  ].


%% Internals

s(S) ->
  {ok, T} = tea:string(S),
  T.

%% End of Module.
