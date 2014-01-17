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
                  [{var, {id,"base"}, 
		    {fn, [], [{b_param,{id,"b"}}], {int,46}}}]},
                 s("58 where fun base.b = 46 end")),
   ?_assertMatch({where, _,
                  [{var, {id,"base"}, 
		    {fn, [], [{b_param,{id,"b1"}},
			      {b_param,{id,"b2"}}], {int,46}}}]},
                 s("58 where fun base.b1.b2 = 46 end")) ].

n_abs_test_() ->
  [?_assertMatch({where, _,
                  [{var, {id,"named"}, 
		    {fn, [], [{n_param,{id,"N"}}], {int,46}}}]},
                 s("58 where fun named N = 46 end")),
   ?_assertMatch({where, _,
                  [{var, {id,"named"}, 
		    {fn, [], [{n_param,{id,"N1"}},
			      {n_param,{id,"N2"}}], {int,46}}}]},
                 s("58 where fun named N1 N2 = 46 end"))].

v_abs_test_() ->
  [?_assertMatch({where, _,
                  [{var, {id,"value"}, 
		    {fn, [], [{v_param,{id,"v"}}], {int,46}}}]},
                 s("58 where fun value!v = 46 end")),
   ?_assertMatch({where, _,
                  [{var, {id,"value"}, 
		    {fn, [], [{v_param,{id,"v1"}},
			      {v_param,{id,"v2"}}], {int,46}}}]},
                 s("58 where fun value!v1 !v2 = 46 end"))].

misc_abs_test_() ->
  [
   ?_assertMatch({where, _,
                  [{var, {id,"f"}, 
		    {fn, [], [{n_param,{id,"N"}},
			      {b_param,{id,"b"}},
			      {v_param,{id,"v"}}], {int,46}}}]},
                 s("58 where fun f N .b !v = 46 end")),
   ?_assertMatch({where, _,
                  [{var, {id,"f"}, 
		    {fn, [], [{n_param,{id,"N"}},
			      {v_param,{id,"v"}},
			      {b_param,{id,"b"}}], {int,46}}}]},
                 s("58 where fun f N !v .b = 46 end")),
   ?_assertMatch({where, _,
                  [{var, {id,"f"}, 
		    {fn, [], [{v_param,{id,"v"}},
			      {n_param,{id,"N"}},
			      {b_param,{id,"b"}}], {int,46}}}]},
                 s("58 where fun f !v N .b = 46 end")),
   ?_assertMatch(
      {where, {primop, _, [{primop, _, [_,_]}, _]},
       [{var, {id,"g"}, _},
        {var, {id,"h"}, _},
        {var, {id,"f"}, 
	 {fn, [], [{n_param,{id,"a"}},
		   {v_param,{id,"b"}},
		   {b_param,{id,"c"}},
		   {n_param,{id,"d"}},
		   {v_param,{id,"e"}},
		   {b_param,{id,"f"}},
		   {n_param,{id,"g"}}], {int,46}}}]},
      s("f 1!2.3 4!5.6 7 + ((f 1!2.3) 4!5.6) 7 + h 7
        where
          fun f a!b.c d!e.f g = 46
          var g = f 1!2.3
          var h = g 4!5.6
        end"))
  ].


%% Internals

s(S) ->
  ice_string:parse(S).

%% End of Module.
