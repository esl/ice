%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(wherevar_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

wherevar_test_() ->
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({46,_},
                  eval(basic())),
    ?_assertMatch({46,_},
                  eval(var_can_refer_to_var_defined_above_in_same_wherevar())),
    ?_assertMatch({46,_},
                  eval(var_can_refer_to_var_defined_below_in_same_wherevar())),
    ?_assertMatch({58, _}, %% FIXME This should give a compile error "Error, var A already defined."
                  eval(var_cannot_be_redefined_in_same_wherevar())),
    ?_assertError({badmatch, hang}, %% Probably same var name in different wherevar clauses shall belong to distinct namespaces, therefore being different variables
                  eval(var_redefined_in_nested_wherevar_hangs_cache())),
    ?_assertError({badmatch, hang}, %% idem as above
                  eval(var_redefined_in_nested_wherevar_hangs_cache2())),
    ?_assertError({badmatch, hang}, %% idem as above
                  eval(var_redefined_in_nested_wherevar_hangs_cache3())),
    ?_assertMatch({46,_}, %% This is really confusing for the programmer
                  eval(var_redefined_in_nested_wherevar_is_shadowed_by_outer_if_outer_already_queried()))
   ]}.

basic() ->
  "A
  where
    var A = 46
  end".

var_can_refer_to_var_defined_above_in_same_wherevar() ->
  "A
  where
    var B = 46
    var A = B
  end".

var_can_refer_to_var_defined_below_in_same_wherevar() ->
  "A
  where
    var A = B
    var B = 46
  end".

var_cannot_be_redefined_in_same_wherevar() ->
  "A
  where
    var A = 46
    var A = 58
  end".

var_redefined_in_nested_wherevar_hangs_cache() ->
  "A
  where
    var B = C
    where
      var A = 46 // Inner A
      var C = A
    end
    var A = B // Outer A
  end".

var_redefined_in_nested_wherevar_hangs_cache2() ->
  "A
  where
    var B = C
    where
      var A = 46 // Inner A
      var C = D
      where
        var D = A
      end
    end
    var A = B // Outer A
  end".

var_redefined_in_nested_wherevar_hangs_cache3() ->
  "X
  where
    var X = A
    where
      var B = C
      where
        var A = 46 // Inner A
        var C = D
        where
          var D = A
        end
      end
      var A = B // Outer A
    end
  end".

var_redefined_in_nested_wherevar_is_shadowed_by_outer_if_outer_already_queried() ->
  "A
  where
    var B = 46
    var C =
      if B == 46 then // Force evaluation of B
        D
        where
          var B = 58 // Programmer intends to shadow outer B
          var D = B
        end
      else
        1
      fi
    var A = C
  end".

%% Internals

tcache_stop(Pid) ->
  catch tcache:stop(),
  case is_process_alive(Pid) of
    false ->
      ok;
    true ->
      tcache_stop(Pid)
  end.

eval(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T).

%% End of Module.
