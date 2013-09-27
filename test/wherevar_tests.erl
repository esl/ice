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
    ?_test(basic()),
    ?_test(var_can_refer_to_var_defined_above_in_same_wherevar()),
    ?_test(var_can_refer_to_var_defined_below_in_same_wherevar()),
    ?_test(var_cannot_be_redefined_in_same_wherevar()), %% FIXME
    ?_test(var_redefined_in_nested_wherevar_hangs_cache()), %% Shall therefore wherevar be used at all?
    ?_test(var_redefined_in_nested_wherevar_hangs_cache2()), %% idem as above
    ?_test(var_redefined_in_nested_wherevar_hangs_cache3()), %% idem as above
    ?_test(var_redefined_in_nested_wherevar_is_shadowed_by_outer_if_outer_already_queried()) %% This is really confusing for the programmer
   ]}.

basic() ->
    {ok, T} = tea:string("
A
where
  var A = 46
end"),
  D = [],
  ?assertMatch({46, _}, tcore:eval(T, [],[],[], D, [0], 0)).

var_can_refer_to_var_defined_above_in_same_wherevar() ->
    {ok, T} = tea:string("
A
where
  var B = 46
  var A = B
end"),
  D = [],
  ?assertMatch({46, _}, tcore:eval(T, [],[],[], D, [0], 0)).

var_can_refer_to_var_defined_below_in_same_wherevar() ->
    {ok, T} = tea:string("
A
where
  var A = B
  var B = 46
end"),
  D = [],
  ?assertMatch({46, _}, tcore:eval(T, [],[],[], D, [0], 0)).

var_cannot_be_redefined_in_same_wherevar() ->
  {ok, T} = tea:string("
A
where
  var A = 46
  var A = 58
end"),
  D = [],
  %% FIXME This should give a compile error "Error, var A already defined."
  ?assertMatch({58, _}, tcore:eval(T, [],[],[], D, [0], 0)).

var_redefined_in_nested_wherevar_hangs_cache() ->
  {ok, T} = tea:string("
A
where
  var B = C
  where
    var A = 46 // Inner A
    var C = A
  end
  var A = B // Outer A
end"),
  D = [],
  ?assertError({badmatch, hang}, tcore:eval(T, [],[],[], D, [0], 0)).

var_redefined_in_nested_wherevar_hangs_cache2() ->
  {ok, T} = tea:string("
A
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
end"),
  D = [],
  ?assertError({badmatch, hang}, tcore:eval(T, [],[],[], D, [0], 0)).

var_redefined_in_nested_wherevar_hangs_cache3() ->
  {ok, T} = tea:string("
X
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
end"),
  D = [],
  ?assertError({badmatch, hang}, tcore:eval(T, [],[],[], D, [0], 0)).

var_redefined_in_nested_wherevar_is_shadowed_by_outer_if_outer_already_queried() ->
  {ok, T} = tea:string("
A
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
end"),
  D = [],
  ?assertMatch({46, _}, tcore:eval(T, [],[],[], D, [0], 0)).

%% Internals

tcache_stop(Pid) ->
  catch tcache:stop(),
  case is_process_alive(Pid) of
    false ->
      ok;
    true ->
      tcache_stop(Pid)
  end.

%% End of Module.
