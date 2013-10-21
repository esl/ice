%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(wheredim_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

wheredim_test_() ->
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({58,_},          eval(basic())),
    ?_assertMatch({0,_},           eval(var_can_use_dim_in_same_where_clause())),
    %% TODO dim_cannot_use_var_in_same_where_clause
    ?_assertMatch({[{dim,"s"}],_}, eval(e2())),
    ?_assertMatch({5,_},           eval(e3())),
    ?_assertMatch({5,_},           eval(e4())),
    ?_assertMatch({1024,_},        eval(e5())),
    ?_assertMatch({7,_},           eval(e6())),
    ?_assertMatch({25,_},          eval(e7()))
   ]}.

basic() ->
  "#.t
  where
    dim t <- 58
  end".

var_can_use_dim_in_same_where_clause() ->
  "X
  where
    dim t <- 0
    var X = #.t
  end".

%% dim_cannot_use_var_in_same_where_clause() ->
%%   "#.t
%%   where
%%     var X = 46
%%     dim t <- X
%%   end".

e2() ->
  "#.t + #.s
  where
    dim t <- 1
  end".

e3() ->
  "#.t + #.s
  where
    dim t <- 2
    dim s <- 3
  end".

e4() ->
  "X
  where
    var X = #.t + #.s
    dim t <- 2
    dim s <- 3
  end".

e5() ->
  %% Sequential, multi-dimensional wheredim clause
  "N0
  where
    var N0 =
      if #.t == 0 then
        N1
      else
        N0 @ [t <- #.t - 1]
      fi
    var N1 =
      if #.s == 0 then
        1
      else
        N1 @ [s <- #.s - 1] * 2
      fi
    dim t <- 10
    dim s <- 10
  end".

e6() ->
  %% Parallel, one-dimensional (tournament) wheredim clause (introducing parallelism)
  "// Tournament in 1 dimension
  A
  where
    dim t <- 2
    dim s <- 0

    // Compute A across space
    var A =
      if #.t <= 0 then
        B
      else
        (A @ [s <- #.s * 2] + A @ [s <- #.s * 2 + 1]) @ [t <- #.t - 1]
      fi

    // Ensure spatial values are between 1 and 1024
    var B =
      if #.s >= 1 and #.s <= 1024 then
        #.s
      else
        1
      fi
  end".

e7() ->
  %% Parallel, two-dimensional (tournament)
  "Y1 @ [x <- 0]
  where
    dim t <- 2
    var Y1 =
      if #.t <= 0 then
        X1
      else
        ( Y1 @ [x <- #.x * 2,     y <- #.y * 2]
        + Y1 @ [x <- #.x * 2 + 1, y <- #.y * 2]
        + Y1 @ [x <- #.x * 2,     y <- #.y * 2 + 1]
        + Y1 @ [x <- #.x * 2 + 1, y <- #.y * 2 + 1]
        ) @ [t <- #.t - 1]
      fi
    var X1 =
      if #.x >= 1 and #.x <= 1024 and
         #.y >= 1 and #.y <= 1024 then
        #.x
      else
        1
      fi
    dim x <- 0
    dim y <- 0
  end".

e8_test () ->
    %% Parallel, multi-dimensional matrix multiplication (the one you're all waiting for ;))
    ok.

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
