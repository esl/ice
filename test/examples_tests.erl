%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(examples_tests).

%% Examples not aiming to test specific features.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

parallelism_examples_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(sequential_multi_dimensional()),
    ?_test(parallel_one_dimensional_tournament()),
    ?_test(parallel_two_dimensional_tournament()),
    ?_test(parallel_multi_dimensional_matrix_multiplication())
   ]}.

intension_examples_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(temperatureAtInuvik()),
    ?_test(temperatureAtInuvik_wo_string_comparison())
   ]}.


sequential_multi_dimensional() ->
  %% Sequential, multi-dimensional wheredim clause
  S =
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
    end",
  ?assertMatch({1024,_}, eval(S)).

parallel_one_dimensional_tournament() ->
  %% Parallel, one-dimensional (tournament) wheredim clause (introducing parallelism)
  S =
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
    end",
  ?assertMatch({7,_}, eval(S)).

parallel_two_dimensional_tournament() ->
  %% Parallel, two-dimensional (tournament)
  S =
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
    end",
  ?assertMatch({25,_}, eval(S)).

parallel_multi_dimensional_matrix_multiplication() ->
  %% Parallel, multi-dimensional matrix multiplication (the one you're all waiting for ;))
  ok. %% TODO

temperatureAtInuvik() ->
  S = "(↓ tempInuvik) @ [location <- `Paris`]
      where
        dim location <- `Somewhere`
        var temperature =
          if #.location == `Inuvik` then
            46
          elsif #.location == `Paris` then
            58
          else
            1
          fi
        var tempAtLocation = ↑{location} temperature
        var tempInuvik = tempAtLocation @ [location <- `Inuvik`]
      end",
  %% Upstream TL returns spundef - ignoring it as in upstream TL
  %% comparison between strings is broken
  ?assertMatch({46,_}, eval(S)).

temperatureAtInuvik_wo_string_comparison() ->
  %% In upstream TL, comparison between strings is broken
  S = "// Legenda:
       // * Somewhere <-> 11
       // * Inuvik    <-> 22
       // * Paris     <-> 33
      (↓ tempInuvik) @ [location <- 33]
      where
        dim location <- 11
        var temperature =
          if #.location == 22 then
            46
          elsif #.location == 33 then
            58
          else
            1
          fi
        var tempAtLocation = ↑{location} temperature
        var tempInuvik = tempAtLocation @ [location <- 22]
      end",
  %% Upstream TL returns 58 - ignoring it as it looks broken
  ?assertMatch({46,_}, eval(S)).


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

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T).

%% End of Module.
