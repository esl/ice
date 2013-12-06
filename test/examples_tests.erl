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

laplatian_relaxation_test_() ->
  SFormat =
    "S @ [t <- ~s, x <- ~s, y <- ~s]
    where
      // Electrode has always been at (3,4) with potential 5 - and forever will
      var POTENTIAL = if #.x == 3 && #.y == 4 then 5 else 0 fi
      var S = if POTENTIAL != 0 then POTENTIAL else fby.t 0 (avg S) fi
      fun avg A = ((prev.x A + next.x A) + (prev.y A + next.y A)) / 4
    end
    where
      dim t <- 0 // time
      dim x <- 0
      dim y <- 0
    end
    where
      fun fby.d X Y = if #.d == 0 then X else Y @ [d <- #.d - 1] fi
      fun prev.d M = M @ [d <- #.d - 1]
      fun next.d M = M @ [d <- #.d + 1]
    end",
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({5,_}, eval(lists:flatten(io_lib:format(SFormat, ["0","3","4"])))),
    ?_assertMatch({5,_}, eval(lists:flatten(io_lib:format(SFormat, ["1","3","4"])))),
    %%
    ?_assertMatch({0,_}, eval(lists:flatten(io_lib:format(SFormat, ["0","4","4"])))),
    ?_assertMatch({1.25,_}, eval(lists:flatten(io_lib:format(SFormat, ["1","4","4"])))),
    ?_assertMatch({1.25,_}, eval(lists:flatten(io_lib:format(SFormat, ["2","4","4"]))))
   ]}.

fibonacci_test_() ->
  SFormat =
    "(fib @ [t <- ~s]
    where
      var fib = fby.t 0 (fby.t 1 (fib + fib @ [t <- #.t + 1]))
      dim t <- 0
    end
    ) where
      fun fby.d X Y = if #.d == 0 then X else Y @ [d <- #.d - 1] fi
    end",
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({ 0,_}, eval(lists:flatten(io_lib:format(SFormat, ["0"])))),
    ?_assertMatch({ 1,_}, eval(lists:flatten(io_lib:format(SFormat, ["1"])))),
    ?_assertMatch({ 1,_}, eval(lists:flatten(io_lib:format(SFormat, ["2"])))),
    ?_assertMatch({ 2,_}, eval(lists:flatten(io_lib:format(SFormat, ["3"])))),
    ?_assertMatch({ 3,_}, eval(lists:flatten(io_lib:format(SFormat, ["4"])))),
    ?_assertMatch({ 5,_}, eval(lists:flatten(io_lib:format(SFormat, ["5"])))),
    ?_assertMatch({13,_}, eval(lists:flatten(io_lib:format(SFormat, ["7"])))),
    ?_assertMatch({55,_}, eval(lists:flatten(io_lib:format(SFormat, ["10"]))))
   ]}.

matrix_multiplication_test_() ->
  %% Ref: Nov 2013 semantics paper.
  SFormat =
    "(multiply.r.c.p A B
    where
      var p = ~p
      var A = ~s
      var B = ~s
      dim r <- ~p
      dim c <- ~p
    end
    ) where

      fun fby.d X Y = if #.d == 0 then X else Y @ [d <- #.d - 1] fi

      // Change variance in dimension d_1 of X to dimension d_2.
      fun rotate.d_1.d_2 X = X @ [d_1 <- #.d_2]

      // Add up the first n elements in direction d_x of the encapsulated
      // intension X. The local variable Y holds the running sums of the
      // elements of X.
      fun sum.d_x.n X = Y @ [d_x <- n]
      where
        var Y = fby.d_x 0 (X + Y)
      end

      fun multiply.d_r.d_c.k X Y = W
      where
        dim d <- 0
        var Xd = rotate.d_c.d X
        var Yd = rotate.d_r.d Y
        var Z = Xd * Yd
        var W = sum.d.k Z
      end

    end",
  F = fun(P, A, B, {R,C}) ->
          lists:flatten(io_lib:format(SFormat, [P, A, B, R, C]))
      end,
  [
   %% A[](1x1) * B[](1x1) = [](1x1)
   {setup, fun setup/0, fun cleanup/1,
    ?_assertMatch({46,_}, eval(F(1, "2", "23", {1,1})))}, %% 2 * 23
   %%
   %% A[](mxp) * B[](pxn) = [](mxn)
   {foreach, fun setup/0, fun cleanup/1,
    %% A[](2x3)       * B[](3x2)           = [](2x2)
    %% [1 2 3; 4 5 6] * [7 8; 9 10; 11 12] = [58 64; 139 154]
    %% Ref: http://www.mathsisfun.com/algebra/matrix-multiplying.html
    [?_assertMatch(
        {Res,_},
        eval(
          F(3,
            %% A = [1 2 3; 4 5 6]
            "if #.r == 0 then
              if #.c == 0 then 1 elsif
                 #.c == 1 then 2 elsif
                 #.c == 2 then 3 else 999 fi
            elsif #.r == 1 then
              if #.c == 0 then 4 elsif
                 #.c == 1 then 5 elsif
                 #.c == 2 then 6 else 999 fi
            else
              999
            fi",
            %% B = [7 8; 9 10; 11 12]
            "if #.r == 0 then
              if #.c == 0 then  7 elsif
                 #.c == 1 then  8 else 999 fi
            elsif #.r == 1 then
              if #.c == 0 then  9 elsif
                 #.c == 1 then 10 else 999 fi
            elsif #.r == 2 then
              if #.c == 0 then 11 elsif
                 #.c == 1 then 12 else 999 fi
            else
              999
            fi",
            {R,C}))) || {R,C,Res} <- [ %% [58 64; 139 154]
                                       {0,0, 58}, {0,1, 64},
                                       {1,0,139}, {1,1,154}
                                     ]]}
  ].

matrix_multiplication_w_tournament_sum_test_() ->
  SFormat =
    "((S @ [r <- 0, c <- 0]
    where
      dim t <- ~p
      var S =
        if #.t == 0 then (multiply.r.c.p A B) else
                         ( S @ [r <- #.r * 2,     c <- #.c * 2]
                         + S @ [r <- #.r * 2 + 1, c <- #.c * 2]
                         + S @ [r <- #.r * 2,     c <- #.c * 2 + 1]
                         + S @ [r <- #.r * 2 + 1, c <- #.c * 2 + 1]
                         ) @ [t <- #.t - 1] fi
    end)
    where
      var p = ~p
      var A = ~s
      var B = ~s
      dim r <- 0
      dim c <- 0
    end)
    where

      fun fby.d X Y = if #.d == 0 then X else Y @ [d <- #.d - 1] fi

      // Change variance in dimension d_1 of X to dimension d_2.
      fun rotate.d_1.d_2 X = X @ [d_1 <- #.d_2]

      // Add up the first n elements in direction d_x of the encapsulated
      // intension X. The local variable Y holds the running sums of the
      // elements of X.
      fun sum.d_x.n X = Y @ [d_x <- n]
      where
        var Y = fby.d_x 0 (X + Y)
      end

      fun multiply.d_r.d_c.k X Y = W
      where
        dim d <- 0
        var Xd = rotate.d_c.d X
        var Yd = rotate.d_r.d Y
        var Z = Xd * Yd
        var W = sum.d.k Z
      end

    end",
  F = fun(Log2MN, P, A, B) ->
          lists:flatten(io_lib:format(SFormat, [Log2MN, P, A, B]))
      end,
  [
   %% A[](1x1) * B[](1x1) = [](1x1)
   {setup, fun setup/0, fun cleanup/1,
    ?_assertMatch({46,_}, eval(F(0, 1, "2", "23")))}, %% 2 * 23
   %%
   %% A[](mxp) * B[](pxn) = [](mxn)
   {setup, fun setup/0, fun cleanup/1,
    %% A[](2x3)       * B[](3x2)           = [](2x2)
    %% [1 2 3; 4 5 6] * [7 8; 9 10; 11 12] = [58 64; 139 154]
    %% Ref: http://www.mathsisfun.com/algebra/matrix-multiplying.html
    begin
      Res = lists:sum(lists:flatten([[58, 64], [139, 154]])),
      ?_assertMatch(
         {Res,_},
         eval(
           F(1, %% log_2 2 = 1
             3,
             %% A = [1 2 3; 4 5 6]
             "if #.r == 0 then
               if #.c == 0 then 1 elsif
                  #.c == 1 then 2 elsif
                  #.c == 2 then 3 else 999 fi
             elsif #.r == 1 then
               if #.c == 0 then 4 elsif
                  #.c == 1 then 5 elsif
                  #.c == 2 then 6 else 999 fi
             else
               999
             fi",
             %% B = [7 8; 9 10; 11 12]
             "if #.r == 0 then
               if #.c == 0 then  7 elsif
                  #.c == 1 then  8 else 999 fi
             elsif #.r == 1 then
               if #.c == 0 then  9 elsif
                  #.c == 1 then 10 else 999 fi
             elsif #.r == 2 then
               if #.c == 0 then 11 elsif
                  #.c == 1 then 12 else 999 fi
             else
               999
             fi")))
    end},
   {setup, fun setup/0, fun cleanup/1,
    %% A[](4x4)       * B[](4x4)           = [](4x4)
    %% [ 1  2  3  4
    %%   5  6  7  8
    %%   9 10 11 12
    %%  13 14 15 16 ] * [ 1 0 0 0
    %%                    0 1 0 0
    %%                    0 0 1 0
    %%                    0 0 0 1 ] = [ 1  2  3  4
    %%                                  5  6  7  8
    %%                                  9 10 11 12
    %%                                 13 14 15 16 ]
    {timeout, 10,
     ?_assertMatch(
        %% lists:sum(lists:flatten([ [ 1, 2, 3, 4],
        %%                           [ 5, 6, 7, 8],
        %%                           [ 9,10,11,12],
        %%                           [13,14,15,16] ] ))
        %% -> 136
        {136,_},
        eval(
          F(2, %% log_2 4 = 2
            4,
            %% A = [1 2 3 4; 5 6 7 8; 9 10 11 12; 13 14 15 16]
            "if 0 <= #.r && #.r < 4 then
              if 0 <= #.c && #.c < 4 then
                (4 * #.r) + (1 + #.c)
              else
                999
              fi
            else
              999
            fi",
            %% B = identity 4x4 matrix
            "if 0 <= #.r && #.r < 4 then
              if 0 <= #.c && #.c < 4 then
                if #.r == #.c then 1 else 0 fi
              else
                999
              fi
            else
              999
            fi")))}}
  ].

max_examples_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch(
       {2,_},
       eval(
         "Max @ [x <- 1, y <- 2]
         where
           var Max = if #.x > #.y then #.x else #.y fi
           dim x <- 0
           dim y <- 0
         end")),
    %%
    ?_assertMatch(
       {2,_},
       eval(
         "(MaxF!x!y) @ [x <- 1, y <- 2]
         where
           fun MaxF!px!py =
             Max
             where
               var Max = if #.px > #.py then #.px else #.py fi
             end
           dim x <- 0
           dim y <- 0
         end")),
    ?_assertMatch(
       {2,_},
       eval(
         "CurriedMaxF!y @ [x <- 1, y <- 2]
         where
           var CurriedMaxF =
             // The returned abstraction is closed over the first formal
             // parameter i.e. dim id x, but the ordinate of dim x is not saved
             (MaxF!x) @ [x <- 100]
           fun MaxF!px!py =
             Max
             where
               var Max = if #.px > #.py then #.px else #.py fi
             end
           dim x <- 0
           dim y <- 0
         end")),
    ?_assertMatch(
       {2,_},
       eval(
         "FakeCurriedMaxF!0 @ [x <- 1, y <- 2]
         where
           var FakeCurriedMaxF =
             // The returned abstraction is closed over the formal parameters
             // i.e. dim id x and dim id y, but the ordinates are not saved.
             (MaxF!x!y) @ [x <- 100, y <- 200]
           fun MaxF!px!py!unusedparam =
             Max
             where
               var Max = if #.px > #.py then #.px else #.py fi
             end
           dim x <- 0
           dim y <- 0
         end")),
    %%
    ?_assertMatch(
       {2,_},
       eval(
         "(i! IMax @ [x <- 1, y <- 2]
         where
           var IMax = (i^ {dx, dy} Max) @ [dx <- x, dy <- y]
           var Max = if #.(#.dx) > #.(#.dy) then #.(#.dx) else #.(#.dy) fi
           dim dx <- 0
           dim dy <- 0
         end)
         where
           dim x <- 0
           dim y <- 0
         end"))
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
