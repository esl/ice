%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(wheredim_tests).

%% tea_tests: tests for the interpreter.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

e1_test_ () ->
    {ok, T} = tea:string(
        "X
        where
            dim t <- 0
            var X = #.t
        end"),
    TimeD = {[0],"t"},
    D = [TimeD],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({0,_},
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

wherevar_only_test_ () ->
    {ok, T} = tea:string(
        "X
        where
            var X = 46
        end"),
    D = [],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({46,_},
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

wheredim_only_test_ () ->
    {ok, T} = tea:string(
        "#.t
        where
            dim t <- 58
        end"),
    TimeD = {[0],"t"},
    D = [TimeD],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({58,_},
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

e2_test_ () ->
    {ok, T} = tea:string(
        "#.t + #.s
        where
            dim t <- 1
        end"),
    TimeD = {[0],"t"},
    SpaceD = {[0],"s"},
    D = [],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({[TimeD,SpaceD],_},
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

e3_test_ () ->
    {ok, T} = tea:string(
        "#.t + #.s
        where
            dim t <- 2
            dim s <- 3
        end"),
    TimeD = {[0],"t"},
    SpaceD = {[0],"s"},
    D = [],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({[TimeD,SpaceD],_},
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

e4_test_ () ->
    {ok, T} = tea:string(
        "X
        where
            var X = #.t + #.s
            dim t <- 2
            dim s <- 3
        end"),
    TimeD = {[0],"t"},
    SpaceD = {[0],"s"},
    D = [TimeD,SpaceD],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({5,_},
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

e5_test_ () ->
    %% Sequential, multi-dimensional wheredim clause
    {ok, T} = tea:string(
        "N0
        where
            var N0 = if #.t == 0
                     then N1
                     else N0 @ [t <- #.t - 1]
                     fi
            var N1 = if #.s == 0
                     then 1
                     else N1 @ [s <- #.s - 1] * 2
                     fi
            dim t <- 10
            dim s <- 10
        end"),
    TimeD = {[0],"t"},
    SpaceD = {[0],"s"},
    D = [TimeD,SpaceD],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({1024,_}, % Not sure about 1024 here.
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

e6_test_ () ->
    %% Parallel, one-dimensional (tournament) wheredim clause (introducing parallelism)
    {ok, T} = tea:string(
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
        end"),
    TimeD = {[0],"t"},
    SpaceD = {[0],"s"},
    D = [TimeD,SpaceD],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({7, _},
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

e7_test_ () ->
    %% Parallel, two-dimensional (tournament)
    {ok, T} = tea:string(
        "Y1 @ [x <- 0]
        where
            dim t <- 2
            var Y1 = if #.t <= 0 then X1
                     else ( Y1 @ [x <- #.x * 2,     y <- #.y * 2]
                          + Y1 @ [x <- #.x * 2 + 1, y <- #.y * 2]
                          + Y1 @ [x <- #.x * 2,     y <- #.y * 2 + 1]
                          + Y1 @ [x <- #.x * 2 + 1, y <- #.y * 2 + 1]
                          ) @ [t <- #.t - 1]
                     fi
            var X1 = if     #.x >= 1 and #.x <= 1024
                        and #.y >= 1 and #.y <= 1024
                     then #.x
                     else 1
                     fi
            dim x <- 0
            dim y <- 0
        end"),
    TimeD = {[0],"t"},
    D = [TimeD],
    {setup, fun () -> tcache:start_link(100) end,
            fun (_) -> tcache:stop() end,
    fun () ->
        ?assertMatch({25, _},
            tcore:eval(T, [],[],[], D, [0], 0))
    end}.

e8_test () ->
    %% Parallel, multi-dimensional matrix multiplication (the one you're all waiting for ;))
    ok.

%% Internals

%% End of Module.
