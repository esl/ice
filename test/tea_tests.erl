%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tea_tests).

%% tea_tests: tests for the interpreter.

-include_lib("eunit/include/eunit.hrl").

%% API tests.

%% Parallel, one-dimensional (tournament) wheredim clause (introducing parallelism)

t1_test () ->
    {ok, E6} = tea:string(
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
    I = [],
    E = [],
    K = [],
    TimeD = {[0],time},
    SpaceD = {[0],space},
    D = [TimeD,SpaceD],
    tcache:start_link(100),
    ?assertMatch({7,_}, tcore:eval(E6, I, E, K, D, [0], 0)).
    % {badmatch,_} = (catch tcore:eval(bla, I, E, K, D, [0], 0)). % kills the cache.

%% End of Module.
