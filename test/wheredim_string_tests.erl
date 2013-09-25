%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(wheredim_string_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

e1_test () ->
    TimeD = {dim,"t"},
    {ok, Tree} = tea:string(
        "X
        where
            dim t <- 0
            var X = #.t
        end"),
    ?assertEqual(
        {wheredim,
            {wherevar, "X",
                [{"X", {'#', TimeD}}]},
            [{TimeD, 0}]},
        Tree).

wherevar_only_test () ->
    {ok, Tree} = tea:string(
        "X
        where
            var X = 46
        end"),
    ?assertEqual(
        {wherevar, "X", [{"X", 46}]},
        Tree).

wheredim_only_test () ->
    TimeD = {dim,"t"},
    {ok, Tree} = tea:string(
        "#.t
        where
            dim t <- 58
        end"),
    ?assertEqual(
        {wheredim,
            {'#', TimeD},
            [{TimeD, 58}]},
        Tree).

e2_test () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    {ok, Tree} = tea:string(
        "#.t + #.s
        where
            dim t <- 1
        end"),
    ?assertEqual(
        {wheredim,
            plus({'#',TimeD}, {'#',SpaceD}),
            [{TimeD, 1}]},
        Tree).

e3_test () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    {ok, Tree} = tea:string(
        "#.t + #.s
        where
            dim t <- 2
            dim s <- 3
        end"),
    ?assertEqual(
        {wheredim,
            plus({'#',TimeD}, {'#',SpaceD}),
            [{TimeD,2}, {SpaceD,3}]},
        Tree).

e4_test () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    {ok, Tree} = tea:string(
        "X
        where
            var X = #.t + #.s
            dim t <- 2
            dim s <- 3
        end"),
    ?assertEqual(
        {wheredim, 
            {wherevar, "X",
                [{"X",
                    plus({'#',TimeD}, {'#',SpaceD})}]},
            [{TimeD,2}, {SpaceD, 3}]},
        Tree).

e5_test () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    {ok, Tree} = tea:string(
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
    ?assertEqual(
        {wheredim, 
            {wherevar, "N0",
                [
                    {"N0",
                        {'if',
                            eq({'#', TimeD}, 0),
                            "N1",
                            {'@', "N0",
                                {t, [{TimeD, minus({'#', TimeD}, 1)}]}}}},
                    {"N1",
                        {'if',
                            eq({'#', SpaceD}, 0),
                            1,
                            times({'@', "N1", {t, [{SpaceD, minus({'#', SpaceD}, 1)}]}}
                                , 2)
                        }}
                ]},
            [{TimeD, 10}, {SpaceD, 10}]},
        Tree).

e6_test () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    {ok, Tree} = tea:string(
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
    ?assertEqual(
        {wheredim,
            {wherevar, "A", [
            {"A", {'if',
                    lte({'#', TimeD}, 0),
                    "B",
                    {'@',
                        plus({'@', "A", {t, [{SpaceD,      times({'#', SpaceD}, 2    )}]}},
                             {'@', "A", {t, [{SpaceD, plus(times({'#', SpaceD}, 2), 1)}]}}),
                        {t, [{TimeD, minus({'#', TimeD}, 1)}]}
                    }
                }
            },
            {"B", {'if',
                    tand(gte({'#', SpaceD}, 1),
                         lte({'#', SpaceD}, 1024)),
                    {'#', SpaceD},
                    1
                }
            }
            ]},
            [{TimeD, 2}, {SpaceD, 0}]},
        Tree).

e7_test () ->
    TimeD = {dim,"t"},
    XD = {dim, "x"},
    YD = {dim, "y"},
    {ok, Tree} = tea:string(
        "Y1 @ [x <- 0]
        where
            var Y1 = if #.t <= 0 then X1
                     else ((Y1 @ [x <- #.x * 2,     y <- #.y * 2]
                         +  Y1 @ [x <- #.x * 2 + 1, y <- #.y * 2])
                         + (Y1 @ [x <- #.x * 2,     y <- #.y * 2 + 1]
                         +  Y1 @ [x <- #.x * 2 + 1, y <- #.y * 2 + 1])
                          ) @ [t <- #.t - 1]
                     fi
            var X1 = if (#.x >= 1 and #.y <= 1024) and   ///Mind the .y
                        (#.y >= 1 and #.y <= 1024)
                     then #.x
                     else 1
                     fi
            dim t <- 2
            dim x <- 0
            dim y <- 0
        end"),
    ?assertEqual(
        {wheredim,
            {wherevar,
                {'@', "Y1", {t, [{XD,0}]}},
                [
                    {"Y1",
                        {'if',
                            lte({'#', TimeD}, 0),
                            "X1",
                            {'@',
                                plus(
                                    plus({'@', "Y1", {t, [{XD, times({'#', XD}, 2)},
                                                          {YD, times({'#', YD}, 2)}]}},
                                         {'@', "Y1", {t, [{XD, plus(times({'#', XD}, 2), 1)},
                                                          {YD, times({'#', YD}, 2)}]}}),
                                    plus({'@', "Y1", {t, [{XD, times({'#', XD}, 2)}, 
                                                          {YD, plus(times({'#', YD}, 2), 1)}]}},
                                         {'@', "Y1", {t, [{XD, plus(times({'#', XD}, 2), 1)}, 
                                                          {YD, plus(times({'#', YD}, 2), 1)}]}})
                                ),
                                {t, [{TimeD, minus({'#', TimeD}, 1)}]}}}},
                    {"X1",
                        {'if',
                            tand(
                                tand(gte({'#', XD}, 1), lte({'#', YD}, 1024)),
                                tand(gte({'#', YD}, 1), lte({'#', YD}, 1024))),
                            {'#', XD},
                            1}}
                ]},
            [{TimeD,2}, {XD,0}, {YD,0}]},
        Tree).

%% Internals

eq (A, B) ->
  {primop, fun erlang:'=='/2, [A, B]}.
tand (A, B) ->
  {primop, fun erlang:'and'/2, [A, B]}.
lte (A, B) ->
  {primop, fun erlang:'=<'/2, [A, B]}.
gte (A, B) ->
  {primop, fun erlang:'>='/2, [A, B]}.
plus (A, B) ->
  {primop, fun erlang:'+'/2, [A,B]}.
times (A, B) ->
  {primop, fun erlang:'*'/2, [A,B]}.
minus (A, B) ->
  {primop, fun erlang:'-'/2, [A, B]}.

%% End of Module.
