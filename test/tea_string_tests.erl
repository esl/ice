%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tea_string_tests).

%% tea_string_tests: tests for function tea:string/1.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

const_test () ->
    {ok, Tree} = tea:string("42"),
    ?assertEqual(42, Tree).

bool_test () ->
    {ok, Tree} = tea:string("true"),
    ?assertEqual(true, Tree).

string_test () ->
    {ok, Tree} = tea:string("\"Test\""),
    ?assertEqual({string,"Test"}, Tree).

char_test () ->
    {ok, Tree} = tea:string("'.'"),
    ?assertEqual({char,$.}, Tree).

constant_dim_test () ->
    {ok, Tree} = tea:string("#.t"),
    ?assertEqual(
        {'#', {dim,"t"}},
        Tree).

tuple_test () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    ?assertEqual({ok, {t, [{TimeD,1}, {SpaceD,2}]}},
        tea:string(" [t <- 1, s <- 2] ")),
    ?assertEqual({ok, {t, [{{dim,{'#',TimeD}}, 0}, {{dim,{'#',SpaceD}}, 1}]}},
        tea:string(" [#.t <- 0, #.s <- 1] ")).

primop_test () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    ?assertEqual(
        {ok, {primop, fun erlang:'+'/2, [10,20]}},
        tea:string(" 10 + 20 ")),
    ?assertEqual(
        {ok, {primop, fun erlang:'+'/2, [{'#',TimeD}, {'#',SpaceD}]}},
        tea:string(" #.t + #.s ")).

perturb_test () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    ?assertEqual(
        {ok, {'@', {'#',SpaceD}, {t,[{SpaceD,0}]}}},
        tea:string(" #.s @ [s <- 0] ")),
    ?assertEqual(
        {ok, {'@', {'#',TimeD}, {t,[{SpaceD,0}]}}},
        tea:string(" #.t @ [s <- 0] ")),
    ?assertEqual(
        {ok, {'@', {'#',TimeD}, {t,[{SpaceD,0}, {TimeD,1}]}}},
        tea:string(" #.t @ [s <- 0, t <- 1] ")),
    ?assertEqual(
        {ok, {'@', {'#',TimeD}, {t,[{TimeD,1}, {SpaceD,0}]}}},
        tea:string(" #.t @ [t <- 1, s <- 0] ")).

elsif_test () ->
    ?assertEqual(
        {ok, {'if', {primop,fun erlang:'=='/2,[1,0]}, 1,
                {'if', {primop,fun erlang:'=='/2,[1,1]}, 2,
                    3}}},
        tea:string(" if 1 == 0 then 1 elsif 1 == 1 then 2 else 3 fi ")),
    ?assertEqual(
        {ok, {'if', {primop,fun erlang:'=='/2,[1,0]}, 1,
                {'if', {primop,fun erlang:'=='/2,[0,1]}, 2,
                    {'if', {primop,fun erlang:'=='/2,[1,1]}, 3,
                        4}}}},
        tea:string(" if 1 == 0 then 1
                  elsif 0 == 1 then 2
                  elsif 1 == 1 then 3 else 4 fi ")).

%% Internals

%% End of Module.
