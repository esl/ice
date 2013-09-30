%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tea_tests).

%% tea_tests: tests for the interpreter.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

const_test_ () ->
    %% replace with qc generators
    MaxN = 100000000000000000000,
    [begin
        N = random:uniform(MaxN),
        {ok, Tree} = tea:string(integer_to_list(N)),
        ?_assertMatch({N, _},
            tcore:eval(Tree, fun(X)->X end,[],[], [], self(), 0))
    end || _ <- lists:seq(1,10)].

bool_test_ () ->
    {ok, Tree} = tea:string(" false "),
    fun () ->
        ?assertMatch({false, _},
            tcore:eval(Tree, fun(X)->X end,[],[], [], self(), 0))
    end.

string_test_ () ->
    {ok, Tree} = tea:string("`Test`"),
    S = {string,"Test"},
    fun () ->
        ?assertMatch({S, _},
            tcore:eval(Tree, fun(X)->X end,[],[], [], self(), 0))
    end.

constant_dim_test_ () ->
    {ok, Tree} = tea:string("#.t"),
    K = [{{dim,"t"}, 100}],
    D = [{dim,"t"}],
    fun () ->
        ?assertMatch({100, _},
            tcore:eval(Tree, [],[],K, D, [0], 0))
    end.

tuple1_test_ () ->
    {ok, Tree} = tea:string(" [t <- 1, s <- 2] "),
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    K = [{TimeD,100}, {SpaceD,100}],
    D = [TimeD,SpaceD],
    fun () ->
        ?assertMatch({{te,[{TimeD,1},{SpaceD,2}]}, _},
            tcore:eval(Tree, [],[],K, D, [0], 0))
    end.

primop1_test_ () ->
    {ok, Tree} = tea:string(" 10 + 20 "),
    fun () ->
        ?assertMatch({30, _},
            tcore:eval(Tree, fun(X)->X end,[],[], [], self(), 0))
    end.

primop2_test_ () ->
    {ok, Tree} = tea:string(" #.t + #.s "),
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    K = [{TimeD,100}, {SpaceD,100}],
    fun () ->
        ?assertMatch({[SpaceD,TimeD], _},
            tcore:eval(Tree, [],[],K, [], [0], 0))
    end.

primop3_test_ () ->
    {ok, Tree} = tea:string(" #.t + #.s "),
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    K = [{TimeD,100}, {SpaceD,100}],
    D = [SpaceD, TimeD],
    fun () ->
        ?assertMatch({200, _},
            tcore:eval(Tree, [],[],K, D, [0], 0))
    end.

perturb_test_ () ->
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    K = [{TimeD,100}, {SpaceD,100}],
    {ok, E1} = tea:string(" #.s @ [s <- 0] "),
    {ok, E2} = tea:string(" #.t @ [s <- 0] "),
    {ok, E3} = tea:string(" #.t @ [s <- 0, t <- 1] "),
    {ok, E4} = tea:string(" #.t @ [t <- 1, s <- 0] "),
    fun () ->
        ?assertMatch({0, _},
            tcore:eval(E1, [],[],K, [SpaceD], [0], 0)),
        ?assertMatch({[TimeD], _},
            tcore:eval(E2, [],[],[], [], [0], 0)),
        ?assertMatch({1, _},
            tcore:eval(E3, [],[],K, [TimeD], [0], 0)),
        ?assertMatch({1, _},
            tcore:eval(E4, [],[],K, [TimeD], [0], 0))
    end.


elsif_test_ () ->
    {ok, E1} = tea:string(" if 1 == 0 then 1
                         elsif 1 == 1 then 2 else 3 fi "),
    {ok, E2} = tea:string(" if 1 == 0 then 1
                         elsif 0 == 1 then 2
                         elsif 1 == 1 then 3 else 4 fi "),
    fun () ->
        ?assertMatch({2,_},
            tcore:eval(E1, [],[],[], [], [0], 0)),
        ?assertMatch({3, _},
            tcore:eval(E2, [],[],[], [], [0], 0))
    end.

%% Internals

%% End of Module.
