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
        ?_assertMatch({N, _}, eval(Tree))
    end || _ <- lists:seq(1,10)].

bool_test_ () ->
    {ok, Tree} = tea:string(" false "),
    fun () ->
        ?assertMatch({false, _}, eval(Tree))
    end.

string_test_ () ->
    {ok, Tree} = tea:string("`Test`"),
    S = {string,"Test"},
    fun () ->
        ?assertMatch({S, _}, eval(Tree))
    end.

constant_dim_test_ () ->
    {ok, Tree} = tea:string("#.t"),
    K = [{{dim,"t"}, 100}],
    D = [{dim,"t"}],
    fun () ->
        ?assertMatch({100, _}, eval(Tree, K, D))
    end.

tuple1_test_ () ->
    {ok, Tree} = tea:string(" [t <- 1, s <- 2] "),
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    K = [{TimeD,100}, {SpaceD,100}],
    D = [TimeD,SpaceD],
    fun () ->
        ?assertMatch({{te,[{TimeD,1},{SpaceD,2}]}, _}, eval(Tree, K, D))
    end.

primop1_test_ () ->
    {ok, Tree} = tea:string(" 10 + 20 "),
    fun () ->
        ?assertMatch({30, _}, eval(Tree))
    end.

primop2_test_ () ->
    {ok, Tree} = tea:string(" #.t + #.s "),
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    K = [{TimeD,100}, {SpaceD,100}],
    fun () ->
        ?assertMatch({[SpaceD,TimeD], _}, eval(Tree, K, _D=[]))
    end.

primop3_test_ () ->
    {ok, Tree} = tea:string(" #.t + #.s "),
    TimeD = {dim,"t"},
    SpaceD = {dim,"s"},
    K = [{TimeD,100}, {SpaceD,100}],
    D = [SpaceD, TimeD],
    fun () ->
        ?assertMatch({200, _}, eval(Tree, K, D))
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
        ?assertMatch({0, _}, eval(E1, K, [SpaceD])),
        ?assertMatch({[TimeD], _}, eval(E2)),
        ?assertMatch({1, _}, eval(E3, K, [TimeD])),
        ?assertMatch({1, _}, eval(E4, K, [TimeD]))
    end.


elsif_test_ () ->
    {ok, E1} = tea:string(" if 1 == 0 then 1
                         elsif 1 == 1 then 2 else 3 fi "),
    {ok, E2} = tea:string(" if 1 == 0 then 1
                         elsif 0 == 1 then 2
                         elsif 1 == 1 then 3 else 4 fi "),
    fun () ->
        ?assertMatch({2,_}, eval(E1)),
        ?assertMatch({3, _}, eval(E2))
    end.

%% Internals

t0(T) ->
  ttransform0:transform0(T).

t1(T) ->
  ttransform1:transform1(T).

eval(T, K, D) ->
  tcore:eval(t1(t0(T)),[],[],K,D,{[],self()},0).

eval(T) ->
  tea:eval(T).

%% End of Module.
