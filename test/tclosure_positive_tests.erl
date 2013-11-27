%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tclosure_positive_tests).

%% Positive tests for closure of abstractions over environment.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

regression_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({46,_}, eval("B where var A = 46;; var B = A end")),
    %%
    ?_assertMatch({46,_}, eval(" F.1 where var A = 46;; fun F.x =   A end")),
    ?_assertMatch({46,_}, eval(" F!1 where var A = 46;; fun F!x =   A end")),
    ?_assertMatch({46,_}, eval("(↓F) where var A = 46;; var F = ↑{} A end"))
   ]}.


abs_can_access_variable_ids_in_definition_env_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   lists:append(
     [
      shallow_named_abs_in_wherevar1(),
      shallow_named_abs_in_wherevar2(),
      shallow_unnamed_abs_in_wherevar1(),
      shallow_unnamed_abs_in_wherevar2(),
      nested_wherevar_clauses(),
      nested_unnamed_abs_in_wherevar()
      %%
      %% Vars with the same name in nested wherevar clauses are not
      %% supported ATM - there will be namespaces for avoiding this
      %% (?)
     ])}.


shallow_named_abs_in_wherevar1() ->
  lists:map(
    fun(S) -> ?_assertMatch({46,_}, eval(S)) end,
    [
     "(F where var A = 46;; fun F.x = A end).1",
     "(F where var A = 46;; fun F!x = A end)!1"
    ]).

shallow_named_abs_in_wherevar2() ->
  lists:map(
    fun(S) -> ?_assertMatch({47,_}, eval(S)) end,
    [
     "(G where fun G.y = F.y;; fun F.x = x + A;; var A = 46 end).1",
     "(G where fun G!y = F!y;; fun F!x = x + A;; var A = 46 end)!1",
     %%
     "G.1 where fun G.y = F.y;; fun F.x = x + A;; var A = 46 end",
     "G!1 where fun G!y = F!y;; fun F!x = x + A;; var A = 46 end",
     %%
     "X where var X = G.1;; fun G.y = F.y;; fun F.x = x + A;; var A = 46 end",
     "X where var X = G!1;; fun G!y = F!y;; fun F!x = x + A;; var A = 46 end"
    ]).

shallow_unnamed_abs_in_wherevar1() ->
  lists:map(
    fun(SOrT) -> ?_assertMatch({47,_}, eval(SOrT)) end,
    [
     {fn_call,
      {where, s("F"),
       [{var, s("F"), {b_abs, [], [s("x")], s("x + A")}},
        {var, s("A"), s("46")}]},
      [{b_param, s("1")}]},
     %% "(F where var F = \_ x -> x + A;; var A = 46 end).1"
     %%
     {fn_call,
      {where, s("F"),
       [{var, s("F"), {v_abs, [], [s("x")], s("x + A")}},
        {var, s("A"), s("46")}]},
      [{v_param, s("1")}]},
     %% "(F where var F = \ x -> x + A;; var A = 46 end)!1"
     %%
     %% "(F where var F = \\ x -> x + A;; var A = 46 end) 1" TODO: Improve transformation 0
     %%
     "↓(F where var F = ↑{} 1 + A;; var A = 46 end)"
    ]).

shallow_unnamed_abs_in_wherevar2() ->
  lists:map(
    fun(SOrT) -> ?_assertMatch({47,_}, eval(SOrT)) end,
    [
     {fn_call,
      {where, s("G"),
       [{var, s("G"), {b_abs, [], [s("y")], s("F.y")}},
        {var, s("F"), {b_abs, [], [s("x")], s("x + A")}},
        {var, s("A"), s("46")}]},
      [{b_param, s("1")}]},
     %% "(G where var G = \_ y -> F.y;; var F = \_ x -> x + A;; var A = 46 end).1"
     %%
     {fn_call,
      {where, s("G"),
       [{var, s("G"), {v_abs, [], [s("y")], s("F!y")}},
        {var, s("F"), {v_abs, [], [s("x")], s("x + A")}},
        {var, s("A"), s("46")}]},
      [{v_param, s("1")}]},
     %% "(G where var G = \ y -> F!y;; var F = \ x -> x + A;; var A = 46 end)!1"
     %%
     %% "(G where var G = \\ y -> F y;; var F = \\ x -> x + A;; var A = 46 end) 1" TODO: Improve transformation 0
     %%
     "↓(G where var G = ↑{} (↓F);; var F = ↑{} 1 + A;; var A = 46 end)"
    ]).

nested_wherevar_clauses() ->
  lists:map(
    fun(S) -> ?_assertMatch({47,_}, eval(S)) end,
    [
     "(G
     where
       var G = F
       where
         fun F.x = x + A;;
       end
       var A = 46
     end).1",
     %%
     "(H
     where
       var H = G
       where
         var G = F
         where
           fun F.x = x + A;;
         end
       end
       var A = 46
     end).1",
     %%
     "(H
     where
       var H = G
       where
         fun G.y = F.y
         fun F.x = x + A
       end
       var A = 46
     end).1",
     %%
     "(G
     where
       var G = (F
       where
         fun F.x = x + A;;
       end) @ [d <- #.d]
       var A = 46
       dim d <- 0
     end).1"
    ]).

nested_unnamed_abs_in_wherevar() ->
  lists:map(
    fun(P) -> ?_assertMatch({47,_}, eval(P)) end,
    [
     {fn_call,
      {where, s("G"),
       [{var, s("G"), {'@', {b_abs, [], [s("y")], s("F.y"  )}, s("[t <- 0]")}},
        {var, s("F"), {'@', {b_abs, [], [s("x")], s("x + A")}, s("[t <- 0]")}},
        {var, s("A"), s("46")},
        {dim, s("t"), s("0")}]},
      [{b_param, s("1")}]},
     %% "(G where var G = (\_ y -> F.y) @ [t <- 0];; var F = (\_ x -> x + A) @ [t <- 0];; var A = 46;; dim t <- 0 end).1"
     %%
     {fn_call,
      {fn_call,
       {where, {b_abs, [], [s("z")], s("G")},
        [{var, s("G"), {'@', {b_abs, [], [s("y")], s("F.y"  )}, s("[t <- 0]")}},
         {var, s("F"), {'@', {b_abs, [], [s("x")], s("x + A")}, s("[t <- 0]")}},
         {var, s("A"), s("46")},
         {dim, s("t"), s("0")}]},
       [{b_param, s("2")}]},
      [{b_param, s("1")}]}
     %% "(((\_ z -> G) where var G = (\_ y -> F.y) @ [t <- 0];; var F = (\_ x -> x + A) @ [t <- 0];; var A = 46;; dim t <- 0 end).2).1"
    ]).


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

s(S) ->
  {ok, T} = tea:string(S),
  T.

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T);
eval(T) ->
  tea:eval(T).

%% End of Module.
