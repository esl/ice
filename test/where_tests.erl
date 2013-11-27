%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(where_tests).

%% Tests for where/wheredim/wherevar clauses.
%%
%% Testing of homonymous ids is out of scope.

-include_lib("eunit/include/eunit.hrl").

-define(_testMockingTpar(Expr),
        {setup, fun mock_tpar/0, fun(_) -> unmock_tpar() end,
         ?_test(Expr)}).


%% API tests.

where_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(where_with_only_dims_is_represented_as_wheredim_only()),
    ?_test(where_with_only_vars_is_represented_as_wherevar_only()),
    %%
    ?_test(var_can_use_dim_in_same_where()),
    ?_test(var_can_use_dims_in_same_where()),
    %%
    ?_test(var_can_refer_to_var_defined_before_in_same_where()),
    ?_test(var_can_refer_to_var_defined_after_in_same_where()),
    %%
    ?_testMockingTpar(dim_cannot_use_var_in_same_where())
   ]}.

fn_declaration_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_assertMatch({-5,_}, eval("F.1!2.4!8
                               where
                                 fun F.b1!v1.b2!v2 = b1 - v1 + b2 - v2
                               end")),
    ?_assertMatch({46,_}, eval("(F.1).46
                               where
                                 fun F.x =
                                   G
                                   where
                                     fun G.y = y where var KeepMe = 0 end
                                   end
                               end"))
   ]}.


where_with_only_dims_is_represented_as_wheredim_only() ->
  S = "#.t where dim t <- 46 end",
  ?assertMatch({wheredim, _, [{_, 46}]}, t0(s(S))),
  ?assertMatch({46,_}, eval(S)).

where_with_only_vars_is_represented_as_wherevar_only() ->
  S = "A where var A = 46 end",
  ?assertMatch({wherevar, _, [{_, 46}]}, t0(s(S))),
  ?assertMatch({46,_}, eval(S)).

var_can_use_dim_in_same_where() ->
  S = "X where dim t <- 0;; var X = #.t end",
  ?assertMatch({0,_}, eval(S)).

var_can_use_dims_in_same_where() ->
  S = "X where var X = #.t + #.s;; dim t <- 2;; dim s <- 3 end",
  ?assertMatch({5,_}, eval(S)).

var_can_refer_to_var_defined_before_in_same_where() ->
  S = "A where var B = 46;; var A = B end",
  ?assertMatch({46,_}, eval(S)).

var_can_refer_to_var_defined_after_in_same_where() ->
  S = "A where var A = B;; var B = 46 end",
  ?assertMatch({46,_}, eval(S)).

dim_cannot_use_var_in_same_where() ->
  S = "#.t where var X = 46;; dim t <- X end",
  ?assertError({badmatch, {error, undefined_identifier, "X"}}, eval(S)).


%% Internals - Mocking

mock_tpar() ->
  ok = meck:new(tpar, [passthrough]),
  ok = meck:expect(tpar, eval,
                   fun(Xs, I, E, K, D, W, T) ->
                       tpar:eval_seq(Xs, I, E, K, D, W, T)
                   end).

unmock_tpar() ->
  ok = meck:unload(tpar).

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

t0(T) ->
  ttransform0:transform0(T).

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T).

%% End of Module.
