%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(textensional_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

textensional_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    extensional_expr_w_missing_dims(),
    extensional_expr_w_primops()
   ]}.


extensional_expr_w_missing_dims() ->
  DimT = {dim,"t"},
  DimS = {dim,"s"},
  T0 = {ext_expr, s("1"),
        {[{DimT,"int"},{DimS,"int"}], "int"}, 1},
  K = [{DimT,1}, {DimS,2}],
  D = [DimT],
  ?_assertMatch({[DimS],_}, tcore_eval(T0, K, D)).

extensional_expr_w_primops() ->
  S = "B
      where
        var B = A @ [t <- 0] + A @ [t <- 1]
        ext 1 A :: (t::uint s::float) -> int = 1 - #.t + #.s
        dim t <- 0
        dim s <- 2
      end",
  ?_assertMatch({5,_}, eval(S)).


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

tcore_eval(T) ->
  tcore_eval(T, [], []).

tcore_eval(T, K, D) ->
  tcore:eval(T,[],[],K,D,{[],self()},0).

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T);
eval(T) ->
  tea:eval(T).

%% End of Module.
