%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(textensional_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

textensional_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    extensional_expr_w_missing_dims(),
    extensional_expr_w_primops(),
    extensional_options_parser()
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

extensional_options_parser () ->
  S = "black_scholes_call.1.2.3.4.5
    where
      fun black_scholes_call.S.X.T.r.v = return where
        var vsqrT = v * sqrt.T
        var d1 = (log.(S/X) + (r + 0.5f0 * pow.v.2) * T) / vsqrT
        var d2 = d1 - vsqrT
        var return = S * cnd.d1 - X * exp.((0-r) * T) * cnd.d2
        fun cnd.X = cnd_ret where
          var L = abs.X
          var K_x = 1 / (1 + 0.2316419f0 * L)
          var K_y = pow.K_x.2
          var K_z = pow.K_x.3
          var K_w = pow.K_x.4
          var dot =  0.31938153f0  * K_x
                  -  0.356563782f0 * K_y
                  +  1.781477937f0 * K_z
                  -  1.821255978f0 * K_w
          var W = (dot + 1.330274429f0 * K_w * K_x)
                / sqrt.(2 * Pi) * exp.(-0.5f0 * pow.L.2)
          var Pi = 3.1415926535f0
          var cnd_ret = if X > 0
                        then 1 - W
                        else     W fi
        end
      end",
  ?_assertMatch({42,_}, eval(S)).


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
