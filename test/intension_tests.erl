%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(intension_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

i_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(intension_wo_frozen_dims()),
    ?_test(intension_w_one_frozen_dim()),
    ?_test(intension_w_one_frozen_dim_w_homonymous()),
    ?_test(intension_w_two_frozen_dims()),
    ?_test(intention_abstraction_w_missing_frozen_dim()),
    ?_test(intention_application_w_missing_frozen_dim()),
    ?_test(temperatureAtInuvik()),
    ?_test(temperatureAtInuvik_wo_string_comparison())
   ]}.


intension_wo_frozen_dims() ->
  ?assertMatch({46,_}, eval("↓(↑{} 46)")).

intension_w_one_frozen_dim() ->
  S = "↓((↑{t} #.t) @ [t <- 46]) where dim t <- 0 end",
  %% Behaviour of upstream TL:
  %% * "↓((↑{t} #.t) @ [t <- 46]) where dim t <- 0;; end;;" returns typeerror
  %% * "↓((↑{t} (#.t)) @ [t <- 46]) where dim t <- 0;; end;;" returns 0
  %% Ignoring upstream TL as it looks broken
  ?assertMatch({46,_}, eval(S)).

intension_w_one_frozen_dim_w_homonymous() ->
  S = "↓(((↑{t} #.t) where dim t <- 46 end) where dim t <- 0 end)",
  %% Behaviour of upstream TL:
  %% * "↓(((↑{t} #.t) where dim t <- 46;; end) where dim t <- 0;; end);;" returns typeerror
  %% * "↓(((↑{t} (#.t)) where dim t <- 46;; end) where dim t <- 0;; end);;" returns spdim
  %% Ignoring upstream TL as it looks broken
  ?assertMatch({46,_}, eval(S)).

intension_w_two_frozen_dims() ->
  S = "↓((↑{t,s} #.t - #.s) @ [t <- 46, s <- 1]) where dim t <- 58;; dim s <- 0 end",
  %% Behaviour of upstream TL:
  %% * "↓((↑{t,s} #.t - #.s) @ [t <- 46, s <- 1]) where dim t <- 58;; dim s <- 0;; end;;" returns typeerror
  %% * "↓((↑{t,s} (#.t - #.s)) @ [t <- 46, s <- 1]) where dim t <- 58;; dim s <- 0;; end;;" returns 58
  %% Ignoring upstream TL as it looks broken
  ?assertMatch({45,_}, eval(S)).

intention_abstraction_w_missing_frozen_dim() ->
  S = "↑{t} 46",
  DimT = {dim,"t"},
  %% Check representation of dim, in case modify the test
  ?assertMatch({i_abs, [DimT], _}, s(S)),
  K = [{DimT,0}],
  D = [],
  %% Upstream TL returns 'intension"I don\'t know how to print this
  %% type"'.
  %%
  %% Note on frozen dims in intension abstractions - It looks like
  %% that upstream TL, when evaluating an intension abstraction,
  %% simply closes the abstraction over the context (even if the
  %% context has not enough dims), and then maybe throws an error for
  %% missing dim only when evaluating the intension application.
  %% Ignoring the behaviour of upstream TL, in particular considering
  %% the cache semantics in the Feb 2013 paper; the reasons are (1)
  %% that if an intension abstraction is the expression of a variable,
  %% the frozen dims need to be checked at the time of evaluation of
  %% the abstraction in order to properly populate the cache and (2)
  %% that the evaluation of variables start with no known dims, so the
  %% evaluation of the abstraction needs to complain about missing
  %% frozen dims in order to have them.  What applies to the frozen
  %% dims in intension abstractions applies also to the frozen dims in
  %% anonymous functions.
  ?assertMatch({[DimT],_}, eval(S, K, D)).

intention_application_w_missing_frozen_dim() ->
  S = "↓(↑{t} 46)",
  DimT = {dim,"t"},
  %% Check representation of dim, in case modify the test
  ?assertMatch({i_apply, {i_abs, [DimT], _}}, s(S)),
  K = [{DimT,0}],
  D = [],
  %% Upstream TL returns 46. See "Note on frozen dims in intension
  %% abstractions".
  ?assertMatch({[DimT],_}, eval(S, K, D)).

temperatureAtInuvik() ->
  S = "(↓ tempInuvik) @ [location <- `Paris`]
      where
        dim location <- `Somewhere`
        var temperature =
          if #.location == `Inuvik` then
            46
          elsif #.location == `Paris` then
            58
          else
            1
          fi
        var tempAtLocation = ↑{location} temperature
        var tempInuvik = tempAtLocation @ [location <- `Inuvik`]
      end",
  %% Upstream TL returns spundef - ignoring it as in upstream TL
  %% comparison between strings is broken
  ?assertMatch({46,_}, eval(S)).

temperatureAtInuvik_wo_string_comparison() ->
  %% In upstream TL, comparison between strings is broken
  S = "// Legenda:
       // * Somewhere <-> 11
       // * Inuvik    <-> 22
       // * Paris     <-> 33
      (↓ tempInuvik) @ [location <- 33]
      where
        dim location <- 11
        var temperature =
          if #.location == 22 then
            46
          elsif #.location == 33 then
            58
          else
            1
          fi
        var tempAtLocation = ↑{location} temperature
        var tempInuvik = tempAtLocation @ [location <- 22]
      end",
  %% Upstream TL returns 58 - ignoring it as it looks broken
  ?assertMatch({46,_}, eval(S)).


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

t1(T) ->
  ttransform1:transform1(T).

tcore_eval(T, K, D) ->
  tcore:eval(T,[],[],K,D,{[],self()},0).

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T);
eval(T) ->
  tea:eval(T).

eval(S, K, D) when is_list(S) ->
  tcore_eval(t1(t0(s(S))), K, D).

%% End of Module.
