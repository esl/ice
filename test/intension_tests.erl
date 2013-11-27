%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(intension_tests).

%% Tests for intension abstractions / applications.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

i_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(intension_wo_frozen_dims()),
    ?_test(intension_w_one_frozen_dim()),
    ?_test(intension_w_one_frozen_dim_w_homonymous()),
    ?_test(intension_w_two_frozen_dims()),
    %%
    ?_test(intention_abstraction_w_missing_frozen_dim()),
    ?_test(intention_application_w_missing_frozen_dim())
   ]}.

i_abs_body_does_not_need_parentheses_test_() ->
  %% Parser of upstream TL needs parentheses for identifying the body
  %% of intension abstractions but they are superfluous in this
  %% implementation.
  [
   ?_assertEqual(
      s("↓(↑{} (1 + 46));;"),
      s("↓(↑{}  1 + 46 );;")),
   ?_assertEqual(
      s("↓(↑{} (#.t)) where dim t <- 0;; end;;"),
      s("↓(↑{}  #.t ) where dim t <- 0;; end;;"))
  ].


intension_wo_frozen_dims() ->
  ?assertMatch({46,_}, eval("↓(↑{} 46)")).

intension_w_one_frozen_dim() ->
  S = "↓((↑{t} #.t) @ [t <- 46]) where dim t <- 0 end",
  %% Behaviour of upstream TL:
  %% * "↓((↑{t}  #.t ) @ [t <- 46]) where dim t <- 0;; end;;" returns typeerror
  %% * "↓((↑{t} (#.t)) @ [t <- 46]) where dim t <- 0;; end;;" returns 0
  %% Ignoring upstream TL as it looks broken
  ?assertMatch({46,_}, eval(S)).

intension_w_one_frozen_dim_w_homonymous() ->
  S = "↓(((↑{t} #.t) where dim t <- 46 end) where dim t <- 0 end)",
  %% Behaviour of upstream TL:
  %% * "↓(((↑{t}  #.t ) where dim t <- 46;; end) where dim t <- 0;; end);;" returns typeerror
  %% * "↓(((↑{t} (#.t)) where dim t <- 46;; end) where dim t <- 0;; end);;" returns spdim
  %% Ignoring upstream TL as it looks broken
  ?assertMatch({46,_}, eval(S)).

intension_w_two_frozen_dims() ->
  S = "↓((↑{t,s} #.t - #.s) @ [t <- 46, s <- 1]) where dim t <- 58;; dim s <- 0 end",
  %% Behaviour of upstream TL:
  %% * "↓((↑{t,s}  #.t - #.s ) @ [t <- 46, s <- 1]) where dim t <- 58;; dim s <- 0;; end;;" returns typeerror
  %% * "↓((↑{t,s} (#.t - #.s)) @ [t <- 46, s <- 1]) where dim t <- 58;; dim s <- 0;; end;;" returns 58
  %% Ignoring upstream TL as it looks broken
  ?assertMatch({45,_}, eval(S)).

intention_abstraction_w_missing_frozen_dim() ->
  IAbsS = "↑{t} 46",
  %% Upstream TL returns 'intension"I don\'t know how to print this
  %% type"' when evaluating "↑{t} 46".
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
  %% that the evaluation of variables starts with no known dims, so the
  %% evaluation of the abstraction needs to complain about missing
  %% frozen dims in order to have them.  What applies to the frozen
  %% dims in intension abstractions applies also to the frozen dims in
  %% anonymous functions.
  %%
  %% Extract valid hidden dim id...
  TmpS = "(" ++ IAbsS ++ ") where dim t <- 0 end",
  {wheredim, IAbsT1, _Dims=[{DimT,0}]} = t1(t0(s(TmpS))),
  %% ... for explicitly specifying context and known dims.
  K = [{DimT,0}],
  D = [],
  ?assertMatch({[DimT],_}, tcore_eval(IAbsT1, K, D)).

intention_application_w_missing_frozen_dim() ->
  IApplyS = "↓(↑{t} 46)",
  %% Upstream TL returns 46 when evaluating "↓(↑{t} 46)". See "Note on
  %% frozen dims in intension abstractions".
  %%
  %% Extract valid hidden dim id...
  TmpS = "(" ++ IApplyS ++ ") where dim t <- 0 end",
  {wheredim, IApplyT1, _Dims=[{DimT,0}]} = t1(t0(s(TmpS))),
  %% ... for explicitly specifying context and known dims.
  K = [{DimT,0}],
  D = [],
  ?assertMatch({[DimT],_}, tcore_eval(IApplyT1, K, D)).


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
  tea:eval(T).

%% End of Module.
