%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(known_dims_tests).

%% Tests for usage of known dimensions.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

who_cares_about_known_dims_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(context_query_needs_dim()),
    ?_test(tuple_does_not_need_dim()),
    ?_test(context_perturbation_does_not_need_dim()),
    ?_test(wheredim_does_not_need_dim()),
    ?_test(wherevar_inside_wheredim_does_not_need_dim())
   ]}.

only_needed_unknown_dims_are_returned_as_missing_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(primop_returns_as_missing_only_needed_unknown_dims())
   ]}.


context_query_needs_dim() ->
  QS = "#.t",
  %% Extract valid hidden dim id...
  TmpS = QS ++ " where dim t <- 46 end",
  {wheredim, QT1, [{DimT,_}]} = t1(t0(s(TmpS))),
  %% ... for explicitly specifying context and known dims.
  K = [{DimT,46}],
  D = [],
  ?assertMatch({[DimT],_}, ice_core_eval(QT1, K, D)).

tuple_does_not_need_dim() ->
  TS = "[t <- 46]",
  %% Extract valid hidden dim id...
  TmpS = TS ++ " where dim t <- 0 end",
  {wheredim, TT1, [{DimT,_}]} = t1(t0(s(TmpS))),
  %% ... for explicitly specifying context and known dims.
  K = [{DimT,0}],
  D = [],
  ?assertMatch({{te,[{DimT,46}]},_}, ice_core_eval(TT1, K, D)).

context_perturbation_does_not_need_dim() ->
  PS = "#.t @ [t <- 46]",
  %% Extract valid hidden dim id...
  TmpS = PS ++ " where dim t <- 0 end",
  {wheredim, PT1, [{DimT,_}]} = t1(t0(s(TmpS))),
  %% ... for explicitly specifying context and known dims.
  K = [{DimT,0}],
  D = [],
  ?assertMatch({46,_}, ice_core_eval(PT1, K, D)).

wheredim_does_not_need_dim() ->
  S = "#.t where dim t <- 46 end",
  ?assertMatch({46,_}, eval(S)).

wherevar_inside_wheredim_does_not_need_dim() ->
  S = "X where dim t <- 46;; var X = #.t end",
  ?assertMatch({46,_}, eval(S)).

primop_returns_as_missing_only_needed_unknown_dims() ->
  PS = "#.t + #.s",
  %% Extract valid hidden dim id...
  TmpS = PS ++ " where dim t <- 0;; dim s <- 1 end",
  {wheredim, PT1, [{DimT,{int,0}}, {DimS,{int,1}}]} = t1(t0(s(TmpS))),
  %% ... for explicitly specifying context and known dims.
  K = [{DimT,0}, {DimS,1}],
  D = [DimT],
  ?assertMatch({[DimS],_}, ice_core_eval(PT1, K, D)).


%% Internals

setup() ->
  ice:start().

cleanup(_Pid) ->
  ice:stop().

s(S) ->
  ice_string:parse(S).

t0(T) ->
  ice_t0:transform(T).

t1(T) ->
  ice_t1:transform(T).

ice_core_eval(T, K, D) ->
  ice_counter:start_link(),
  R = ice_core:eval(T,[],[],K,D,{[],self()},0),
  ice_counter:stop(),
  R.

eval(S) when is_list(S) ->
  T = ice_string:parse(S),
  ice:eval(T).

%% End of Module.
