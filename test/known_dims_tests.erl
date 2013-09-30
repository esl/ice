%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(known_dims_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

who_cares_about_known_dims_test_() ->
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_test(context_query_needs_dim()),
    ?_test(context_perturbation_does_not_need_dim()),
    ?_test(wherevar_does_not_need_dim()),
    ?_test(wheredim_does_not_need_dim()),
    ?_test(wherevar_inside_wheredim_does_not_need_dim())
   ]}.

context_query_needs_dim() ->
  {ok, T} = tea:string("#.t"),
  TimeD = {dim,"t"},
  ?assertEqual({'#',TimeD}, T),
  K = [{TimeD,46}],
  ?assertMatch(
     {[TimeD],_},
     eval(t1(t0(T)), K, _D=[])).

context_perturbation_does_not_need_dim() ->
  {ok, T} = tea:string("#.t @ [t <- 46]"),
  TimeD = {dim,"t"},
  ExpectedT =
    {'@',
     {'#',TimeD},
     {t,[{TimeD,46}]}},
  ?assertEqual(ExpectedT, T),
  ?assertMatch(
     {46, _},
     eval(t1(t0(T)), _K=[], _D=[])).

wherevar_does_not_need_dim() ->
  T = {wherevar,"X",
       [{"X",46}]},
  ?assertMatch(
     {46,_},
     eval(t1(T), _K=[], _D=[])).

wheredim_does_not_need_dim() ->
  T = {wheredim,
       {'#',{dim,"t"}},
       [{"t",58}]},
  ?assertMatch(
     {58,_},
     eval(t1(T), _K=[], _D=[])).

wherevar_inside_wheredim_does_not_need_dim() ->
  {ok, T} = tea:string(
              "X
              where
                dim t <- 58
                var X = #.t
              end"),
  ExpectedT0 =
    {wheredim,
     {wherevar,"X",
        [ {"X", {'#',{dim,"t"}}} ]},
     [ {"t", 58} ]},
  T0 = t0(T),
  ?assertEqual(ExpectedT0, T0),
  ?assertMatch(
     {58,_},
     eval(t1(T0), _K=[], _D=[])).


%% Internals

tcache_stop(Pid) ->
  catch tcache:stop(),
  case is_process_alive(Pid) of
    false ->
      ok;
    true ->
      tcache_stop(Pid)
  end.

t0(T) ->
  ttransform0:transform0(T).

t1(T) ->
  ttransform1:transform1(T).

eval(T, K, D) ->
  tcore:eval(T,[],[],K,D,{[],self()},0).

%% End of Module.
