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
    TimeD = {dim,"t"},
    T = {'#',TimeD},
    K = [{TimeD,46}],
    D = [],
    ?assertMatch({[TimeD],_},
                 tcore:eval(T, [],[], K, D, [0], 0)).

context_perturbation_does_not_need_dim() ->
    TimeD = {dim,"t"},
    T = {'@',
         {'#',TimeD},
         {t,[{TimeD,46}]}},
    D = [],
    ?assertMatch({46, _},
                 tcore:eval(T, [],[],[], D, [0], 0)).

wherevar_does_not_need_dim() ->
    T = {wherevar,"X",
         [{"X",46}]},
    D = [],
    ?assertMatch({46,_},
                 tcore:eval(T, [],[],[], D, [0], 0)).

wheredim_does_not_need_dim() ->
    TimeD = {dim,"t"},
    T = {wheredim,
         {'#',TimeD},
         [{TimeD,58}]},
    D = [],
    ?assertMatch({58,_},
        tcore:eval(T, [],[],[], D, [0], 0)).

wherevar_inside_wheredim_does_not_need_dim() ->
    TimeD = {dim,"t"},
    T = {wheredim,
         {wherevar,"X",
          [{"X",{'#',TimeD}}]},
         [{TimeD,58}]},
    D = [],
    ?assertMatch({58,_},
                 tcore:eval(T, [],[],[], D, [0], 0)).


%% Internals

tcache_stop(Pid) ->
  catch tcache:stop(),
  case is_process_alive(Pid) of
    false ->
      ok;
    true ->
      tcache_stop(Pid)
  end.

%% End of Module.
