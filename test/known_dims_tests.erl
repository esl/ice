%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(known_dims_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

who_cares_about_known_dims_test_() ->
    {foreach,
     _Setup = fun() -> tcache:start_link(100), ok end,
     _Cleanup = fun(_) -> tcache:stop() end,
     [
      ?_test(context_query_needs_dim()),
      ?_test(context_perturbation_does_not_need_dim()),
      ?_test(wherevar_does_not_need_dim()),
      %%?_test(wheredim_does_not_need_dim()), FIXME
      ?_test(wherevar_inside_wheredim_does_not_need_dim())
     ]}.

context_query_needs_dim() ->
    TimeD = {[0],"t"},
    T = {'#',TimeD},
    K = [{TimeD,46}],
    D = [],
    ?assertMatch({[TimeD],_},
                 tcore:eval(T, [],[], K, D, [0], 0)).

context_perturbation_does_not_need_dim() ->
    TimeD = {[0],"t"},
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
    %% Wheredim should act "like a context perturbation with a unique
    %% local dimension" (ref "Multidimensional Infinite Data in the
    %% Language Lucid", Feb 2013), therefore the "fixed dimension"
    %% shall be added by the wheredim rule to the set of known
    %% dimensions (the rule in the paper needs this correction re
    %% Delta).
    TimeD = {[],"t"},
    T = {wheredim,
         {'#',TimeD},
         [{TimeD,58}]},
    D = [],
    ?assertMatch({58,_},
        tcore:eval(T, [],[],[], D, [0], 0)).

wherevar_inside_wheredim_does_not_need_dim() ->
    TimeD = {[0],"t"},
    T = {wheredim,
         {wherevar,"X",
          [{"X",{'#',TimeD}}]},
         [{TimeD,58}]},
    D = [],
    ?assertMatch({58,_},
                 tcore:eval(T, [],[],[], D, [0], 0)).


%% Internals

%% End of Module.
