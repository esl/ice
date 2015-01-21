%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice).

%% ice: interpreter for the Ice language

-export([eval/1]).
-export([i/1, f/1]).

-spec eval(term()) -> term().
eval(T) ->
  T0 = ice_t0:transform(T),
  T1 = ice_t1:transform(T0),
  ice_core:eval(T1,[],[],[],[],{[],self()},0).

-spec i(string()) -> term().
i(String) ->
  Tree = ice_string:parse(String),
  ice_cache:create(),
  Res = eval(Tree),
  ice_cache:delete(),
  Res.

-spec f(string()) -> term().
f(Filename) ->
  Tree = ice_file:parse(Filename),
  ice_cache:create(),
  Res = eval(Tree),
  ice_cache:delete(),
  Res.
