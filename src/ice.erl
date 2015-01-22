%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice).

%% ice: interpreter for the Ice language

-export([start/0, stop/0]).
-export([eval/1]).
-export([i/1, f/1]).

start() ->
  ice_counter:start_link(),
  ice_cache:create().

stop() ->
  ice_cache:delete(),
  ice_counter:stop().

-spec eval(term()) -> term().
eval(T) ->
  T0 = ice_t0:transform(T),
  T1 = ice_t1:transform(T0),
  ice_counter:start_link(),
  V = ice_core:eval(T1,[],[],[],[],{[],self()},0),
  ice_counter:stop(),
  V.

-spec i(string()) -> term().
i(String) ->
  Tree = ice_string:parse(String),
  start(),
  Res = eval(Tree),
  stop(),
  Res.

-spec f(string()) -> term().
f(Filename) ->
  Tree = ice_file:parse(Filename),
  start(),
  Res = eval(Tree),
  stop(),
  Res.

