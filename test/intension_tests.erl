%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(intension_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

i_test_() ->
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch(
       {[{dim,"t"}],_},
       tcore_eval(t1(t0(missing_frozen_dim())),
                  _K=[{{dim,"t"},0}], _D=[])),
    ?_assertMatch({46,_}, eval(temperatureAtInuvik()))
   ]}.


missing_frozen_dim() ->
  {i_abs, [{dim,"t"}], 46}.

temperatureAtInuvik() ->
  InuvikS = s("`Inuvik`"),
  ParisS = s("`Paris`"),
  DimLocation = {dim,"location"},
  {where,
   {'@', {i_apply, "tempInuvik"}, {t, [{DimLocation,ParisS}]}},
   [{var, "temperature",
     s("if #.location == `Inuvik` then
         46
       elsif #.location == `Paris` then
         58
       else
         1
       fi")},
    {var, "tempAtLocation",
     {i_abs, [DimLocation], "temperature"}},
    {var, "tempInuvik",
     {'@', "tempAtLocation", {t, [{DimLocation,InuvikS}]}}}
   ]}.


%% Internals

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
