%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tcache_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

cache_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(superfluous_calc()),
    %%
    ?_test(try_to_find_knowing_both_needed_dims()),
    ?_test(try_to_find_wo_knowing_enough_needed_dims())
   ]}.


superfluous_calc() ->
  W = {[],self()},
  DimT = "t",
  DimS = "s",
  K = [{DimT,1},{DimS,0}],
  D = [          DimS   ],
  %%     {V', T'} = ice_cache:... ( X ,K,     D     ,W,T, V -if any-)
  {{calc,W}   ,0} = ice_cache:find("X",K,[         ],W,1            ),
  {[DimS,DimT],0} = ice_cache:add ("X",K,[         ],W,2,[DimS,DimT]),
  {{calc,W}   ,0} = ice_cache:find("X",K,[DimS,DimT],W,3            ),
  {46         ,0} = ice_cache:add ("X",K,[DimS,DimT],W,4,46         ),
  %% XXX Is the following calc wrong? Or shall the semantics not ask
  %% this question/find as K is supposed to have D as domain?
  {{calc,W}   ,0} = ice_cache:find("X",K,     D     ,W,1            ).

try_to_find_knowing_both_needed_dims() ->
  S = "A
      where
        dim t <- 1
        dim d <- 2
        var A =
          if #.t + #.d == 0 then
            B
          else
            B
          fi
        var B = #.t + #.d
      end",
  ?assertMatch({3,_}, eval(S)).

try_to_find_wo_knowing_enough_needed_dims() ->
  S = "A
      where
        dim t <- 0
        dim d <- 0
        var A =
          if #.t == 0 then
            B
          else
            B
          fi
        var B = #.t + #.d
      end",
  ?assertMatch({0,_}, eval(S)).


%% Internals

setup() ->
  ice:start().

cleanup(Pid) ->
  ice:stop().

eval(S) when is_list(S) ->
  T = ice_string:parse(S),
  ice:eval(T);
eval(T) ->
  ice:eval(T).

%% End of Module.
