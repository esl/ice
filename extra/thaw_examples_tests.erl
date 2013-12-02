%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(thaw_examples_tests).

%% Examples written in Erlang rather than ICE.
%%
%% They are written as tests for convenience.

-include_lib("eunit/include/eunit.hrl").


%% API.

-export([fib_seq/1]).
-export([fib_par/1]).
-export([spawn_n/2, join/1]). %% Functions exported for internals of fib_par/1


fib_seq(0) ->
  0;
fib_seq(1) ->
  1;
fib_seq(N) ->
  fib_seq(N - 2) + fib_seq(N - 1).

fib_par(0) ->
  0;
fib_par(1) ->
  1;
fib_par(N) ->
  MFA2 = [?MODULE, fib_par, [N - 2]],
  MFA1 = [?MODULE, fib_par, [N - 1]],
  Pids = ?MODULE:spawn_n(self(), [MFA2, MFA1]),
  [N2, N1] = ?MODULE:join(Pids),
  N2 + N1.


%% API internals.

spawn_n(PPid, MFAs) ->
  lists:map(
    fun([M,F,A]) ->
        spawn(fun() -> PPid ! {self(), apply(M,F,A)} end)
    end,
    MFAs).

join(Pids) ->
  lists:map(fun(Pid) -> receive {Pid, RV} -> RV end end, Pids).


%% API tests.

fib_seq_test() ->
  ?assertEqual(
     [0,1,1,2,3,5,13,55],
     lists:map(fun ?MODULE:fib_seq/1, [0,1,2,3,4,5,7,10])).

fib_par_test() ->
  ?assertEqual(
     [0,1,1,2,3,5,13,55],
     lists:map(fun ?MODULE:fib_par/1, [0,1,2,3,4,5,7,10])).


%% Tests' internals.

%% End of Module.
