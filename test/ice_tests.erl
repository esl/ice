%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice_tests).

%% Simple positive tests without interaction with the cache.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

const_test_() ->
  %% replace with qc generators
  MaxN = 100000000000000000000,
  [begin
     N = random:uniform(MaxN),
     ?_assertMatch({N, _}, eval(integer_to_list(N)))
   end || _ <- lists:seq(1,10)].

bool_test_() ->
  [
   ?_assertMatch({false, _}, eval("false")),
   ?_assertMatch({true,  _}, eval("true "))
  ].

string_test_() ->
  [
   ?_assertMatch({"Test", _}, eval("\"Test\"")),
   ?_assertMatch({"Test", _}, eval("`Test`"))
  ].

char_test() ->
  ?assertMatch({$., _}, eval("'.'")).

elsif_test_() ->
  [
   ?_assertMatch({2, _}, eval("if 1 == 0 then 1
                            elsif 1 == 1 then 2 else 3 fi")),
   ?_assertMatch({3, _}, eval("if 1 == 0 then 1
                            elsif 0 == 1 then 2
                            elsif 1 == 1 then 3 else 4 fi"))
  ].

context_query_test() ->
  ?assertMatch({46, _}, eval("#.t where dim t <- 46 end")).

tuple_test() ->
  ?assertMatch(
     {{te, [{{dim,_,"t"},1}, {{dim,_,"s"},2}]}, _},
     eval("[t <- 1, s <- 2] where dim t <- 0;; dim s <- 0 end")).

perturb_test_() ->
  [
   ?_assertMatch(
      {0, _},
      eval("#.s @ [s <- 0] where dim t <- 100;; dim s <- 100 end")),
   ?_assertMatch(
      {100, _},
      eval("#.t @ [s <- 0] where dim t <- 100;; dim s <- 100 end")),
   ?_assertMatch(
      {1, _},
      eval("#.t @ [s <- 0, t <- 1] where dim t <- 100;; dim s <- 100 end")),
   ?_assertMatch(
      {1, _},
      eval("#.t @ [t <- 1, s <- 0] where dim t <- 100;; dim s <- 100 end"))
  ].

primop_test_() ->
  [
   ?_assertMatch({30, _}, eval("10 + 20")),
   ?_assertMatch({-10, _}, eval("10 - 20")),
   ?_assertMatch({5, _}, eval("#.t + #.s where dim t <- 2;; dim s <- 3 end")),
   %%
   ?_assertMatch({ 46, _}, eval("+46")),
   ?_assertMatch({-46, _}, eval("-46")),
   ?_assertMatch({where, {primop,'+',[_]}, _}, s("+A where var A = 46 end")),
   ?_assertMatch({where, {primop,'-',[_]}, _}, s("-A where var A = 46 end")),
   ?_assertEqual({primop,'+',[{int,46}]}, s("+46")), %% No optimization
   ?_assertEqual({primop,'-',[{int,46}]}, s("-46")), %% No optimization
   %%
   ?_assertMatch({46, _}, eval("abs.(-46)")),
   ?_assertMatch({0.0, _}, eval("atan2.0.1")),
   %%
   ?_assertThrow({parse_error,_}, eval("+.46")),
   ?_assertThrow({parse_error,_}, eval("+.1.2")),
   ?_assertThrow({parse_error,_}, eval("not.true")),
   ?_assertThrow({parse_error,_}, eval("(not).true")),
   ?_assertThrow({parse_error,_}, eval("and.true.false"))
  ].


%% Internals

s(S) ->
  ice_string:parse(S).

eval(S) when is_list(S) ->
  ice_counter:start_link(),
  T = ice_string:parse(S),
  V = ice:eval(T),
  ice_counter:stop(),
  V.

%% End of Module.
