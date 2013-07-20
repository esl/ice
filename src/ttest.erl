-module(ttest).

-export([run_tests/0]).
-export([debug_fn/0]).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------------------------
%% Primitive operations for tests
%%-------------------------------------------------------------------------------------
debug_fn() ->
  fun (A, B) ->
      A + B
  end.

eq(A, B) ->
  {primop, fun erlang:'=='/2, [A, B]}.
tand(A, B) ->
  {primop, fun erlang:'and'/2, [A, B]}.
lt(A, B) ->
  {primop, fun erlang:'<'/2, [A, B]}.
lte(A, B) ->
  {primop, fun erlang:'=<'/2, [A, B]}.
gte(A, B) ->
  {primop, fun erlang:'>='/2, [A, B]}.
plus(A, B) ->
  {primop, fun (X, Y) -> 
%%	       io:format("~p + ~p = ~p~n", [X,Y,X+Y]), 
	       X + Y end, [A,B]}.
times(A, B) ->
  {primop, fun (X, Y) -> 
%%	       io:format("~p * ~p = ~p~n", [X,Y,X*Y]), 
	       X * Y end, [A, B]}.
minus(A, B) ->
  {primop, fun erlang:'-'/2, [A, B]}.
product(A,B) ->
  {primop, fun erlang:'/'/2, [A, B]}.
ilogn(N) ->
  {primop, fun (0) -> 0;
	       (X) -> round(math:log(X) / math:log(2))
	   end, [N]}.

%%-------------------------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------------------------
const_test() ->
  %% replace with qc generators
  MaxN = 100000000000000000000,
  lists:foreach(
    fun (N) ->
	?assertMatch({N, _}, run_test(N))
    end, [random:uniform(MaxN) || _ <- lists:seq(1,10)]).

string_test() ->
  S = {string, "Test"},
  ?assertMatch({S, _}, run_test(S)).

constant_dim_test() ->
  CD = {'?', {[0], time}},
  K = [{{[0], time}, 100}],
  D = [{[0], time}],
  ?assertMatch({100, _}, run_test(CD, [], [], K, D, 0)).

tuple_test() ->
  K = [{{[0], time}, 100}, {{[1], space}, 100}],
  D = [{[0], time}, {[1], space}],
  Tup1 = [{{[0],time}, 1}, {{[1], space}, 2}],
  Tup4 = [{{'?', {[0], time}}, 0}, {{'?', {[1], space}}, 1}],
  ?assertMatch({{te, Tup1}, _}, run_test({t, Tup1}, [], [], K, D, 0)),
  ?assertMatch({[{[0], time}, {[1], space}], _}, run_test({t, Tup4})).

primop_test() ->
  K = [{{[0], time}, 100}, {{[1], space}, 100}],
  D = [{[1], space}, {[0], time}],
  AddNs = {primop, fun erlang:'+'/2, [10,20]},
  AddDims = {primop, fun erlang:'+'/2, [{'#', {[0], time}},
					{'#', {[1], space}}]},
  ?assertMatch({30, _}, run_test(AddNs)),
  ?assertMatch({[{[0],time},{[1],space}], _}, run_test(AddDims, [], [], K, [], 0)),
  ?assertMatch({200, _}, run_test(AddDims, [], [], K, D, 0)).

perturb_test() ->
  K = [{{[0], time}, 100}, {{[1], space}, 100}],
  D0 = [{[0], time}, {[1], space}],
  D1 = [{[0], time}],
  D2 = [{[0], space}],
  TimeD = {[0], time},
  SpaceD = {[1], space},
  E1 = {'@', {'#', SpaceD}, 
	{t, [{SpaceD, 0}]}},
  E2 = {'@', {'#', TimeD},
	{t, [{SpaceD, 0}]}},
  E3 = {'@', {'#', TimeD},
	{t, [{SpaceD, 0},
	     {TimeD, 1}]}},
  E4 = {'@', {'#', TimeD},
	{t, [{TimeD, 1},
	     {SpaceD, 0}]}},
  ?assertMatch({0, _}, run_test(E1, [], [], K, D2, 0)),
  ?assertMatch({[TimeD], _}, run_test(E2, [], [], [], [], 0)),
  ?assertMatch({1, _}, run_test(E3, [], [], K, D1, 0)),
  ?assertMatch({1, _}, run_test(E4, [], [], K, D1, 0)).

wheredim_test() ->
  K = [],
  D = [{[0],time}, {[1],space}],
  TimeD = {[0],time},
  SpaceD = {[1],space},
  E1 = {wheredim, 
  	{wherevar, "X",
  	 [{"X", {'#', TimeD}}]},
  	[{TimeD, 0}]},
  E2 = {wheredim, plus({'#', TimeD}, {'#', SpaceD}),
	[{TimeD, 1}]},
  E3 = {wheredim, plus({'#', TimeD}, {'#', SpaceD}),
	[{TimeD, 2},
	 {SpaceD, 3}]},
  E4 = {wheredim, 
	{wherevar, "X",
	 [{"X", plus({'#', TimeD}, {'#', SpaceD})}]},
	[{TimeD, 2},
	 {SpaceD, 3}]},
  %% Sequential, multi-dimensional wheredim clause
  E5 = {wheredim, 
	{wherevar, "N0",
	 [{"N0",
	   {'if', eq({'#', TimeD}, 0),
	    "N1",
	    {'@', "N0", {t, [{TimeD, minus({'#', TimeD}, 1)}]}}}},
	  {"N1",
	   {'if', eq({'#', SpaceD}, 0),
	    1,
	    times({'@', "N1", {t, [{SpaceD, minus({'#', SpaceD}, 1)}]}}, 2)}}]},
	[{TimeD, 10},
	 {SpaceD, 10}]},
  %% Parallel, multi-dimensional (tournament) wheredim clause (introducing parallelism)
  E6 = {wheredim,
  	{wherevar, "A", 
  	 [{"A",
  	   {'if', lte({'#', TimeD}, 0),
	    "B", %% for time = 1 => depends on 
	         %% "B" @ [space <- 0, time <- 0], 
	         %% "B" @ [space <- 1, time <- 0]
	    {'@', 
	     plus({'@', "A", {t, [{SpaceD, plus(times({'#', SpaceD}, 2), 1)}]}},
		  {'@', "A", {t, [{SpaceD, times({'#', SpaceD}, 2)}]}}),
	     {t, [{TimeD, minus({'#', TimeD}, 1)}]}}}},
	  {"B",
	   {'if', tand(gte({'#', SpaceD}, 1),
		       lte({'#', SpaceD}, 1024)),
	    {'#', SpaceD},
	    1}}]},
  	[{TimeD, 2},
  	 {SpaceD, 0}]},

  ?assertMatch({0,_}, run_test(E1, [], [], [], [TimeD], 0)),
  ?assertMatch({[{[0], time}, {[1], space}],_}, run_test(E2, [], [], [], [], 0)),
  ?assertMatch({[{[0], time}, {[1], space}],_}, run_test(E3, [], [], [], [], 0)),
  ?assertMatch({5,_}, run_test(E4, [], [], [], [TimeD,SpaceD], 0)),
  run_test(E6).

run_test(Src) ->
  I = fun (X) -> X end,
  E = [],
  K = [],
  D = [],
  T = 0,
  tcache:start_link(100),
%%  SrcT = ttrans:transform([0], Src, []),
  tcore:eval(Src, I, E, K, D, self(), T).

run_test(Src, I, E, K, D, T) ->
  tcache:start_link(100),
%%  SrcT = ttrans:transform([0], Src, []),
  tcore:eval(Src, I, E, K, D, [0], T).

run_tests() ->
  const_test(),
  string_test(),
  constant_dim_test(),
  tuple_test(),
  primop_test(),
  perturb_test(),
  wheredim_test().
  
  
  
