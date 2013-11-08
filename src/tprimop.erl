-module(tprimop).

-export([eq/2, tand/2, lt/2, lte/2, gte/2, plus/2]).
-export([times/2, minus/2, product/2, ilogn/1]).  

%%------------------------------------------------------------------------------
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
  {primop, fun erlang:'+'/2, [A,B]}.
times(A, B) ->
  {primop, fun erlang:'*'/2, [A, B]}.
minus(A, B) ->
  {primop, fun erlang:'-'/2, [A, B]}.
product(A,B) ->
  {primop, fun erlang:'/'/2, [A, B]}.
ilogn(N) ->
  {primop, fun (0) -> 0;
	       (X) -> round(math:log(X) / math:log(2))
	   end, [N]}.
