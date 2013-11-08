-module(tprimop).

-export([eq/2, neq/2]).
-export([tand/2, tor/2]).
-export([lt/2, gt/2]).
-export([lte/2, gte/2]).
-export([plus/2, minus/2, times/2, product/2]).
-export([mod/2]).
-export([ilogn/1]).

%%----------------------------------------------------------------------------
eq (A, B) ->
  {primop, fun erlang:'=='/2, [A,B]}.
neq (A, B) ->
  {primop, fun erlang:'/='/2, [A,B]}.

tand (A, B) ->
  {primop, fun erlang:'and'/2, [A,B]}.
tor (A, B) ->
  {primop, fun erlang:'or'/2, [A,B]}.

lt (A, B) ->
  {primop, fun erlang:'<'/2, [A,B]}.
gt (A, B) ->
  {primop, fun erlang:'>'/2, [A,B]}.

lte (A, B) ->
  {primop, fun erlang:'=<'/2, [A,B]}.
gte (A, B) ->
  {primop, fun erlang:'>='/2, [A,B]}.

plus (A, B) ->
  {primop, fun erlang:'+'/2, [A,B]}.
minus (A, B) ->
  {primop, fun erlang:'-'/2, [A,B]}.
times (A, B) ->
  {primop, fun erlang:'*'/2, [A,B]}.
product (A, B) ->
  {primop, fun erlang:'/'/2, [A,B]}.

mod (A, B) ->
  %% http://stackoverflow.com/a/858649/1418165
  {primop,
    fun
      (X, Y) -> (X rem Y + Y) rem Y
    end, [A,B]}.

ilogn (N) ->
  {primop,
    fun
      (0) -> 0;
      (X) -> round(math:log(X) / math:log(2))
    end, [N]}.
