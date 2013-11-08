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
  {primop, '==', [A,B]}.
neq (A, B) ->
  {primop, '/=', [A,B]}.

tand (A, B) ->
  {primop, 'and', [A,B]}.
tor (A, B) ->
  {primop, 'or', [A,B]}.

lt (A, B) ->
  {primop, '<', [A,B]}.
gt (A, B) ->
  {primop, '>', [A,B]}.

lte (A, B) ->
  {primop, '=<', [A,B]}.
gte (A, B) ->
  {primop, '>=', [A,B]}.

plus (A, B) ->
  {primop, '+', [A,B]}.
minus (A, B) ->
  {primop, '-', [A,B]}.
times (A, B) ->
  {primop, '*', [A,B]}.
product (A, B) ->
  {primop, '/', [A,B]}.

mod (A, B) ->
  {primop, '%', [A,B]}.

ilogn (N) ->
  {primop, 'ilogn', [N]}.
