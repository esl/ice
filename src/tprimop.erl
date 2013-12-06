%%-------------------------------------------------------------------------------------
%% Primitive operations
%%-------------------------------------------------------------------------------------
-module(tprimop).

-export([eq/2, neq/2]).
-export([tand/2, tor/2]).
-export([lt/2, gt/2]).
-export([lte/2, gte/2]).
-export([plus/2, minus/2, times/2, divide/2]).
-export([mod/2]).
-export([ilogn/1]).

-export([f/1]).

%%----------------------------------------------------------------------------
%% Functions for creating the AST nodes of primitive operations
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
divide (A, B) ->
  {primop, '/', [A,B]}.

mod (A, B) ->
  {primop, '%', [A,B]}.

ilogn (N) ->
  {primop, 'ilogn', [N]}.


%%----------------------------------------------------------------------------
%% @doc Return the Erlang function evaluating the primitive operation.
%%----------------------------------------------------------------------------
f('%') ->
  fun %% http://stackoverflow.com/a/858649/1418165
    (X, Y) -> (X rem Y + Y) rem Y
  end;
f('ilogn') ->
  fun
    (0) -> 0;
    (X) -> round(math:log(X) / math:log(2))
  end;
f(Op) ->
  fun erlang:Op/2.
