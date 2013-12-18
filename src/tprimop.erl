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

-export([floor/1, ceil/1]).
-export([  'sin'/1,   'cos'/1,   'tan'/1
        , 'asin'/1,  'acos'/1,  'atan'/1, 'atan2'/2
        ,  'sinh'/1,  'cosh'/1,  'tanh'/1
        , 'asinh'/1, 'acosh'/1, 'atanh'/1
        , 'exp'/1, 'log'/1, 'log10'/1, 'pow'/1, 'sqrt'/1]).
-export([abs/1]).

-export([f/1]).

-define(primop1(Name),
        Name (N) -> {primop, Name, [N]}).
-define(primop2(Name),
        Name (N,M) -> {primop, Name, [N,M]}).

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

?primop1('floor').
?primop1('ceil').

?primop1('sin').
?primop1('cos').
?primop1('tan').
?primop1('asin').
?primop1('acos').
?primop1('atan').
?primop2('atan2').
?primop1('sinh').
?primop1('cosh').
?primop1('tanh').
?primop1('asinh').
?primop1('acosh').
?primop1('atanh').
?primop1('exp').
?primop1('log').
?primop1('log10').
?primop1('pow').
?primop1('sqrt').

?primop1('abs').

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
f('floor') ->
  fun %% https://erlangcentral.org/wiki/index.php/Floating_Point_Rounding
    (X) when X >= 0 -> trunc(X);
    (X) ->
      T = trunc(X),
      case X - T of
        0 -> T;
        _ -> T - 1
      end
  end;
f('ceil') ->
  fun %% https://erlangcentral.org/wiki/index.php/Floating_Point_Rounding
    (X) when X < 0 -> trunc(X);
    (X) ->
      T = trunc(X),
      case X - T of
        0 -> T;
        _ -> T + 1
      end
  end;
f('atan2'=Fun) ->
  fun math:Fun/2;
f(Fun) when Fun == 'sin';
            Fun == 'cos';
            Fun == 'tan';
            Fun == 'asin';
            Fun == 'acos';
            Fun == 'atan';
            Fun == 'sinh';
            Fun == 'cosh';
            Fun == 'tanh';
            Fun == 'asinh';
            Fun == 'acosh';
            Fun == 'atanh';
            Fun == 'exp';
            Fun == 'log';
            Fun == 'log10';
            Fun == 'pow';
            Fun == 'sqrt' ->
  fun math:Fun/1;
f('abs') ->
  fun erlang:abs/1;
f(Op) ->
  fun erlang:Op/2.
