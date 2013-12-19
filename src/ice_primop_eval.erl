%%-------------------------------------------------------------------------------------
%% Primitive operations - Evaluation
%%-------------------------------------------------------------------------------------
-module(ice_primop_eval).

-export(['=='/2, '/='/2]).
-export(['<'/2, '=<'/2,
         '>'/2, '>='/2]).

-export(['not'/1, 'and'/2, 'or'/2]).

-export([floor/1, ceil/1]).
-export([abs/1]).

-export(['+'/1, '-'/1]).
-export(['+'/2, '-'/2,
         '*'/2, '/'/2]).
-export(['%'/2]).

-export([pow/2, sqrt/1]).
-export([exp/1, log/1]).
-export([log10/1]).
-export([log2/1]).

-export([   sin/1,   cos/1,   tan/1
        ,  asin/1,  acos/1,  atan/1, atan2/2]).
-export([  sinh/1,  cosh/1,  tanh/1
        , asinh/1, acosh/1, atanh/1]).

-define(E1(Op),
        Op(A) -> erlang:Op(A)).
-define(E2(Op),
        Op(A,B) -> erlang:Op(A,B)).

-define(M1(Op),
        Op(A) -> math:Op(A)).
-define(M2(Op),
        Op(A,B) -> math:Op(A,B)).

%%----------------------------------------------------------------------------
%% @doc Evaluate the primitive operation.
%%----------------------------------------------------------------------------

?E2('==').
?E2('/=').
?E2('<').
?E2('=<').
?E2('>').
?E2('>=').

?E1('not').
?E2('and').
?E2('or').

%% https://erlangcentral.org/wiki/index.php/Floating_Point_Rounding
floor(A) when A >= 0 ->
  trunc(A);
floor(A) ->
  T = trunc(A),
  case A - T of
    0 -> T;
    _ -> T - 1
  end.
%% https://erlangcentral.org/wiki/index.php/Floating_Point_Rounding
ceil(A) when A < 0 ->
  trunc(A);
ceil(A) ->
  T = trunc(A),
  case A - T of
    0 -> T;
    _ -> T + 1
  end.
?E1(abs).

?E1('+').
?E1('-').
?E2('+').
?E2('-').
?E2('*').
?E2('/').
'%'(A, B) -> (A rem B + B) rem B. %% http://stackoverflow.com/a/858649/1418165

?M2(pow).
?M1(sqrt).
?M1(exp).
?M1(log).
?M1(log10).
log2(0) -> 0;
log2(A) -> round(math:log(A) / math:log(2)).

?M1(sin).
?M1(cos).
?M1(tan).
?M1(asin).
?M1(acos).
?M1(atan).
?M2(atan2).
?M1(sinh).
?M1(cosh).
?M1(tanh).
?M1(asinh).
?M1(acosh).
?M1(atanh).
