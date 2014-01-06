%%-------------------------------------------------------------------------------------
%% Primitive operations - AST nodes
%%-------------------------------------------------------------------------------------
-module(ice_primop).

-export([eq/2, neq/2]).
-export([lt/2, lte/2,
         gt/2, gte/2]).

-export(['not'/1, 'and'/2, 'or'/2]).

-export([floor/1, ceil/1]).
-export([abs/1]).

-export([plus/1, minus/1]).
-export([plus/2, minus/2,
         times/2, divide/2]).
-export([mod/2]).

-export([pow/2, sqrt/1]).
-export([exp/1, log/1]).
-export([log10/1]).
-export([log2/1]).

-export([   sin/1,   cos/1,   tan/1
        ,  asin/1,  acos/1,  atan/1, atan2/2]).
-export([  sinh/1,  cosh/1,  tanh/1
        , asinh/1, acosh/1, atanh/1]).

-define(OP1(F, Op),
        F(A) -> {primop, Op, [A]}).
-define(OP2(F, Op),
        F(A,B) -> {primop, Op, [A,B]}).

-define(OP1(Op), ?OP1(Op, Op)).
-define(OP2(Op), ?OP2(Op, Op)).

%%----------------------------------------------------------------------------
%% Functions for creating the AST nodes of primitive operations
%%----------------------------------------------------------------------------

?OP2(eq, '==').
?OP2(neq, '/=').
?OP2(lt,  '<').
?OP2(lte, '=<').
?OP2(gt,  '>').
?OP2(gte, '>=').

?OP1('not').
?OP2('and').
?OP2('or').

?OP1(floor).
?OP1(ceil).
?OP1(abs).

?OP1(plus, '+').
?OP1(minus, '-').
?OP2(plus, '+').
?OP2(minus, '-').
?OP2(times, '*').
?OP2(divide, '/').
?OP2(mod, '%').

?OP2(pow).
?OP1(sqrt).
?OP1(exp).
?OP1(log).
?OP1(log10).
?OP1(log2).

?OP1(sin).
?OP1(cos).
?OP1(tan).
?OP1(asin).
?OP1(acos).
?OP1(atan).
?OP2(atan2).
?OP1(sinh).
?OP1(cosh).
?OP1(tanh).
?OP1(asinh).
?OP1(acosh).
?OP1(atanh).
