%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tea_tests).

%% tea_tests: tests for the interpreter.

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

v ({where, _, Id, DimDecls, VarDecls}) ->
    MainVar = id(Id),
    Vars = {wherevar, MainVar,
        [{id(Var), E} || {var_decl,_,Var,E} <- VarDecls]},
    {wheredim, Vars,
        [{{[0],id(Dim)},int(N)} || {dim_decl,_,Dim,N} <- DimDecls]};

v ({'if', _, [If|_], Else}) ->
    {if_expr,_,Cond,Then} = If,
    {'if', Cond, Then, Else};

v ({'#.', _, Val}) ->
    {'#', case Val of
            "s" ->        {[0],"s"};
            "t" ->        {[0],"t"};
            {id,_,"s"} -> {[0],"s"};
            {id,_,"t"} -> {[0],"t"}
    end};

v ({tuple, _, Assocs}) -> {t, Assocs};
v ({tuple_element, _, Lhs, Rhs}) -> {{[0],Lhs}, Rhs};

v ({'and', _, A, B}) -> tand(A, B);
v ({'>=', _, A, B}) -> gte(A, B);
v ({'<=', _, A, B}) -> lte(A, B);
v ({'*', _, A, B}) -> times(A, B);
v ({'+', _, A, B}) -> plus(A, B);
v ({'-', _, A, B}) -> minus(A, B);

v ({'@', _, A, B}) -> {'@', A, B};

v ({int, _, Int}) -> Int;
v ({id,_,Id}) -> Id.

id (Id) ->
    case Id of
        {id, _, Name} -> Name;
        Name -> Name
    end.

int (Int) ->
    case Int of
        {int, _, N} -> N;
        N -> N
    end.

%% API tests.

    %% Parallel, one-dimensional (tournament) wheredim clause (introducing parallelism)

p () ->
    {ok, T} = tparser:sp (
        "// Tournament in 1 dimension"                                                  "\n"
        "A"                                                                             "\n"
        "where"                                                                         "\n"
            "dim t <- 2"                                                                "\n"
            "dim s <- 0"                                                                "\n"

            "// Compute A across space"                                                 "\n"
            "var A ="                                                                   "\n"
                "if #.t <= 0 then"                                                      "\n"
                    "B"                                                                 "\n"
                "else"                                                                  "\n"
                    "(A @ [s <- #.s * 2] + A @ [s <- #.s * 2 + 1]) @ [t <- #.t - 1]"    "\n"
                "fi"                                                                    "\n"

            "// Ensure spatial values are between 1 and 1024"                           "\n"
            "var B ="                                                                   "\n"
               "if #.s >= 1 and #.s <= 1024 then"                                       "\n"
                    "#.s"                                                               "\n"
                "else"                                                                  "\n"
                    "1"                                                                 "\n"
                "fi"                                                                    "\n"
        "end"),
    [{expr,_,TheWhereDim}] = tvisitor:visit (fun v/1, T, bottom_up),
    TheWhereDim.

e6 () ->
    TimeD = {[0],"t"},
    SpaceD = {[0],"s"},
    {wheredim,
      {wherevar, "A", [
        {"A", {'if',
                lte({'#', TimeD}, 0),
                "B",
                {'@',
                  plus({'@', "A", {t, [{SpaceD,      times({'#', SpaceD}, 2    )}]}},
                       {'@', "A", {t, [{SpaceD, plus(times({'#', SpaceD}, 2), 1)}]}}),
                  {t, [{TimeD, minus({'#', TimeD}, 1)}]}
                }
              }
        },
        {"B", {'if',
                tand(gte({'#', SpaceD}, 1),
                     lte({'#', SpaceD}, 1024)),
                {'#', SpaceD},
                1
              }
        }
    ]},
    [{TimeD, 2}, {SpaceD, 0}]}.

t (E6) ->
    I = [],
    E = [],
    K = [],
    TimeD = {[0],time},
    SpaceD = {[0],space},
    D = [TimeD,SpaceD],
    tcache:start_link(100),
    ?assertMatch({7,_}, tcore:eval(E6, I, E, K, D, [0], 0)).
    % {badmatch,_} = (catch tcore:eval(bla, I, E, K, D, [0], 0)). % kills the cache.

the_test () ->
    Expected = e6(),
    Expected = p().
e6_test () -> t(e6()).
p_test () -> t(p()).


%% Internals

tand(A, B) ->
  {primop, fun erlang:'and'/2, [A, B]}.
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

%% End of Module.
