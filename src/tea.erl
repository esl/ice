%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tea).

%% tea: interpreter for the Tea language

-export([string/1, file/1]).
-export([eval/1]).
-export([i/1]).

-type ast() :: term().

%% API

-spec string(string()) -> {ok | error, ast()}.
string(TeaCode) ->
  {ok, Tree} = tparser:string(TeaCode),
  rework_tree(Tree).

-spec file(string()) -> {ok | error, ast()}.
file(Filename) ->
  {ok, Tree} = tparser:file(Filename),
  rework_tree(Tree).

-spec eval(ast()) -> term().
eval(T) ->
  T0 = ttransform0:transform0(T),
  T1 = ttransform1:transform1(T0),
  tcore:eval(T1,[],[],[],[],{[],self()},0).

-spec i(string()) -> term().
i(String) ->
  {ok, Tree} = string(String),
  tcache:start_link(100),
  Res = eval(Tree),
  tcache:stop(),
  Res.

%% Internals

rework_tree (Tree) ->
  V = fun
        ({expr, _, E}) -> E;

        ({lambda, _, FrozenDims, Params, Body}) ->
          {fn, FrozenDims, Params, Body};

        ({intension_creation, _, FrozenDims, Body}) ->
          {i_abs, FrozenDims, Body};
        ({intension_evaluation, _, IAbsExpr}) ->
          {i_apply, IAbsExpr};

        ({call, _, FunExpr, Params}) ->
          case {FunExpr,Params} of
            {"atan2",[{b_param,N},{b_param,M}]} ->
              tprimop:atan2(N, M);
            {Fun,[{b_param,N}]} when Fun == "floor";
                                     Fun == "ceil";
                                     Fun == "sin";
                                     Fun == "cos";
                                     Fun == "tan";
                                     Fun == "asin";
                                     Fun == "acos";
                                     Fun == "atan";
                                     Fun == "sinh";
                                     Fun == "cosh";
                                     Fun == "tanh";
                                     Fun == "asinh";
                                     Fun == "acosh";
                                     Fun == "atanh";
                                     Fun == "exp";
                                     Fun == "log";
                                     Fun == "log10";
                                     Fun == "pow";
                                     Fun == "sqrt";
                                     Fun == "abs" ->
              tprimop:(list_to_existing_atom(Fun))(N);
            _ ->
              {fn_call, FunExpr, Params}
          end;

        ({where, _, Exp, DimDecls, VarDecls}) ->
          TopExpr = Exp,
          Dims = [{dim,Dim,N} || {dim_decl,_,Dim,N} <- DimDecls],
          Funs = [{var,Name,{fn,[],Params,Body}}
                  || {fun_decl,_,Name,Params,Body} <- VarDecls],
          Vars = [{var,Var,E} || {var_decl,_,Var,E} <- VarDecls] ++ Funs,
          {where, TopExpr, Dims ++ Vars};

        ({base_param,  _, P}) -> {b_param, P};
        ({named_param, _, P}) -> {n_param, P};
        ({value_param, _, P}) -> {v_param, P};

        ({'if', _, Ifs, Else}) -> unwrap_elsifs(Ifs, Else);

        ({'#.', _, Val}) -> {'#', Val};

        ({tuple, _, Assocs}) -> {t, Assocs};
        ({tuple_element, _, Lhs, Rhs}) -> {Lhs, Rhs};

        ({'or',  _, A, B}) -> tprimop:tor(A, B);
        ({'and', _, A, B}) -> tprimop:tand(A, B);
        ({'<',   _, A, B}) -> tprimop:lt(A, B);
        ({'<=',  _, A, B}) -> tprimop:lte(A, B);
        ({'==',  _, A, B}) -> tprimop:eq(A, B);
        ({'>=',  _, A, B}) -> tprimop:gte(A, B);
        ({'>',   _, A, B}) -> tprimop:gt(A, B);
        ({'!=',  _, A, B}) -> tprimop:neq(A, B);
        ({'+',   _, A, B}) -> tprimop:plus(A, B);
        ({'-',   _, A, B}) -> tprimop:minus(A, B);
        ({'*',   _, A, B}) -> tprimop:times(A, B);
        ({'/',   _, A, B}) -> tprimop:divide(A, B);
        ({'%',   _, A, B}) -> tprimop:mod(A, B);

        ({bool, _, Boolean}) -> Boolean;
        ({raw_string, _, S})    -> {string, S};
        ({cooked_string, _, S}) -> {string, S};
        ({char, _, Char}) -> {char, Char};

        ({'@', _, A, B}) -> {'@', A, B};

        ({int,_,N}) -> N;
        ({float,_,N}) -> N;
        ({id,_,Name}) -> Name
      end,
  case tvisitor:visit(V, Tree, bottom_up) of
    [X] ->
      {ok, X};
    Y ->
      {ok, Y}
  end.


unwrap_elsifs ([{if_expr,_,Cond,Then}|Rest], Else) ->
  {'if', Cond, Then, unwrap_elsifs(Rest,Else)};
unwrap_elsifs ([], Else) ->
  Else.

%% End of Module.
