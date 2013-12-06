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
        ({expr, _, E}) ->
          E;

        ({intension_creation, _, FrozenDims, Body}) ->
          {i_abs, [{dim,Dim} || Dim <- FrozenDims], Body};
        ({intension_evaluation, _, IAbsExpr}) ->
          {i_apply, IAbsExpr};

        ({call, _, FunExpr, Params}) ->
          {fn_call, FunExpr, lists:map(fun rework_fun_param/1, Params)};

        ({where, _, Exp, DimDecls, VarDecls}) ->
          TopExpr = Exp,
          Vars = [{var,Var,E} || {var_decl,_,Var,E} <- VarDecls],
          Dims = [{dim,Dim,N} || {dim_decl,_,Dim,N} <- DimDecls],
          Funs = [{fn, Name,
                   lists:map(fun rework_fun_param/1, Params),
                   Body}
                  || {fun_decl,_,Name,Params,Body} <- VarDecls],
          {where, TopExpr, Vars ++ Dims ++ Funs};

        ({'if', _, Ifs, Else}) -> unwrap_elsifs(Ifs, Else);

        ({'#.', _, Val}) ->
          %% On Section 6.4.4 “Querying the context” of the TL-doc-0.3.0
          %%   it explicitly states that ‘#.’ takes a dimension as input.
          {'#', {dim,Val}};

        ({tuple, _, Assocs}) -> {t, Assocs};
        ({tuple_element, _, Lhs, Rhs}) ->
          %% On Section 6.4.5 “Tuples” of the TL-doc-0.3.0, tuples are
          %%   defined as a ‘set of (dimension, value) pairs’.
          {{dim,Lhs}, Rhs};

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


rework_fun_param({base_param,  _, P}) -> {b_param, P};
rework_fun_param({named_param, _, P}) -> {n_param, P};
rework_fun_param({value_param, _, P}) -> {v_param, P}.

unwrap_elsifs ([{if_expr,_,Cond,Then}|Rest], Else) ->
  {'if', Cond, Then, unwrap_elsifs(Rest,Else)};
unwrap_elsifs ([], Else) ->
  Else.

%% End of Module.
