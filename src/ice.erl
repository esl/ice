%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice).

%% ice: interpreter for the Ice language

-export([string/1, file/1]).
-export([eval/1]).
-export([i/1]).

-type ast() :: term().

%% API

-spec string(string()) -> {ok | error, ast()}.
string(IceCode) ->
  {ok, Tree} = tparser:string(IceCode),
  rework_tree(Tree).

-spec file(string()) -> {ok | error, ast()}.
file(Filename) ->
  {ok, Tree} = tparser:file(Filename),
  rework_tree(Tree).

-spec eval(ast()) -> term().
eval(T) ->
  T0 = ice_trans0:transform0(T),
  T1 = ice_trans1:transform1(T0),
  ice_core:eval(T1,[],[],[],[],{[],self()},0).

-spec i(string()) -> term().
i(String) ->
  {ok, Tree} = string(String),
  ice_cache:create(),
  Res = eval(Tree),
  ice_cache:delete(),
  Res.

-spec f(string()) -> term().
f(Filename) ->
  {ok, Tree} = file(String),
  ice_cache:create(),
  Res = eval(Tree),
  ice_cache:delete(),
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
          bind_primop_to_base_fn_call(FunExpr, Params);

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

        ({'==',  _, A, B}) -> ice_primop:eq(A, B);
        ({'!=',  _, A, B}) -> ice_primop:neq(A, B);
        ({'<',   _, A, B}) -> ice_primop:lt(A, B);
        ({'<=',  _, A, B}) -> ice_primop:lte(A, B);
        ({'>',   _, A, B}) -> ice_primop:gt(A, B);
        ({'>=',  _, A, B}) -> ice_primop:gte(A, B);

        ({'not', _, A   }) -> ice_primop:'not'(A);
        ({'or',  _, A, B}) -> ice_primop:'or'(A, B);
        ({'and', _, A, B}) -> ice_primop:'and'(A, B);

        ({'+', _, A   }) -> ice_primop:plus(A);
        ({'-', _, A   }) -> ice_primop:minus(A);
        ({'+', _, A, B}) -> ice_primop:plus(A, B);
        ({'-', _, A, B}) -> ice_primop:minus(A, B);
        ({'*', _, A, B}) -> ice_primop:times(A, B);
        ({'/', _, A, B}) -> ice_primop:divide(A, B);
        ({'%', _, A, B}) -> ice_primop:mod(A, B);

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


bind_primop_to_base_fn_call(FunExpr, Params) when is_list(FunExpr) ->
  case lists:all(fun({Type,_}) -> Type == b_param end, Params) of
    true ->
      Ps = lists:map(fun({_,P}) -> P end, Params),
      try apply(ice_primop, list_to_atom(FunExpr), Ps) of
          Primop -> Primop
      catch
        error:undef ->
          {fn_call, FunExpr, Params}
      end;
    false ->
      {fn_call, FunExpr, Params}
  end;
bind_primop_to_base_fn_call(FunExpr, Params) ->
  {fn_call, FunExpr, Params}.

unwrap_elsifs([{if_expr,_,Cond,Then}|Rest], Else) ->
  {'if', Cond, Then, unwrap_elsifs(Rest,Else)};
unwrap_elsifs([], Else) ->
  Else.

%% End of Module.
