-module(ice_ast).

-export([transform/1]).

%%------------------------------------------------------------------------------
%% Transform AST to what the evaluator expects (FIXME)
%%------------------------------------------------------------------------------
transform(Tree) ->
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

	({';', _, X, Y}) ->
	  {seq, X, Y};

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

	({bool, _, Bool}) -> {bool, Bool};
	({char, _, Char}) -> {char, Char};
	({int, _, Int}) -> {int, Int};
	({float, _, Float}) -> {float, Float};
        ({raw_string, _, S})    -> {string, S};
        ({cooked_string, _, S}) -> {string, S};

        ({'@', _, A, B}) -> {'@', A, B};

        ({id,_,Name}) -> {id, Name}
      end,
  case ice_visitor:visit(V, Tree, bottom_up) of
    [X] ->
      X;
    Y ->
      Y
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------
bind_primop_to_base_fn_call({id, IdString} = Xi, Params) ->
  case lists:all(fun({Type,_}) -> Type == b_param end, Params) of
    true ->
      Ps = lists:map(fun({_,P}) -> P end, Params),
      try apply(ice_primop, list_to_atom(IdString), Ps) of
          Primop -> Primop
      catch
        error:undef ->
          {fn_call, Xi, Params}
      end;
    false ->
      {fn_call, Xi, Params}
  end;
bind_primop_to_base_fn_call(Xi, Params) ->
  {fn_call, Xi, Params}.

unwrap_elsifs([{if_expr,_,Cond,Then}|Rest], Else) ->
  {'if', Cond, Then, unwrap_elsifs(Rest,Else)};
unwrap_elsifs([], Else) ->
  Else.
