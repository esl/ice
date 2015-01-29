-module(ice_ast).

-export([transform/1]).
-export([prepare/1]).

%%------------------------------------------------------------------------------
%% Transform AST to what the evaluator expects (FIXME)
%%------------------------------------------------------------------------------
transform(Es) when is_list(Es) ->
  [X] = map_prepare(Es),
  X.

prepare({expr, _, E}) ->
  prepare(E);
prepare({b_abs, _, Intens, Args, Body}) ->
  {b_abs, map_prepare(Intens), map_prepare(Args), prepare(Body)};
prepare({v_abs, _, Intens, Args, Body}) ->
  {v_abs, map_prepare(Intens), map_prepare(Args), prepare(Body)};
prepare({intension_creation, _, Intens, Body}) ->
  {i_abs, map_prepare(Intens), prepare(Body)};
prepare({intension_evaluation, _, E}) ->
  {i_apply, prepare(E)};
prepare({call, _, FunExpr, Params}) ->
  bind_primop_to_base_fn_call(FunExpr, Params);
prepare({where, _, E0, Xis, Eis}) ->
  Xis1 = [{prepare(Di), prepare(Ei)} 
          || {dim_decl, _, Di, Ei} <- Xis],
  Eis1 = [{prepare(Vi), prepare(Ei)} 
          || {var_decl, _, Vi, Ei} <- Eis],
  Funs = [{prepare(Vi), 
           {fn, [], map_prepare(Args), prepare(Body)}}
          || {fun_decl, _, Vi, Args, Body} <- Eis],
  {where, prepare(E0), Xis1, Eis1 ++ Funs};
prepare({b_param, _, P}) ->
  {b_param, prepare(P)};
prepare({v_param, _, P}) ->
  {v_param, prepare(P)};
prepare({n_param, _, P}) ->
  {n_param, prepare(P)};
prepare({'if', _, Ifs, Elses}) ->
  unwrap_elsifs(Ifs, Elses);
prepare({'#.', _, Val}) -> 
  {'#', prepare(Val)};
prepare({';', _, X, Y}) ->
  {seq, prepare(X), prepare(Y)};
prepare({tuple, _, Assocs}) -> 
  {t, Assocs};
prepare({tuple_element, _, Lhs, Rhs}) -> 
  {Lhs, Rhs};

prepare({'==',  _, A, B}) -> ice_primop:eq(A, B);
prepare({'!=',  _, A, B}) -> ice_primop:neq(A, B);
prepare({'<',   _, A, B}) -> ice_primop:lt(A, B);
prepare({'<=',  _, A, B}) -> ice_primop:lte(A, B);
prepare({'>',   _, A, B}) -> ice_primop:gt(A, B);
prepare({'>=',  _, A, B}) -> ice_primop:gte(A, B);

prepare({'not', _, A   }) -> ice_primop:'not'(A);
prepare({'or',  _, A, B}) -> ice_primop:'or'(A, B);
prepare({'and', _, A, B}) -> ice_primop:'and'(A, B);
prepare({'+', _, A   }) -> ice_primop:plus(A);
prepare({'-', _, A   }) -> ice_primop:minus(A);
prepare({'+', _, A, B}) -> ice_primop:plus(A, B);
prepare({'-', _, A, B}) -> ice_primop:minus(A, B);
prepare({'*', _, A, B}) -> ice_primop:times(A, B);
prepare({'/', _, A, B}) -> ice_primop:divide(A, B);
prepare({'%', _, A, B}) -> ice_primop:mod(A, B);
prepare({bool, _, Bool}) -> {bool, Bool};
prepare({char, _, Char}) -> {char, Char};
prepare({int, _, Int}) -> {int, Int};
prepare({float, _, Float}) -> {float, Float};
prepare({raw_string, _, S})    -> {string, S};
prepare({cooked_string, _, S}) -> {string, S};
prepare({'@', _, A, B}) -> {'@', A, B};
prepare({id,_,Name}) -> {id, Name}.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------
map_prepare(Xs) ->
  lists:map(fun ice_ast:prepare/1, Xs).

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
