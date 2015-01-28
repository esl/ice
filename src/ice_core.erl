%%-------------------------------------------------------------------------------------
%% Evaluator
%%-------------------------------------------------------------------------------------
-module(ice_core).

-export([eval/6]).

%%-------------------------------------------------------------------------------------
%% Dimension Identifiers replacing formal parameters in abstractions
%%-------------------------------------------------------------------------------------

eval({dim,_Pi}=Di, _I, _E, _K, _D, _W) ->
  Di;

%%-------------------------------------------------------------------------------------
%% Constant Values
%%-------------------------------------------------------------------------------------

eval({bool, Bool}, _I, _E, _K, _D, _W) ->
  Bool;

eval({char, Char}, _I, _E, _K, _D, _W) ->
  Char;

eval({int, Int}, _I, _E, _K, _D, _W) ->
  Int;

eval({float, Float}, _I, _E, _K, _D, _W) ->
  Float;

eval({string, Str}, _I, _E, _K, _D, _W) ->
  Str;

%%-------------------------------------------------------------------------------------
%% Sequence
%%-------------------------------------------------------------------------------------

eval({seq, E0, E1}, I, E, K, D, W) ->
  eval_seq([E0, E1], I, E, K, D, W);

%%-------------------------------------------------------------------------------------
%% Primop
%%-------------------------------------------------------------------------------------

eval({primop, Op, Eis}, I, E, K, D, W) ->
%%  io:format(user, "Evaluating primop ~p ~p~nK = ~p~nD = ~p~n", [Op, Eis, K, D]),
  Dis = ice_par:eval(Eis, I, E, K, D, W),
%%  io:format(user, "Primop parameters = ~p~n", [Dis]),
  case ice_sets:union_d(Dis) of
    {true, Dims} ->
      Dims;
    false ->
      apply(ice_primop_eval, Op, Dis)
  end;

%%-------------------------------------------------------------------------------------
%% Tuple Expressions
%%-------------------------------------------------------------------------------------

eval({t, Es}, I, E, K, D, W) ->
  XiEis = lists:flatmap(fun({Xi, Ei}) -> [Xi, Ei] end, Es),
  %% XXX Does evaluating lhs make sense if dims are not ground values?
  Dis = ice_par:eval(XiEis, I, E, K, D, W), 
  case ice_sets:union_d(Dis) of
    {true, Dims} ->
      Dims;
    false ->
      Tuple = lists:zip(odd_elements(Dis), even_elements(Dis)),
      {te, Tuple}
  end;

%%-------------------------------------------------------------------------------------
%% Context Perturbation
%%-------------------------------------------------------------------------------------

eval({'@', E0, E1}, I, E, K, D, W) ->
  Di = eval(E1, I, E, K, D, W),
  case ice_sets:is_k(Di) of
    true ->
      lists:filter(fun ice_sets:is_d/1, Di);
    false ->
      {te, Di2} = Di,
      Ki = ice_sets:perturb(K, Di2),
      Di3 = ice_sets:union(D, ice_sets:domain(Di2)),
      eval(E0, I, E, Ki, Di3, W)
  end;

%%-------------------------------------------------------------------------------------
%% Conditional
%%-------------------------------------------------------------------------------------

eval({'if', E0, E1, E2}, I, E, K, D, W) ->
  D0 = eval(E0, I, E, K, D, W),
  case ice_sets:is_k(D0) of
    true ->
      D0;
    false ->
      case D0 of
	true -> 
	  eval(E1, I, E, K, D, W);
	false -> 
	  eval(E2, I, E, K, D, W)
      end
  end;

%%-------------------------------------------------------------------------------------
%% Dimensional Query
%%-------------------------------------------------------------------------------------

eval({Q, E0}, I, E, K, D, W) when Q == '#' orelse Q == '?' ->
  D0 = eval(E0, I, E, K, D, W),
  case ice_sets:is_k(D0) of
    true ->
      D0;
    false ->
      case lists:member(D0, D) of
        true ->
          lookup_ordinate(D0, K);
        false ->
          [D0]
      end
  end;

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------

eval({b_abs, Intensions, H, ArgDims, E0}, I, E, K, D, W) ->
  Vis = ice_par:eval(Intensions, I, E, K, D, W),
  case ice_sets:union_d(Vis) of
    {true, Dims} ->
      Dims;
    false ->
      K1 = ice_sets:restrict_domain(K, ice_sets:union(Vis, H)),
      D1 = ice_sets:union(Vis, ArgDims),
      {closure, K1, D1, E0}
  end;

eval({b_apply, E0, Eis, [N]}, I, E, K, D, W) ->
  E0Eis = ice_par:eval([E0|Eis], I, E, K, D, W),
  case ice_sets:union_d(E0Eis) of
    {true, Dims} ->
      Dims;
    false ->
      [{closure, K1, D1, Body}|Eis1] = E0Eis,
      HParams = map_params([N+1], Eis1),
      K2 = ice_sets:perturb(K1, HParams),
      eval(Body, I, E, K2, D1, W)
  end;
      
%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------

eval({v_abs, Intensions, H, ArgDims, E0}, I, E, K, D, W) ->
  Vis = ice_par:eval(Intensions, I, E, K, D, W),
  case ice_sets:union_d(Vis) of
    {true, Dims} ->
      Dims;
    false ->
      K1 = ice_sets:restrict_domain(K, ice_sets:union(Vis, H)),
      D1 = ice_sets:union(Vis, ArgDims),
      {closure, K1, D1, E0}
  end;

eval({v_apply, E0, Eis, [N]}, I, E, K, D, W) ->
  E0Eis = ice_par:eval([E0|Eis], I, E, K, D, W),
  case ice_sets:union_d(E0Eis) of
    {true, Dims} ->
      Dims;
    false ->
      [{closure, K1, D1, Body}|Eis1] = E0Eis,
      HParams = map_params([N+1], Eis1),
      K2 = ice_sets:perturb(ice_sets:perturb(K, K1), HParams),
      D2 = ice_sets:union(D, D1),
      eval(Body, I, E, K2, D2, W)
  end;

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------

eval({i_abs, Intensions, H, E0}, I, E, K, D, W) ->
  Vis = ice_par:eval(Intensions, I, E, K, D, W),
  case ice_sets:union_d(Vis) of
    {true, Dims} ->
      Dims;
    false ->
      K1 = ice_sets:restrict_domain(K, ice_sets:union(Vis, H)),
      D1 = ice_sets:union(Vis, H),
      {closure, K1, D1, E0}
  end;

eval({i_apply, E0}, I, E, K, D, W) ->
  Vi = eval(E0, I, E, K, D, W),
  case ice_sets:union_d(Vi) of
    {true, Dims} ->
      Dims;
    false ->
      {closure, K1, D1, Body} = Vi,
      K2 = ice_sets:perturb(K, K1),
      D2 = ice_sets:union(D, D1),
      eval(Body, I, E, K2, D2, W)
  end;

%%-------------------------------------------------------------------------------------
%% Wherevar
%%-------------------------------------------------------------------------------------

eval({wherevar, E0, XiEis}, I, E, K, D, W) ->
  eval(E0, I, ice_sets:perturb(E, XiEis), K, D, W);

%%-------------------------------------------------------------------------------------
%% Wheredim
%%-------------------------------------------------------------------------------------

eval({wheredim, _Q, Rho, E0, XiEis}, I, E, K, D, W) ->
  {Xis, Eis} = lists:unzip(XiEis),
  Vis = ice_par:eval(Eis, I, E, K, D, W),
  K1 =
    case lookup_ordinate(Rho, K) of
      false ->
        ice_sets:perturb(K, [{Rho, 0}]);
      N ->
        ice_sets:perturb(K, [{Rho, N+1}])
    end,
  XiVis = lists:zip(Xis, Vis),
  K2 = ice_sets:perturb(K1, XiVis),
  eval(E0, I, E, K2, D, W);

%%-------------------------------------------------------------------------------------
%% Variable Identifiers
%%-------------------------------------------------------------------------------------

eval({id,_} = Xi, I, E, K, _D, W) ->
  eval1(Xi, I, E, K, [], W).

%%-------------------------------------------------------------------------------------
%% Finding identifiers in the cache
%%-------------------------------------------------------------------------------------
eval1(Xi, I, E, K, D, W) ->
  D0 = eval2(Xi, I, E, K, D, W),
  case ice_sets:is_k(D0) andalso ice_sets:subset(D0, ice_sets:domain(K)) of
    true ->
      case ice_sets:difference(D0, D) of
        [] ->
          {error, loop_detected, {already_known_dimensions, D0}};
        _ ->
          eval1(Xi, I, E, K, ice_sets:union(D, D0), W)
      end;
    false ->
      D0
  end.

eval2(Xi, I, E, K, D, W) ->
  D0 = ice_cache:find(Xi, K, D, W),
  case D0 of
    {calc, W} ->
      case lists:keyfind(Xi, 1, E) of
        {_, E0} ->
          D1 = eval(E0, I, E, K, D, W),
          ice_cache:add(Xi, K, D, W, D1);
        false ->
          {error, undefined_identifier, Xi}
      end;
    {calc, _W1} ->
      eval2(Xi, I, E, K, D, W);
    _ ->
      D0
  end.

%%-------------------------------------------------------------------------------------
%% @doc Evaluate a sequence expressions 
%%-------------------------------------------------------------------------------------

eval_seq(Xs, I, E, K, D, W) ->
  eval_seq(Xs, I, E, K, D, W, []).

eval_seq([], _I, _E, _K, _D, _W, Acc) ->
  hd(Acc);
eval_seq([X|Xs], I, E, K, D, W, Acc) ->
  D0 = eval(X, I, E, K, D, W),
  %% FIXME Check missing dimensions before progressing?
  eval_seq(Xs, I, E, K, D, W, [D0|Acc]).

%%-------------------------------------------------------------------------------------
%% Internal
%%-------------------------------------------------------------------------------------

map_params(P, Eis) ->
  Is = lists:seq(1, length(Eis)),
  lists:map(
    fun ({Ei, I}) ->
        {{dim, [I|P]}, Ei}
    end,
    lists:zip(Eis, Is)
   ).

lookup_ordinate(_, []) ->
  false;
lookup_ordinate(D, [{D,Ordinate}|_]) ->
  Ordinate;
lookup_ordinate(D, [_|K]) ->
  lookup_ordinate(D, K).

odd_elements(L) ->
  odd_elements(L, 0, []).

odd_elements([], _, Acc) ->
  lists:reverse(Acc);
odd_elements([X|L], N, Acc) when N rem 2 == 0 ->
  odd_elements(L, N+1, [X|Acc]);
odd_elements([_|L], N, Acc) ->
  odd_elements(L, N+1, Acc).

even_elements(L) ->
  even_elements(L, 0, []).

even_elements([], _, Acc) ->
  lists:reverse(Acc);
even_elements([X|L], N, Acc) when N rem 2 =/= 0 ->
  even_elements(L, N+1, [X|Acc]);
even_elements([_|L], N, Acc) ->
  even_elements(L, N+1, Acc).

