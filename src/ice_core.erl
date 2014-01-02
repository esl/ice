%%-------------------------------------------------------------------------------------
%% Evaluator
%%-------------------------------------------------------------------------------------
-module(ice_core).

-export([eval/7]).

%%-------------------------------------------------------------------------------------
%% Constant Values
%%-------------------------------------------------------------------------------------
eval(Const, _I, _E, _K, _D, _W, T) when is_number(Const) orelse is_boolean(Const) ->
  {Const, T};

eval({string, Str}, _I, _E, _K, _D, _W, T) ->
  {{string, Str}, T};

eval({char, Char}, _I, _E, _K, _D, _W, T) ->
  {{char, Char}, T};

%%-------------------------------------------------------------------------------------
%% Sequence
%%-------------------------------------------------------------------------------------
eval({seq, Eis}, I, E, K, D, W, T) ->
  eval_seq(Eis, I, E, K, D, W, T);

%%-------------------------------------------------------------------------------------
%% Primop
%%-------------------------------------------------------------------------------------
eval({primop, Primop, Eis}, I, E, K, D, W, T) ->
  {Dis, MaxT} = ice_par:eval(Eis, I, E, K, D, W, T),
  case ice_sets:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis1} ->
      F = ice_primop:f(Primop),
      {apply(F, Dis1), MaxT}
  end;

%%-------------------------------------------------------------------------------------
%% Tuple Expressions
%%-------------------------------------------------------------------------------------
eval({t, Es}, I, E, K, D, W, T) ->
  XiEis = lists:flatmap(fun({Xi,Ei}) -> [Xi,Ei] end, Es),
  {Dis, MaxT} = ice_par:eval(XiEis, I, E, K, D, W, T), %% XXX Does evaluating lhs make sense if dims are not ground values?
  case ice_sets:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis1} ->
      Tuple = lists:zip(odd_elements(Dis1), even_elements(Dis1)),
      {{te, Tuple}, MaxT}
  end;

%%-------------------------------------------------------------------------------------
%% Context Perturbation
%%-------------------------------------------------------------------------------------
eval({'@', E0, E1}, I, E, K, D, W, T) ->
  {Di, T1} = eval(E1, I, E, K, D, W, T),
  case ice_sets:is_k(Di) of
    true ->
      {lists:filter(fun ice_sets:is_d/1, Di), T1};
    false ->
      {te, Di2} = Di,
      Ki = ice_sets:perturb(K, Di2),
      Di3 = ice_sets:union(D, ice_sets:domain(Di2)),
      eval(E0, I, E, Ki, Di3, W, T1)
  end;

%%-------------------------------------------------------------------------------------
%% Conditional
%%-------------------------------------------------------------------------------------
eval({'if', E0, E1, E2}, I, E, K, D, W, T) ->
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case ice_sets:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      case D0 of
	true -> 
	  eval(E1, I, E, K, D, W, T0);
	false -> 
	  eval(E2, I, E, K, D, W, T0)
      end
  end;

%%-------------------------------------------------------------------------------------
%% Dimensional Query
%%-------------------------------------------------------------------------------------
eval({Q, E0}, I, E, K, D, W, T) when Q == '#' orelse Q == '?' ->
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case ice_sets:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      case lists:member(D0, D) of
        true ->
          case {Q, D0} of
            {'?', {phi,_}} ->
              ok;
            {'#', {dim,_,_}} ->
              ok;
            {'#', _} ->
              io:format("Querying (~p) context for dimension ~p~n", [Q, D0])
          end,
          {lookup_ordinate(D0, K), T0};
        false ->
          {[D0], T0}
      end
  end;

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
eval({b_abs, _Is, _Params, _E0}=Abs, I, E, K, D, W, T) ->
  freeze_closure(ice_closure:close_abs(Abs, I, E), I, E, K, D, W, T);

eval({b_apply, E0, Eis}, I, E, K, D, W, T) ->
  {D0is, MaxT} = ice_par:eval([E0 | Eis], I, E, K, D, W, T),
  case ice_sets:union_d(D0is) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, D0is1} -> %% XXX Why Dis1 even if equal to Dis?
      [D0 | Dis] = D0is1,
      {frozen_closed_b_abs, ClI, ClE, FrozenK, AbsParams, AbsBody} = D0,
      AbsParamsK = lists:zip(AbsParams, Dis),
      FPK = ice_sets:perturb(FrozenK, AbsParamsK),
      eval(AbsBody, ClI, ClE,
           FPK, ice_sets:domain(FPK),
           W, MaxT)
  end;

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
eval({v_abs, _Is, _Params, _E0}=Abs, I, E, K, D, W, T) ->
  freeze_closure(ice_closure:close_abs(Abs, I, E), I, E, K, D, W, T);

eval({v_apply, E0, Eis}, I, E, K, D, W, T) ->
  {D0is, MaxT} = ice_par:eval([E0 | Eis], I, E, K, D, W, T),
  case ice_sets:union_d(D0is) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, D0is1} -> %% XXX Why Dis1 even if equal to Dis?
      [D0 | Dis] = D0is1,
      {frozen_closed_v_abs, ClI, ClE, FrozenK, AbsParams, AbsBody} = D0,
      AbsParamsK = lists:zip(AbsParams, Dis),
      FPK = ice_sets:perturb(FrozenK, AbsParamsK),
      eval(AbsBody, ClI, ClE,
           ice_sets:perturb(K, FPK), ice_sets:union(D, ice_sets:domain(FPK)),
           W, MaxT)
  end;

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
eval({i_abs, _Is, _E0}=Abs, I, E, K, D, W, T) ->
  freeze_closure(ice_closure:close_abs(Abs, I, E), I, E, K, D, W, T);

eval({i_apply, E0}, I, E, K, D, W, T) ->
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case ice_sets:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      {frozen_closed_i_abs, ClI, ClE, FrozenK, AbsBody} = D0,
      eval(AbsBody, ClI, ClE,
           ice_sets:perturb(K, FrozenK), ice_sets:union(D, ice_sets:domain(FrozenK)),
           W, T0)
  end;

%%------------------------------------------------------------------------------
%% Closure of abstraction over enviroment
%%------------------------------------------------------------------------------
eval({closure, _ClI, _ClE, _Abs}=Cl, I, E, K, D, W, T) ->
  freeze_closure(Cl, I, E, K, D, W, T);

%%-------------------------------------------------------------------------------------
%% Wherevar
%%-------------------------------------------------------------------------------------
eval({wherevar, E0, XiEis}, I, E, K, D, W, T) ->
  %% Close shallowest abstractions in new expressions in environment
  %% if needed. Wherevar is the only expression changing the
  %% environment, therefore the only one needing to do this.
  XiClEis = ice_closure:close_shallowest_abs_in_wherevar_expressions(XiEis, I, E),
  eval(E0, I, ice_sets:perturb(E, XiClEis), K, D, W, T);

%%-------------------------------------------------------------------------------------
%% Wheredim
%%-------------------------------------------------------------------------------------
eval({wheredim, E0, XiEis}, I, E, K, D, W, T) ->
  {Xis, Eis} = lists:unzip(XiEis),
  {Dis, MaxT} = ice_par:eval(Eis, I, E, K, D, W, T),
  case ice_sets:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis} ->
      Ki1 = ice_sets:perturb(K, lists:zip(Xis, Dis)),
      %% XXX It is unclear if legal or illegal programs violating the
      %% following hardcoded expectation exist.
      [] = ice_sets:intersection(D, Xis),
      %% The hidden dimensions shall be added by the wheredim rule to
      %% the set of known dimensions (the rule in the paper
      %% "Multidimensional Infinite Data in the Language Lucid", Feb
      %% 2013, needs this correction re Delta) otherwise the body
      %% cannot use them.
      Di1 = ice_sets:union(D, Xis),
      eval(E0, I, E, Ki1, Di1, W, MaxT)
  end;

%%-------------------------------------------------------------------------------------
%% Dimension Identifiers replacing local dimensions in wheredim clauses
%%-------------------------------------------------------------------------------------
eval({dim,{_Pos,_Idx},Xi}=Di, _I, _E, _K, _D, _W, T) when is_list(Xi) orelse is_atom(Xi) ->
  {Di, T};

%%-------------------------------------------------------------------------------------
%% Dimension Identifiers replacing formal parameters in abstractions
%%-------------------------------------------------------------------------------------
eval({phi,Xi}=Di, _I, _E, _K, _D, _W, T) when is_list(Xi) orelse is_atom(Xi) ->
  {Di, T};

%%-------------------------------------------------------------------------------------
%% Variable Identifiers
%%-------------------------------------------------------------------------------------
eval(Xi, I, E, K, D, W, T) when is_list(Xi) orelse is_atom(Xi) ->
  %% This rule differs from the one described in the Feb 2013 cache
  %% semantics paper in order to avooid to return a calc value in case
  %% of GC concurrent with the invocation of beta.find().
  %%
  %% XXX Can GC be concurrent in the first place? Answer via email by
  %% John Plaice indicates that "the collect cannot be run
  %% simultaneously".
  %%
  %% The rule as per Feb 2013 cache semantics paper has (or at least
  %% Luca thinks it has) the aim of resetting to 0 the age of the
  %% queried value (and upstream chain). The removal of the final call
  %% to beta.find() nullifies such (alleged) aim.
  %%
  %% In order to restore such aim, a new instruction shall be designed
  %% and implemented for the cache, beta.pseudo_find(), whose aim is
  %% traversing the tree in search of the specified (x,k) and
  %% resetting beta.age (whatever it is) and gamma_j.age in all the
  %% chain until the position of the node is reached. If
  %% beta.data(x,k) is not defined, *no calc<w> node shall be
  %% created*. Probably, GC shall be triggered by this instruction
  %% too.
  {_D0, _T0} = eval1(Xi, I, E, ice_sets:restrict_domain(K, D), [], W, T).

%%-------------------------------------------------------------------------------------
%% Finding identifiers in the cache
%%-------------------------------------------------------------------------------------
eval1(Xi, I, E, K, D, W, T) ->
  {D0, T0} = eval2(Xi, I, E, K, D, W, T),
  case ice_sets:is_k(D0) andalso ice_sets:subset(D0, ice_sets:domain(K)) of
    true ->
      case ice_sets:difference(D0, D) of
        [] ->
          {error, loop_detected, {already_known_dimensions, D0}};
        _ ->
          eval1(Xi, I, E, K, ice_sets:union(D, D0), W, T)
      end;
    false ->
      {D0, T0}
  end.

eval2(Xi, I, E, K, D, W, T) ->
  {D0, T0} = ice_cache:find(Xi, K, D, W, T),
  case D0 of
    {calc, W} ->
      case lists:keyfind(Xi, 1, E) of
        {_, E0} ->
          {D1, T1} = eval(E0, I, E, K, D, W, T0),
          ice_cache:add(Xi, K, D, W, T1, D1);
        false ->
          {error, undefined_identifier, Xi}
      end;
    {calc, _W1} ->
      eval2(Xi, I, E, K, D, W, T0 + 1);
    _ ->
      {D0, T0}
  end.

%%-------------------------------------------------------------------------------------
%% @doc Evaluate a sequence expressions 
%%-------------------------------------------------------------------------------------
eval_seq(Xs, I, E, K, D, W, T) ->
  eval_seq(Xs, I, E, K, D, W, T, []).

eval_seq([], I, E, K, D, W, T, Acc) ->
  {hd(lists:reverse(Acc)), T};
eval_seq([X|Xs], I, E, K, D, W, T, Acc) ->
  {D0, T1} = eval(X, I, E, K, D, W, T),
  %% FIXME Check missing dimensions before progressing?
  case T1 > T of
    true ->
      eval_seq(Xs, I, E, K, D, W, T1, [D0|Acc]);
    false ->
      eval_seq(Xs, I, E, K, D, W, T, [D0|Acc])
  end.

%%-------------------------------------------------------------------------------------
%% Internal
%%-------------------------------------------------------------------------------------
lookup_ordinate(_, []) ->
  {error, undefined_dimension};
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

%%------------------------------------------------------------------------------
%% @doc Freeze closure.
%% @private
%%------------------------------------------------------------------------------
freeze_closure({closure, _ClI, _ClE, Abs}=Cl, I, E, K, D, W, T) ->
  Is = frozen_expressions(Abs),
  {Dis, MaxT} = ice_par:eval(Is, I, E, K, D, W, T),
  case ice_sets:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis1} -> %% XXX Why Dis1 even if equal to Dis?
      case ice_sets:difference(Dis1, D) of
        [] ->
          KD = ice_sets:restrict_domain(K, D),
          FrozenK = ice_sets:restrict_domain(KD, Dis1),
          {freeze_closure_in_k(Cl, FrozenK), MaxT};
        Dims2 -> %% Missing frozen dims
          {Dims2, MaxT}
      end
  end.

frozen_expressions({b_abs, Is, _Params, _E0}) -> Is;
frozen_expressions({v_abs, Is, _Params, _E0}) -> Is;
frozen_expressions({i_abs, Is,          _E0}) -> Is.

freeze_closure_in_k({closure, ClI, ClE, {b_abs, _Is, Params, E0}}, FrozenK) ->
  {frozen_closed_b_abs, ClI, ClE, FrozenK, Params, E0};
freeze_closure_in_k({closure, ClI, ClE, {v_abs, _Is, Params, E0}}, FrozenK) ->
  {frozen_closed_v_abs, ClI, ClE, FrozenK, Params, E0};
freeze_closure_in_k({closure, ClI, ClE, {i_abs, _Is,         E0}}, FrozenK) ->
  {frozen_closed_i_abs, ClI, ClE, FrozenK,         E0}.
