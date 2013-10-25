%%-------------------------------------------------------------------------------------
%% Evaluator
%%-------------------------------------------------------------------------------------
-module(tcore).

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
%% Primop
%%-------------------------------------------------------------------------------------
eval({primop, Primop, Eis}, I, E, K, D, W, T) ->
  {Dis, MaxT} = tpar:eval(Eis, I, E, K, D, W, T),
  case tset:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis1} ->
      F = case Primop of
        '%' ->
          fun %% http://stackoverflow.com/a/858649/1418165
            (X, Y) -> (X rem Y + Y) rem Y
          end;
        'ilogn' ->
          fun (0) -> 0;
              (X) -> round(math:log(X) / math:log(2))
          end;
        Op -> fun erlang:Op/2
      end,
      {apply(F, Dis1), MaxT}
  end;

%%-------------------------------------------------------------------------------------
%% Tuple Expressions
%%-------------------------------------------------------------------------------------
eval({t, Es}, I, E, K, D, W, T) ->
  XiEis = lists:flatmap(fun({Xi,Ei}) -> [Xi,Ei] end, Es),
  {Dis, MaxT} = tpar:eval(XiEis, I, E, K, D, W, T), %% XXX Does evaluating lhs make sense if dims are not ground values?
  case tset:union_d(Dis) of
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
  case tset:is_k(Di) of
    true ->
      {lists:filter(fun tset:is_d/1, Di), T1};
    false ->
      {te, Di2} = Di,
      Ki = tset:perturb(K, Di2),
      Di3 = tset:union(D, tset:domain(Di2)),
      eval(E0, I, E, Ki, Di3, W, T1)
  end;

%%-------------------------------------------------------------------------------------
%% Conditional
%%-------------------------------------------------------------------------------------
eval({'if', E0, E1, E2}, I, E, K, D, W, T) ->
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case tset:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      Branch = case D0 of
        true -> E1;
        false -> E2
      end,
      eval(Branch, I, E, K, D, W, T0)
  end;

%%-------------------------------------------------------------------------------------
%% Dimensional Query
%%-------------------------------------------------------------------------------------
eval({Q, E0}, I, E, K, D, W, T) when Q == '#' orelse Q == '?' ->
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case tset:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      case lists:member(D0, D) of
        true ->
          DimType =
            case Q of
              '#' -> dim;
              '?' -> phi
            end,
          DimType = element(1, D0), %% Hardcoded expectation
          {lookup_ordinate(D0, K), T0};
        false ->
          {[D0], T0}
      end
  end;

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
eval({b_abs, Is, _Params, _E0}=Abs, I, E, K, D, W, T) ->
  eval_abs(Is, Abs, I, E, K, D, W, T);

eval({b_apply, E0, Eis}, I, E, K, D, W, T) ->
  %% The evaluation of abs is serialized from the evaluation of actual
  %% parameters for re-using the context perturbation '@' expression
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case tset:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      {frozen_b_abs, AbsI, AbsE, FrozenK, AbsParams, AbsBody} = D0,
      eval({'@', AbsBody, {t, lists:zip(AbsParams, Eis)}},
           AbsI, AbsE, FrozenK, tset:domain(FrozenK), W, T0)
  end;

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
eval({v_abs, Is, _Params, _E0}=Abs, I, E, K, D, W, T) ->
  eval_abs(Is, Abs, I, E, K, D, W, T);

eval({v_apply, E0, Eis}, I, E, K, D, W, T) ->
  %% The evaluation of abs is serialized from the evaluation of actual
  %% parameters for re-using the context perturbation '@' expression
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case tset:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      {frozen_v_abs, AbsI, AbsE, FrozenK, AbsParams, AbsBody} = D0,
      eval({'@', AbsBody, {t, lists:zip(AbsParams, Eis)}}, AbsI, AbsE,
           tset:perturb(K, FrozenK), tset:union(D, tset:domain(FrozenK)),
           W, T0)
  end;

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
eval({i_abs, Is, _E0}=Abs, I, E, K, D, W, T) ->
  eval_abs(Is, Abs, I, E, K, D, W, T);

eval({i_apply, E0}, I, E, K, D, W, T) ->
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case tset:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      {frozen_i_abs, AbsI, AbsE, FrozenK, AbsBody} = D0,
      eval(AbsBody, AbsI, AbsE,
           tset:perturb(K, FrozenK), tset:union(D, tset:domain(FrozenK)),
           W, T0)
  end;

%%-------------------------------------------------------------------------------------
%% Wherevar
%%-------------------------------------------------------------------------------------
eval({wherevar, E0, XiEis}, I, E, K, D, W, T) ->
  eval(E0, I, tset:perturb(E, XiEis), K, D, W, T);

%%-------------------------------------------------------------------------------------
%% Wheredim
%%-------------------------------------------------------------------------------------
eval({wheredim, E0, XiEis}, I, E, K, D, W, T) ->
  {Xis, Eis} = lists:unzip(XiEis),
  {Dis, MaxT} = tpar:eval(Eis, I, E, K, D, W, T),
  case tset:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis} ->
      Ki1 = tset:perturb(K, lists:zip(Xis, Dis)),
      %% XXX It is unclear if legal or illegal programs violating the
      %% following hardcoded expectation exist.
      [] = tset:intersection(D, Xis),
      %% The hidden dimensions shall be added by the wheredim rule to
      %% the set of known dimensions (the rule in the paper
      %% "Multidimensional Infinite Data in the Language Lucid", Feb
      %% 2013, needs this correction re Delta) otherwise the body
      %% cannot use them.
      Di1 = tset:union(D, Xis),
      eval(E0, I, E, Ki1, Di1, W, MaxT)
  end;

%%-------------------------------------------------------------------------------------
%% Dimension Identifiers (public)
%%-------------------------------------------------------------------------------------
eval({dim,Xi}=Di, _I, _E, _K, _D, _W, T) when is_list(Xi) orelse is_atom(Xi) ->
  {Di, T};

%%-------------------------------------------------------------------------------------
%% Dimension Identifiers (hidden) replacing local dimensions in wheredim clauses
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
  {_D0, _T0} = eval1(Xi, I, E, tset:restrict_domain(K, D), [], W, T).

%%-------------------------------------------------------------------------------------
%% Finding identifiers in the cache
%%-------------------------------------------------------------------------------------
eval1(Xi, I, E, K, D, W, T) ->
  {D0, T0} = eval2(Xi, I, E, K, D, W, T),
  case tset:is_k(D0) andalso tset:subset(D0, tset:domain(K)) of
    true -> 
      eval1(Xi, I, E, K, tset:union(D, D0), W, T);
    false ->
      {D0, T0}
  end.

eval2(Xi, I, E, K, D, W, T) ->
  {D0, T0} = tcache:find(Xi, K, D, W, T),
  case D0 of
    {calc, W} ->
      case lists:keyfind(Xi, 1, E) of
        {_, E0} ->
          {D1, T1} = eval(E0, I, E, K, D, W, T0),
          tcache:add(Xi, K, D, W, T1, D1);
        false ->
          {error, undefined_identifier, Xi}
      end;
    {calc, _W1} ->
      eval2(Xi, I, E, K, D, W, T0 + 1);
    _ ->
      {D0, T0}
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


eval_abs(Is, Abs, I, E, K, D, W, T) ->
  {Dis, MaxT} = tpar:eval(Is, I, E, K, D, W, T),
  case tset:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis1} -> %% XXX Why Dis1 even if equal to Dis?
      case tset:difference(Dis1, D) of
        [] ->
          KD = tset:restrict_domain(K, D),
          FrozenK = tset:restrict_domain(KD, Dis1),
          {freeze_abs(I, E, FrozenK, Abs), MaxT};
        Dims2 -> %% Missing frozen dims
          {Dims2, MaxT}
      end
  end.

freeze_abs(I, E, FrozenK, {i_abs, _Is, E0}) ->
  {frozen_i_abs, I, E, FrozenK, E0};
freeze_abs(I, E, FrozenK, {b_abs, _Is, Params, E0}) ->
  {frozen_b_abs, I, E, FrozenK, Params, E0};
freeze_abs(I, E, FrozenK, {v_abs, _Is, Params, E0}) ->
  {frozen_v_abs, I, E, FrozenK, Params, E0}.
