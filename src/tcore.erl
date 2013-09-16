%%-------------------------------------------------------------------------------------
%% Evaluator
%%-------------------------------------------------------------------------------------
-module(tcore).

-export([eval/7]).

%%-------------------------------------------------------------------------------------
%% Evaluator
%%-------------------------------------------------------------------------------------
%%-------------------------------------------------------------------------------------
%% Constant values
%%-------------------------------------------------------------------------------------
eval(Const, _I, _E, _K, _D, _W, T) when is_number(Const) ->
  {Const, T};

eval(Bool, _I, _E, _K, _D, _W, T) when is_boolean(Bool) ->
  {Bool, T};

eval({string, Str}, _I, _E, _K, _D, _W, T) ->
  {{string, Str}, T};

%%-------------------------------------------------------------------------------------
%% Constant dimensions
%%-------------------------------------------------------------------------------------
eval({'?', Dim}, I, E, K, D, W, T) ->
  eval({'#', Dim}, I, E, K, D, W, T);

%%-------------------------------------------------------------------------------------
%% Primop
%%-------------------------------------------------------------------------------------
eval({primop, F, Eis}, I, E, K, D, W, T) ->
  {Dis, MaxT} = tpar:eval(Eis, I, E, K, D, W, T),
  case tset:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis1} ->
      {apply(F, Dis1), MaxT}
  end;
      
%%-------------------------------------------------------------------------------------
%% Tuple Expressions
%%-------------------------------------------------------------------------------------
eval({t, Es}, I, E, K, D, W, T) ->
  {Dis, MaxT} = tpar:eval_tuple(Es, I, E, K, D, W, T),
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
%% Wherevar
%%-------------------------------------------------------------------------------------
eval({wherevar, E0, XiEis}, I, E, K, D, W, T) ->
  eval(E0, I, tset:perturb(E, XiEis), K, D, W, T);

%%-------------------------------------------------------------------------------------
%% Query
%%-------------------------------------------------------------------------------------
eval({'#', E0}, I, E, K, D, W, T) ->
  {D0, T0} = eval(E0, I, E, K, D, W, T),
  case tset:is_k(D0) of
    true ->
      {D0, T0};
    false ->
      case lists:member(D0, D) of
	      true ->
	        {lookup_ordinate(D0, K), T0};
	      false ->
	        {[D0], T0}
      end
  end;

%%-------------------------------------------------------------------------------------
%% base application / abstraction
%%-------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------
%% wheredim
%%-------------------------------------------------------------------------------------
eval({wheredim, E0, XiEis}, I, E, K, D, W, T) ->
  {Xis, Eis} = lists:unzip(XiEis),
  {Dis, MaxT} = tpar:eval(Eis, I, E, K, D, W, T),
  case tset:union_d(Dis) of
    {true, Dims} ->
      {Dims, MaxT};
    {false, Dis} ->
      Ki1 = tset:perturb(K, lists:zip(Xis, Dis)),
      eval(E0, I, E, Ki1, D, W, MaxT)
  end;

%%-------------------------------------------------------------------------------------
%% identifiers = Xi
%%-------------------------------------------------------------------------------------
eval({Pos,_}=Xi, _I, _E, _K, _D, _W, T) when is_list(Pos) ->
  {Xi, T};

eval(Xi, I, E, K, D, W, T) when is_list(Xi) orelse is_atom(Xi) ->
  {{D0, T0}, Dims} = eval1(Xi, I, E, K, [], W, T),
  tcache:find(Xi, K, Dims, W, T).

%%-------------------------------------------------------------------------------------
%% Finding identifiers in the cache and building a
%%-------------------------------------------------------------------------------------
eval1(Xi, I, E, K, D, W, T) ->
  {D0, T0} = eval2(Xi, I, E, K, D, W, T),
  case tset:is_k(D0) andalso tset:subset(D0, tset:domain(K)) of
    true -> 
      eval1(Xi, I, E, K, tset:union(D, D0), W, T);
    false ->
      {{D0, T0}, D}
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
	        {error, undefined_id, Xi}
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
  {error, dimension_undefined};
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

