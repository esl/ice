%%------------------------------------------------------------------------------
%% AST Transformation - Pass 1
%%------------------------------------------------------------------------------
-module(ttransform1).

-export([transform1/1]).
-export([test/0]).

%%------------------------------------------------------------------------------
%% This transformation module transforms local dimension identifiers
%% and formal parameters.
%%
%% Expressions whose transformation is not straightforward are:
%% wheredim clauses, intension / base / value abstractions, wherevar
%% clauses, identifiers.
%%
%% Local dimension identifiers are declared in wheredim clauses. Such
%% identifiers (e.g. "t") are transformed into respective unique
%% hidden dimensions (e.g. {dim, {Pos,Idx}, "t"}). References to such
%% dimensions in the wheredim's body are transformed to equivalent
%% references to the respective dimensions (e.g. "t" -> {dim,
%% {Pos,Idx}, "t"}).
%%
%% Formal parameters are found in abstractions. Such parameters
%% (e.g. "t") are transformed into respective hidden dimensions
%% (e.g. {phi, "t"}). References to such parameters in the
%% abstraction's body are transformed to context queries of the
%% respective dimensions (e.g. "t" -> {'?', {phi, "t"}}).
%%
%% Local dimension identifiers, formal parameters and local variable
%% identifiers share the same domain, i.e. identifier "t" can refer
%% either to local dimensions "t" defined in outer wheredim clauses,
%% formal parameters "t" defined in outer base/value abstractions or
%% local variable "t" defined in an outer wherevar clause.  When a new
%% identifier (either local dimension, formal parameter or local
%% variable) is declared, it shadows any outer declarations of
%% homonymous identifiers - i.e. lexical (not dynamic) scoping.  Such
%% shadowing / disambiguation is performed by this transformation
%% modules (i.e. statically at compilation time).
%%
%% Note that this transformation assumes that variables cannot be
%% redefined.
%%------------------------------------------------------------------------------

transform1(E) ->
  transform1(E, root_expr_pos(), []).


-spec transform1(AstIn :: term(), P :: pos(), H) ->
                    AstOut :: term() when
    %% H is the set of hidden dimensions allocated
    H :: [D | V],
    %% D is a hidden dimension allocated for replacing a local
    %% dimension as declared in a wheredim clause
    D :: {dim, hidden_dim(), VarId},
    %% V is a hidden dimension allocated for replacing a formal
    %% parameter as declared in an abstractions
    V :: {phi, VarId},
    VarId :: nonempty_string() | atom().

%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
transform1(Const, _P, _H) when is_number(Const) orelse is_boolean(Const) ->
  Const;

transform1({string, Str}, _P, _H) ->
  {string, Str};

transform1({char, Char}, _P, _H) ->
  {char, Char};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
transform1({primop, F, Eis}, P, H) ->
  Ns = lists:seq(1, length(Eis)), %% 1,2,...
  NewEis =
    lists:map(
      fun({Ei, N}) ->
          transform1(Ei, subexpr_pos(N,P), H) %% Pos 1,2,...
      end,
      lists:zip(Eis, Ns)),
  {primop, F, NewEis};

%%------------------------------------------------------------------------------
%% Tuple Expression
%%------------------------------------------------------------------------------
transform1({t, E0E1is}, P, H) ->
  Ns = lists:seq(1, length(E0E1is)), %% 1,2,...
  NewE0E1is =
    lists:map(
      fun({{E0,E1}, N}) ->
          {transform1(E0, subexpr_pos(N*2  ,P), H), %% Pos 2,4,...
           transform1(E1, subexpr_pos(N*2+1,P), H)} %% Pos 3,5,...
      end,
      lists:zip(E0E1is, Ns)),
  {t, NewE0E1is};

%%------------------------------------------------------------------------------
%% Context Perturbation
%%------------------------------------------------------------------------------
transform1({'@', E0, E1}, P, H) ->
  {'@',
   transform1(E0, subexpr_pos(0,P), H),
   transform1(E1, subexpr_pos(1,P), H)};

%%------------------------------------------------------------------------------
%% Conditional
%%------------------------------------------------------------------------------
transform1({'if', E0, E1, E2}, P, H) ->
  {'if',
   transform1(E0, subexpr_pos(0,P), H),
   transform1(E1, subexpr_pos(1,P), H),
   transform1(E2, subexpr_pos(2,P), H)};

%%------------------------------------------------------------------------------
%% Dimensional Query
%%------------------------------------------------------------------------------
transform1({'#', E0}, P, H) ->
  %% Changing the position in the evaluation tree is not needed as:
  %% * There is only one subexpression
  %% * No hidden dimensions are created in the current expression
  {'#', transform1(E0, P, H)};

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
transform1({b_abs, Is, Params, E}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  ParamsAsDims = lists:map(fun(Param) -> {phi, Param} end, Params),
  {b_abs, transform1_frozen_dims(Is, Ps, H), ParamsAsDims,
   transform1(E, subexpr_pos(0,P), h_store_formal_params(ParamsAsDims, H))};

transform1({b_apply, E0, Eis}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Eis))),
  {b_apply, transform1(E0, subexpr_pos(0,P), H),
   transform1_actual_params(Eis, Ps, H)};

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
transform1({v_abs, Is, Params, E}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  ParamsAsDims = lists:map(fun(Param) -> {phi, Param} end, Params),
  {v_abs, transform1_frozen_dims(Is, Ps, H), ParamsAsDims,
   %% XXX Pos 0?
   transform1(E, subexpr_pos(0,P), h_store_formal_params(ParamsAsDims, H))};

transform1({v_apply, E0, Eis}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Eis))),
  {v_apply, transform1(E0, subexpr_pos(0,P), H),
   %% XXX Shouldn't another context application be here somewhere?
   transform1_actual_params(Eis, Ps, H)};

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
transform1({i_abs, Is, E}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  {i_abs, transform1_frozen_dims(Is, Ps, H),
   %% XXX Pos 0?
   transform1(E, subexpr_pos(0,P), H)};

transform1({i_apply, E}, P, H) ->
  %% XXX Is subexpression with position really needed? And BTW -
  %% shouldn't another context application be here somewhere?
  {i_apply, transform1(E, subexpr_pos(0,P), H)};

%%------------------------------------------------------------------------------
%% Wherevar
%%------------------------------------------------------------------------------
transform1({wherevar, E0, XiEis}, P, H) ->
  {Xis, _Eis} = lists:unzip(XiEis),
  HWoXis = h_delete_vars(Xis, H),
  Ns = lists:seq(1, length(XiEis)), %% 1,2,...
  NewXiEis =
    [{Xi, transform1(Ei, subexpr_pos(N,P), HWoXis)} %% Pos 1,2,...
     || {{Xi, Ei}, N} <- lists:zip(XiEis, Ns)],
  %% XXX The way position is assigned here is completely different
  %% from literature.
  {wherevar, transform1(E0, subexpr_pos(0,P), HWoXis), NewXiEis};

%%------------------------------------------------------------------------------
%% Wheredim
%%------------------------------------------------------------------------------
transform1({wheredim, E0, XiEis}, P, H) ->
  Ns = lists:seq(1, length(XiEis)),
  DimsEis =
    [{ {dim, hidden_dim(N,P), Xi},
       transform1(Ei, subexpr_pos(N,P), H) }
     || {{Xi, Ei}, N} <- lists:zip(XiEis, Ns)],
  {Dims, _Eis} = lists:unzip(DimsEis),
  {wheredim, transform1(E0, subexpr_pos(0,P), h_store_dims(Dims, H)), DimsEis};

%%-------------------------------------------------------------------------------------
%% Identifiers
%%-------------------------------------------------------------------------------------
transform1(Xi, _P, H) when is_list(Xi) orelse is_atom(Xi) ->
  case {lists:keyfind(Xi, 3, H), lists:keyfind(Xi, 2, H)} of
    {{dim,_,Xi}=Dim, false} ->
      %% Replace reference to local dimension of wheredim clause with
      %% previously allocated hidden dimension.
      Dim;
    {false, {phi,Xi}=Phi} ->
      %% Replace reference to formal parameter of abstraction with
      %% context query of previously allocated hidden dimension.
      {'?', Phi};
    {false, false} ->
      %% Presume reference to local variable.
      Xi
  end.


%%-------------------------------------------------------------------------------------
%% Internal - Helpers for transforming abstractions and applications
%%-------------------------------------------------------------------------------------
transform1_frozen_dims(Is, Ps, H) ->
  tset:union([D || D={phi,_} <- H], [transform1(I, P, H) || {I, P} <- lists:zip(Is, Ps)]).

transform1_actual_params(Eis, Ps, H) ->
  lists:map(fun({Ei, P}) -> transform1(Ei, P, H) end, lists:zip(Eis, Ps)).


%%-------------------------------------------------------------------------------------
%% Internal - Helpers for set of hidden dimensions
%%-------------------------------------------------------------------------------------
h_store_dims([], H) ->
  H;
h_store_dims([{dim,_,Xi}=Dim | OtherDims], H) ->
  %% New local dim shadows homonymous outer formal param -if any- ...
  HWoParams = lists:keydelete(Xi, 2, H),
  %% .. or homonymous outer local dim - if any.
  h_store_dims(OtherDims, lists:keystore(Xi, 3, HWoParams, Dim)).

h_store_formal_params([], H) ->
  H;
h_store_formal_params([{phi,Xi}=Phi | OtherPhis], H) ->
  %% New formal param shadows homonymous outer local dim - if any.
  h_store_formal_params(OtherPhis, lists:keystore(Xi, 3, H, Phi)).

h_delete_vars(Xis, H) ->
  %% New local var shadows homonymous outer local dim or formal param - if any.
  lists:filter(fun
                 ({phi,  Xi}) -> not lists:member(Xi, Xis);
                 ({dim,_,Xi}) -> not lists:member(Xi, Xis)
               end, H).


%%-------------------------------------------------------------------------------------
%% Internal - Deterministic generation of hidden dimensions using
%% position of expression in evaluation tree
%%-------------------------------------------------------------------------------------
-type n() :: non_neg_integer().
-type pos() :: [n()].

-type index() :: pos_integer().
-type hidden_dim() :: {pos(), index()}.

%%-------------------------------------------------------------------------------------
%% @doc Return position of the root expression in the evaluation tree.
%% @private
%%-------------------------------------------------------------------------------------
-spec root_expr_pos() -> RootP :: pos().
root_expr_pos() -> [].

%%-------------------------------------------------------------------------------------
%% @doc Return position of subexpression N while in position P.
%% @private
%%-------------------------------------------------------------------------------------
-spec subexpr_pos(N :: n(), P :: pos()) -> SubP :: pos().
subexpr_pos(N, P) -> [N | P].

%%-------------------------------------------------------------------------------------
%% @doc Return I-th hidden dimension in position P.
%% @private
%%-------------------------------------------------------------------------------------
-spec hidden_dim(I :: index(), P :: pos()) -> HD :: hidden_dim().
hidden_dim(I, P) -> {P, I}.


%%------------------------------------------------------------------------------
%% Insta-test
%%------------------------------------------------------------------------------
d1_tournament() ->
  %%------------------------------------------------------------------------------
  %% The following function should be equivalent to the following:
  %%
  %% fun tournament.d.lim X = Y
  %% where
  %%   var Y = 
  %%     if #.t <= 0 then 
  %%       X 
  %%     else
  %%       (Y @ [d <- #.d * 2 + 1] + Y @ [d <- #.d * 2]) @ [t <- #.t - 1]
  %%     fi
  %% end
  %%------------------------------------------------------------------------------
  {fn, "tournament", [{b_param, d}, {b_param, lim}, {n_param, "X"}],
   {where, "Y",
    [{var, "Y",
      {'if', tprimop:lte({'#', time}, 0),
       "X",
       {'@',
	tprimop:plus({'@', "Y", {t, [{d, tprimop:plus(tprimop:times({'#', d}, 2), 1)}]}},
		     {'@', "Y", {t, [{d, tprimop:times({'#', d}, 2)}]}}),
	{t, [{time, tprimop:minus({'#', time}, 1)}]}}}}]}}.


test() ->
  R = ttransform0:transform0(d1_tournament()),
  transform1(R).
