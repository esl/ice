%%------------------------------------------------------------------------------
%% AST Transformation - Pass 1
%%------------------------------------------------------------------------------
-module(ice_t1).

-export([transform/1]).

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

transform(E) ->
  transform(E, root_expr_pos(), []).


-spec transform(AstIn :: term(), P :: pos(), H) ->
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
transform({bool, _} = Bool, _P, _H) ->
  Bool;

transform({char, _} = Char, _P, _H) ->
  Char;

transform({int, _} = Int, _P, _H) ->
  Int;

transform({float, _} = Float, _P, _H) ->
  Float;

transform({string, _} = String, _P, _H) ->
  String;

%%------------------------------------------------------------------------------
%% Sequence
%%------------------------------------------------------------------------------
transform({seq, E0, E1}, P, H) ->
  {seq, 
   transform(E0, subexpr_pos(0, P), H),
   transform(E1, subexpr_pos(1, P), H)};

%%------------------------------------------------------------------------------
%% Primop
%%------------------------------------------------------------------------------
transform({primop, F, Eis}, P, H) ->
  Ns = lists:seq(1, length(Eis)), %% 1,2,...
  NewEis =
    lists:map(
      fun({Ei, N}) ->
          transform(Ei, subexpr_pos(N,P), H) %% Pos 1,2,...
      end,
      lists:zip(Eis, Ns)),
  {primop, F, NewEis};

%%------------------------------------------------------------------------------
%% Tuple Expression
%%------------------------------------------------------------------------------
transform({t, E0E1is}, P, H) ->
  Ns = lists:seq(1, length(E0E1is)), %% 1,2,...
  NewE0E1is =
    lists:map(
      fun({{E0,E1}, N}) ->
          {transform(E0, subexpr_pos(N*2  ,P), H), %% Pos 2,4,...
           transform(E1, subexpr_pos(N*2+1,P), H)} %% Pos 3,5,...
      end,
      lists:zip(E0E1is, Ns)),
  {t, NewE0E1is};

%%------------------------------------------------------------------------------
%% Context Perturbation
%%------------------------------------------------------------------------------
transform({'@', E0, E1}, P, H) ->
  {'@',
   transform(E0, subexpr_pos(0,P), H),
   transform(E1, subexpr_pos(1,P), H)};

%%------------------------------------------------------------------------------
%% Conditional
%%------------------------------------------------------------------------------
transform({'if', E0, E1, E2}, P, H) ->
  {'if',
   transform(E0, subexpr_pos(0,P), H),
   transform(E1, subexpr_pos(1,P), H),
   transform(E2, subexpr_pos(2,P), H)};

%%------------------------------------------------------------------------------
%% Dimensional Query
%%------------------------------------------------------------------------------
transform({'#', E0}, P, H) ->
  %% Changing the position in the evaluation tree is not needed as:
  %% * There is only one subexpression
  %% * No hidden dimensions are created in the current expression
  {'#', transform(E0, P, H)};

%%------------------------------------------------------------------------------
%% Base Abstraction
%%------------------------------------------------------------------------------
transform({b_abs, Is, Params, E}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  ParamsAsDims = lists:map(fun({id, Param}) -> {phi, Param} end, Params),
  {b_abs, transform_frozen_dims(Is, Ps, H), ParamsAsDims,
   transform(E, subexpr_pos(0,P), h_store_formal_params(ParamsAsDims, H))};

transform({b_apply, E0, Eis}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Eis))),
  {b_apply, transform(E0, subexpr_pos(0,P), H),
   transform_actual_params(Eis, Ps, H)};

%%------------------------------------------------------------------------------
%% Value Abstraction
%%------------------------------------------------------------------------------
transform({v_abs, Is, Params, E}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  ParamsAsDims = lists:map(fun({id, Param}) -> {phi, Param} end, Params),
  {v_abs, transform_frozen_dims(Is, Ps, H), ParamsAsDims,
   %% XXX Pos 0?
   transform(E, subexpr_pos(0,P), h_store_formal_params(ParamsAsDims, H))};

transform({v_apply, E0, Eis}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Eis))),
  {v_apply, transform(E0, subexpr_pos(0,P), H),
   %% XXX Shouldn't another context application be here somewhere?
   transform_actual_params(Eis, Ps, H)};

%%------------------------------------------------------------------------------
%% Intension Abstraction
%%------------------------------------------------------------------------------
transform({i_abs, Is, E}, P, H) ->
  Ps = lists:map(fun(N) -> subexpr_pos(N,P) end, %% Pos 1,2,...
                 lists:seq(1, length(Is))),
  {i_abs, transform_frozen_dims(Is, Ps, H),
   %% XXX Pos 0?
   transform(E, subexpr_pos(0,P), H)};

transform({i_apply, E}, P, H) ->
  %% XXX Is subexpression with position really needed? And BTW -
  %% shouldn't another context application be here somewhere?
  {i_apply, transform(E, subexpr_pos(0,P), H)};

%%------------------------------------------------------------------------------
%% Wherevar
%%------------------------------------------------------------------------------
transform({wherevar, E0, XiEis}, P, H) ->
  {Xis, _Eis} = lists:unzip(XiEis),
  HWoXis = h_delete_vars(Xis, H),
  Ns = lists:seq(1, length(XiEis)), %% 1,2,...
  NewXiEis =
    [{Xi, transform(Ei, subexpr_pos(N,P), HWoXis)} %% Pos 1,2,...
     || {{Xi, Ei}, N} <- lists:zip(XiEis, Ns)],
  %% XXX The way position is assigned here is completely different
  %% from literature.
  {wherevar, transform(E0, subexpr_pos(0,P), HWoXis), NewXiEis};

%%------------------------------------------------------------------------------
%% Wheredim
%%------------------------------------------------------------------------------
transform({wheredim, E0, XiEis}, P, H) ->
  Ns = lists:seq(1, length(XiEis)),
  DimsEis =
    [{ {dim, hidden_dim(N,P), IdString},
       transform(Ei, subexpr_pos(N,P), H) }
     || {{{id,IdString}, Ei}, N} <- lists:zip(XiEis, Ns)],
  {Dims, _Eis} = lists:unzip(DimsEis),
  {wheredim, transform(E0, subexpr_pos(0,P), h_store_dims(Dims, H)), DimsEis};

%%-------------------------------------------------------------------------------------
%% Identifiers
%%-------------------------------------------------------------------------------------
transform({id,IdString} = Xi, _P, H) ->
  case {lists:keyfind(IdString, 3, H), lists:keyfind(IdString, 2, H)} of
    {{dim,_,IdString}=Dim, false} ->
      %% Replace reference to local dimension of wheredim clause with
      %% previously allocated hidden dimension.
      Dim;
    {false,{phi,IdString}=Phi} ->
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
transform_frozen_dims(Is, Ps, H) ->
  ice_sets:union([D || D = {phi,_} <- H], 
		 [transform(I, P, H) || {I, P} <- lists:zip(Is, Ps)]).

transform_actual_params(Eis, Ps, H) ->
  lists:map(fun({Ei, P}) -> transform(Ei, P, H) end, lists:zip(Eis, Ps)).


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
  {_, IdStrings} = lists:unzip(Xis),
  %% New local var shadows homonymous outer local dim or formal param - if any.
  lists:filter(fun
                 ({phi,  IdString}) -> not lists:member(IdString, IdStrings);
                 ({dim,_,IdString}) -> not lists:member(IdString, IdStrings)
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
