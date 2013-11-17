-module(textensional).

-export([eval_ext/2]).

-type ast() :: term().
-type ground_value_ast() :: term().
-type dim_ast() :: local_dim_ast() | formal_param_ast().
-type local_dim_ast() :: {dim, ttransform1:hidden_dim(), id()}.
-type formal_param_ast() :: {phi, id()}.
-type id() :: nonempty_string().

-type context() :: [{dim_ast(), ground_value_ast()}].

-type granularity() :: pos_integer().
-type ext_type() :: nonempty_string(). %% Extensional type

%%------------------------------------------------------------------------------
%% @doc Evaluate extensional expression.
%%
%% @TODO: Accumulate demands until granularity, i.e. do not call this
%% function always with granularity 1. If total requests are 5 and
%% granularity 4, shall the program hang as there are not enough
%% requests for doing a second offlod to the extensional device?
%%
%% TODO: Consider introducing the possibility to reference in an
%% extensional expression an (other) extensional variable, maybe
%% flattening AST (before or after offloading?).
%%
%% TODO: Consider introducing the possibility to reference in an
%% extensional expression non-extensional expressions, rewriting the
%% AST before offloading.
%%------------------------------------------------------------------------------
-spec eval_ext({ext_expr, ast(), InOutSpec, granularity()}, [context()]) ->
                  [ground_value_ast()] when
    InOutSpec :: {DimTypesIn, TypeOut},
    DimTypesIn :: [{dim_ast(), OrdType :: ext_type()}],
    TypeOut :: ext_type().
eval_ext({ext_expr, E0, {DimTypesIn, TypeOut}, Gr}, Ks) when
    length(Ks) == Gr ->
  {Dims,_OrdTys} = lists:unzip(DimTypesIn),
  KDs = [tset:restrict_domain(K, Dims) || K <- Ks],
  eval_ext_cl(DimTypesIn, TypeOut, E0, KDs).


%%------------------------------------------------------------------------------
%% @doc Evaluate extensional expression as if it were a normal expression.
%%
%% This function is useful during development for the initial quick
%% test of more and more complex extensional expressions, but it is
%% unsafe as (1) it recognizes as valid some expressions that shall be
%% unsupported by an extensional evaluation function and (2) it could
%% insert in the cache weird dummy thread identifiers, causing the
%% rest of the program to hang.
%% @private
%%------------------------------------------------------------------------------
eval_ext_tcore(_DimTypesIn, _TypeOut, E0, Ks) ->
  lists:map(
    fun(K) ->
        {V,_} =
          tcore:eval(
            E0,
            [], %% Interpretation iota - Unused
            %% Environment - No variables allowed in extensional
            %% expression ATM
            [],
            K, _D=tset:domain(K),
            %% Thread id and clock - Default ones as evaluation of
            %% expression will not query cache (environment is empty)
            {[],self()}, 0),
        V
    end,
    Ks).

%%------------------------------------------------------------------------------
%% @doc Evaluate extensional expression in the simplest way.
%%
%% This function is supposed to be the unoptimized reference
%% implementation of evaluation of an extensional expression.
%% @private
%%------------------------------------------------------------------------------
eval_ext_seq_simple(_DimTypesIn, _TypeOut, E0, Ks) ->
  [eval_simple(E0, K) || K <- Ks].

eval_simple(Const, _K) when is_number(Const) orelse is_boolean(Const) ->
  Const;
eval_simple({primop, Primop, Eis}, K) ->
  Dis = [eval_simple(Ei, K) || Ei <- Eis],
  F = tprimop:f(Primop),
  apply(F, Dis);
eval_simple({'#', Dim}, K) ->
  lookup_ordinate(Dim, K).

lookup_ordinate(Dim, K) ->
  {Dim, Ord} = lists:keyfind(Dim, 1, K),
  Ord.

%%------------------------------------------------------------------------------
%% @doc Offload evaluation of extensional expression.
%% @private
%%------------------------------------------------------------------------------
eval_ext_cl(DimTypesIn, TypeOut, E0, Ks) ->
  ISpecs = [{uniq(Dim),Ty} || {Dim,Ty} <- DimTypesIn],
  Exp = sanitize(E0),
  Vectors = binarise(DimTypesIn, Ks),
  cl_map:on_the_fly(gpu, ice2cl, ISpecs, TypeOut, Exp, Vectors).


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------
uniq ({phi, Id}) ->
  "phi"++ Id;
uniq ({dim, {Pos,Ix}, Id}) ->
  Hidden = lists:flatmap(fun integer_to_list/1, [Ix|Pos]),
  "dim"++ Hidden ++ Id.

sanitize (Const) when is_number(Const); is_boolean(Const) ->
  Const;
sanitize ({primop, Op, Eis}) ->
  {primop, Op, [sanitize(Ei) || Ei <- Eis]};
sanitize ({'#', Dim}) ->
  {'#', sanitize(Dim)};
sanitize (Dim = {phi, _Id}) ->
  uniq(Dim);
sanitize (Dim = {dim, _H, _Id}) ->
  uniq(Dim).

-define(binarise(Kind),
  <<<<V:Kind>> || {Dim,V} <- lists:flatten(Ks), Dim == Id>>).
binarise (DimTypesIn, Ks) ->
  [begin
    Vector = case Ty of
      "uint" -> ?binarise(32/native-unsigned-integer);
      "int"  -> ?binarise(32/native         -integer);
      "float" -> ?binarise(32/native          -float)
    end,
    {uniq(Id), Vector}
   end || {Id,Ty} <- DimTypesIn].
