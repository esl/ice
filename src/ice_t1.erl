%%------------------------------------------------------------------------------
%% AST Transformation - Pass 1
%%------------------------------------------------------------------------------
-module(ice_t1).

-export([transform/1]).

%% The purpose of this transformation pass is to allocate a set of hidden 
%% dimensions which are accessible within the evaluation context 
%% deterministically.

%% The arguments to t1 are named Q, P, H, and X. Q is used to label wheredim 
%% clauses with a natural number so that re-entrancy into a wheredim is effective.
%% P is the current position in the evaluation tree. H is the set of hidden 
%% dimensions which are generated. X is the 

transform(E) ->
  t1(E, 0, [], [], []).

%% Every encountered bound identifier is transformed into a hidden dimension so
%% that it is referred to in the context rather than the environment.

t1({id, Xi}, _Q, _P, _H, X) ->
  case lists:keyfind({id, Xi}, 1, X) of

    {{id, Xi}, P1} ->
      {'?', hidden_dimension(P1)};
    false ->
      {id, Xi} %% Free Variable ?
  end;

%% Constant values never need to be transformed

t1({bool, _} = Bool, _Q, _P, _H, _X) ->
  Bool;

t1({char, _} = Char, _Q, _P, _H, _X) ->
  Char;

t1({int, _} = Int, _Q, _P, _H, _X) ->
  Int;

t1({float, _} = Float, _Q, _P, _H, _X) ->
  Float;

t1({string, _} = String, _Q, _P, _H, _X) ->
  String;

t1({seq, E0, E1}, Q, P, H, X) ->
  {seq, t1(E0, Q, [0|P], H, X), t1(E1, Q, [1|P], H, X)};

t1({primop, F, Args}, Q, P, H, X) ->
  {primop, F, map_position(Args, 0, (length(Args)-1), Q, P, H, X)};

t1({t, E0E1is}, Q, P, H, X) ->
  {t, map_tuple_position(E0E1is, 0, (length(E0E1is)-1), Q, P, H, X)};

t1({'@', E0, E1}, Q, P, H, X) ->
  {'@', t1(E0, Q, [0|P], H, X), t1(E1, Q, [1|P], H, X)};

t1({'if', E0, E1, E2}, Q, P, H, X) ->
  {'if', t1(E0, Q, [0|P], H, X), 
   t1(E1, Q, [1|P], H, X),
   t1(E2, Q, [2|P], H, X)};

t1({'#', E0}, Q, P, H, X) ->
  %% Changing the position in the evaluation tree is not needed as:
  %% * There is only one subexpression
  %% * No hidden dimensions are created in the current expression
  {'#', t1(E0, Q, P, H, X)};

t1({b_abs, Intensions, Args, E0}, Q, P, H, X) ->
  %% Limits
  IntensLim = length(Intensions),
  ArgStart = default_n(IntensLim),
  ArgLim = IntensLim + length(Args),
  %% Transform the intensions
  TIntensions = map_position(Intensions, 1, IntensLim, Q, P, H, X),
  %% Transform the arguments into dimensions which can be retrieved predictably
  {XArgs, TArgs} = lists:unzip(generate_dimensions(Args, ArgStart, ArgLim, P)),
  {b_abs, TIntensions, H, TArgs, 
    t1(E0, Q, [0|P], ice_sets:union(TArgs, H), ice_sets:union(XArgs, X))};

t1({b_apply, E0, Eis}, Q, P, H, X) ->
  TEis = map_position(Eis, 1, length(Eis), Q, P, H, X),
  {b_apply, t1(E0, Q, [0|P], H, X), TEis, P};

t1({v_abs, Intensions, Args, E0}, Q, P, H, X) ->
  %% Identical to the base abstraction transform
  IntensLim = length(Intensions),
  ArgStart = default_n(IntensLim),
  ArgLim = IntensLim + length(Args),
  TIntensions = map_position(Intensions, 1, IntensLim, Q, P, H, X),
  {XArgs, TArgs} = lists:unzip(generate_dimensions(Args, ArgStart, ArgLim, P)),
  {v_abs, TIntensions, H, TArgs,
    t1(E0, Q, [0|P], ice_sets:union(TArgs, H), ice_sets:union(XArgs, X))};

t1({v_apply, E0, Eis}, Q, P, H, X) ->
  TEis = map_position(Eis, 1, length(Eis), Q, P, H, X),
  {v_apply, t1(E0, Q, [0|P], H, X), TEis, P};

t1({i_abs, Intensions, E0}, Q, P, H, X) ->
  TIntensions = map_position(Intensions, 1, length(Intensions), Q, P, H, X),
  {i_abs, TIntensions, H, t1(E0, Q, [0|P], H, X)};

t1({i_apply, E0}, Q, P, H, X) ->
  {i_apply, t1(E0, Q, [0|P], H, X)};

t1({wherevar, E0, XiEis}, Q, P, H, X) ->
  TXiEis = map_wherevar_position(XiEis, 1, length(XiEis), Q, P, H, X),
  {wherevar, t1(E0, Q, [0|P], H, X), TXiEis};

t1({wheredim, E0, XiEis}, Q, P, H, X) ->
  {Xis, HDims, TXiEis} = map_wheredim_position(XiEis, 1, length(XiEis), Q, P, H, X),
  {wheredim, Q, {rho, Q},
   t1(E0, Q + 1, [0|P], ice_sets:union(HDims, H), ice_sets:union(Xis, X)),
   TXiEis}.

%%------------------------------------------------------------------------------
default_n(0) -> 1;
default_n(N) -> N.

%%------------------------------------------------------------------------------
%% @doc The most basic implementation of hidden dimensions simply uses the 
%% current position in the evaluation tree as the value of the dimension.
%%------------------------------------------------------------------------------

hidden_dimension(P) ->
  {dim, P}.

%%------------------------------------------------------------------------------
%% @doc We can generate a series of hidden dimensions given a start and end 
%% point. This is useful when we wish to generate many dimensions such as in
%% base abstractions. The first argument of a tuple returned here, labeled as
%% 'XArg' in the abstraction transformations is a reference from an identifier
%% Xi to a hidden dimension index. This is needed so that dimensional references
%% at a deeper scope can be substituted with the generated dimension.
%%------------------------------------------------------------------------------

generate_dimensions(Xis, Start, End, P) ->
  Is = lists:seq(Start, End),
  lists:map(
    fun ({Xi, I}) ->
        {{Xi, [I|P]}, hidden_dimension([I|P])}
    end, lists:zip(Xis, Is)
   ).

%%------------------------------------------------------------------------------
%% @doc Transforms a series of expressions.
%%------------------------------------------------------------------------------

map_position(Eis, Start, End, Q, P, H, X) ->
  Is = lists:seq(Start, End),
  lists:map(
    fun ({Ei, I}) ->
        t1(Ei, Q, [I|P], H, X)
    end,
    lists:zip(Eis, Is)
   ).

%%------------------------------------------------------------------------------
%% @doc Transforms a series of tuple expressions, for transforming contexts.
%%------------------------------------------------------------------------------

map_tuple_position(E0E1is, Start, End, Q, P, H, X) ->
  Is = lists:seq(Start, End),
  lists:map(
    fun ({{E0, E1}, I}) ->
        {t1(E0, Q, [I*2|P], H, X),
         t1(E1, Q, [I*2+1|P], H, X)}
    end, 
    lists:zip(E0E1is, Is)
   ).

%%------------------------------------------------------------------------------
%% @doc Transforms only the expression to the right of the id.
%%------------------------------------------------------------------------------

map_wherevar_position(XiEis, Start, End, Q, P, H, X) ->
  Is = lists:seq(Start, End),
  lists:map(
    fun ({{Xi, Ei}, I}) ->
        {Xi, t1(Ei, Q, [I|P], H, X)}
    end,
    lists:zip(XiEis, Is)
   ).

%%------------------------------------------------------------------------------
%% @doc Transforms the expression to the right of the id and generates a hidden 
%% dimension to replace the id.
%%------------------------------------------------------------------------------

map_wheredim_position(XiEis, Start, End, Q, P, H, X) ->
  Is = lists:seq(Start, End),
  TXiEis = 
    lists:map(
      fun ({{Xi, Ei}, I}) ->
          {{Xi, [I|P]}, {hidden_dimension([I|P]), t1(Ei, Q, [I|P], H, X)}}
      end, 
      lists:zip(XiEis, Is)
     ),
  {Xis, TXiEis1} = lists:unzip(TXiEis),
  {HDims, _} = lists:unzip(TXiEis1),
  {Xis, HDims, TXiEis1}.
