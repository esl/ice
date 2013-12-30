-module(ice_sets).

-export([intersection/2, difference/2]).
-export([restrict/2, union/2, restrict_domain/2]).
-export([subset/2, domain/1, is_d/1, is_k/1]).
-export([subtract/2, subtract_domain/2, subtract_by_domain/2]).
-export([perturb/2, union_d/1]).
-export([identical/2]).

%%-------------------------------------------------------------------------------------
%% @doc The intersection of sets A and B
%%-------------------------------------------------------------------------------------
intersection(A, B) ->
  sets:to_list(sets:intersection(sets:from_list(A), sets:from_list(B))).

%%------------------------------------------------------------------------------
%% @doc The difference between set A and set B
%%------------------------------------------------------------------------------
difference(A, B) ->
  sets:to_list(sets:subtract(sets:from_list(A), sets:from_list(B))).

%%-------------------------------------------------------------------------------------
%% @doc Restrict ( <| ) set A by set B
%%-------------------------------------------------------------------------------------
restrict(A, B) ->
  [X || X <- A, lists:member(X, B)].

%%-------------------------------------------------------------------------------------
%% @doc Restrict ( <| ) set A by domain B
%%-------------------------------------------------------------------------------------
restrict_domain(A, Domain) ->
  [{X, V} || {X, V} <- A, lists:member(X, Domain)].

%%-------------------------------------------------------------------------------------
%% @doc Subtract a set A from set B
%%-------------------------------------------------------------------------------------
subtract(A, B) ->
  [X || X <- A, lists:member(X, B) =:= false].

%%------------------------------------------------------------------------------
%% @doc Subtract the domain B from the domain of A
%%------------------------------------------------------------------------------
subtract_domain(A, B) ->
  [{X,V} || {X,V} <- A, lists:member(X, B) =:= false].

%%------------------------------------------------------------------------------
%% @doc Subtract the domain of B from the domain of A
%%------------------------------------------------------------------------------
subtract_by_domain(A, B) ->
  [{X,V} || {X,V} <- A, lists:keymember(X, 1, B) =:= false].

%%-------------------------------------------------------------------------------------
%% @doc Perturb (|) set A by set B
%%-------------------------------------------------------------------------------------
perturb(A, B) ->
  A1 = subtract_by_domain(A, B),
  union(A1, B).

%%-------------------------------------------------------------------------------------
%% @doc The union of sets A and B
%%-------------------------------------------------------------------------------------
union(A, B) ->
  sets:to_list(sets:union(sets:from_list(A), sets:from_list(B))).

%%-------------------------------------------------------------------------------------
%% @doc The union of sets in Dis where if Di is a set of known dimensions, Ui MaxI
%%-------------------------------------------------------------------------------------
union_d(Dis) ->
  case lists:any(fun ice_sets:is_k/1, Dis) of
    true ->
      {true, union_d(Dis, [])};
    false ->
      {false, Dis}
  end.

union_d([], UDis) ->
  UDis;
union_d([Di|Dis], UDis) ->
  case is_k(Di) of
    true ->
      union_d(Dis, union(Di, UDis));
    false ->
      union_d(Dis, UDis)
  end.

%%-------------------------------------------------------------------------------------
%% @doc Is A a subset of B?
%%-------------------------------------------------------------------------------------
subset(A, B) ->
  sets:is_subset(sets:from_list(A), sets:from_list(B)).

%%-------------------------------------------------------------------------------------
%% @doc Domain of set A
%%-------------------------------------------------------------------------------------
domain(A) ->
  [K || {K, _} <- A].

%%-------------------------------------------------------------------------------------
%% @doc Are two sets identical
%%-------------------------------------------------------------------------------------
identical([],[]) -> 
  true;
identical(A,B) ->
  (length(A) == length(B)) andalso
    (length([true || X <- A, lists:member(X, B)]) == length(A)).

%%-------------------------------------------------------------------------------------
%% @doc Check whether A is a set of known dimensions
%%-------------------------------------------------------------------------------------
is_d({dim, {_Pos,_Idx}, A}) when is_list(A) orelse is_atom(A) ->
  true;
is_d({phi, A}) when is_list(A) orelse is_atom(A) ->
  true;
is_d(_) ->
  false.

is_k(A) when is_list(A) ->
  lists:any(fun (X) -> is_d(X) end, A); %% XXX Why any and not all?
is_k(_) ->
  false.
