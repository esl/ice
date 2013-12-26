-module(ice_cache).

%% ice_cache interface exports
-export([create/0, delete/0]).
-export([find/5, add/6]).

%%------------------------------------------------------------------------------
%% @doc Create a named ets table which represents the cache
%%------------------------------------------------------------------------------
create() ->
  ice_dtree:new().

%%------------------------------------------------------------------------------
%% @doc Delete the named ets table which represents the cache
%%------------------------------------------------------------------------------
delete() ->
  ice_dtree:delete().

%%------------------------------------------------------------------------------
%% @doc Find an identifier with a specific context K restricted by the domain D
%%------------------------------------------------------------------------------
find(X, K, D, {Id0, _} = W0, _T) ->
  KD = lists:keysort(1, ice_sets:restrict_domain(K, D)),
  case ice_dtree:lookup({X,KD}) of
    [] ->
%%      io:format(user, "Inserting X = ~p, KD = ~p, {calc, ~p}~n", [X, KD, W0]),
      true = ice_dtree:insert({X,KD,{calc,W0}}),
      %% FIXME Lookup must atomically write calc if needed, otherwise
      %% concurrent find()s for the same {X,KD} would independently
      %% start computing the same value and write (different) calc,
      %% violating the assumption of add().
      {{calc,W0}, 0};
    {calc, {Id1,_} = W1} = V ->
      case lists:prefix(Id1, Id0) of
	true ->
	  hang;
	false ->
	  {V, 0}
      end;
    V ->
      {V, 0}
  end.

%%------------------------------------------------------------------------------
%% @doc Add an {identifier, context, value} to the cache
%%------------------------------------------------------------------------------
add(X, K, D, W, T, V1) ->
  KD = lists:keysort(1, ice_sets:restrict_domain(K, D)),
  case ice_dtree:lookup({X,KD}) of
    [] ->
      %% 
      find(X, K, D, W, T),
      add(X, K, D, W, T, V1);
    {calc, W} ->
      case V1 of
	V1 when is_list(V1) ->
	  true = ice_dtree:insert({X,KD,{i,V1,[]}});
	V1 ->
	  true = ice_dtree:insert({X,KD,V1})
      end,
      {V1, 0};
    {calc, _} = Thr ->
%%      io:format(user, "X = ~p, KD = ~p is a different thread...~n", [X, KD]),
      {V1, 0};
    ExistingV ->
%%      io:format(user, "Received other = ~p~n", [Other]),
      {ExistingV, 0}
  end.
