-module(ice_cache).

%% ice_cache interface exports
-export([create/0, delete/0]).
-export([find/5, add/6]).

-define(FMT_D(V), %% Format missing dimensions
        case V of
          Dims when is_list(Dims) andalso length(Dims) > 0 ->
            "{i, ~p}";
          _ ->
            "~p"
        end).

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
  KD = ice_dtree:sort_context(ice_sets:restrict_domain(K, D)),
  case ice_dtree:insert_new({X,KD}, {calc,W0}) of
    {true, {calc,W0}} ->
      %% io:format("Inserted X = ~p, KD = ~p, {calc, ~p}~n", [X, KD, W0]),
      {{calc,W0}, 0};
    {false, {calc, {Id1,_} = W1} = V} ->
      {V, 0};

    %% FIXME: Possibly add this to a 'debugging' mode
    %% case lists:prefix(Id1, Id0) of
    %%   true ->
    %%     hang;
    %%   false ->
    %%     {V, 0}
    %% end;

    {false, V} ->
      %% io:format("Found X = ~p, KD = ~p, " ++ ?FMT_D(V) ++ "~n", [X,KD,V]),
      {V, 0}
  end.

%%------------------------------------------------------------------------------
%% @doc Add an {identifier, context, value} to the cache
%%------------------------------------------------------------------------------
add(X, K, D, W, _T, V) ->
  KD = ice_dtree:sort_context(ice_sets:restrict_domain(K, D)),
  case ice_dtree:lookup({X,KD}) of
    {calc, W} ->
      %% io:format("Inserting X = ~p, KD = ~p, " ++ ?FMT_D(V) ++ "~n", [X,KD,V]),
      true = ice_dtree:insert({X,KD}, V),
      {V, 0};
    _ ->
      hang
  end.
