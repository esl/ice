-module(ice_dtree).

-export([new/0, delete/0]).
-export([lookup/1, insert/1]).
-export([test/0]).

-define(TABLE_NAME, ice_cache).

%%------------------------------------------------------------------------------
%% @doc Create a new ets table for the cache
%%------------------------------------------------------------------------------
new() ->
  Tab = ets:new(?TABLE_NAME, [named_table, public, set, 
			      {read_concurrency, true},
			      {write_concurrency, true},
			      {keypos, 1}]),
  ets:delete_all_objects(Tab),
  Tab.

%%------------------------------------------------------------------------------
%% @doc Delete the ets table
%%------------------------------------------------------------------------------
delete() ->
  ets:delete(?TABLE_NAME).

%%------------------------------------------------------------------------------
%% @doc Lookup an {Identifier, Context} pair in the cache
%%------------------------------------------------------------------------------
lookup({Xi,Key} = XiKey) ->
  case ets:lookup(?TABLE_NAME, XiKey) of
    [] ->
      lookup({Xi,[]}, Key);
    [{{_,_},{i,Dims,_}}] ->
      Dims;
    [{{_,_},Value}] ->
      Value
  end.

lookup({Xi,Key} = XiKey, K) ->
  case ets:lookup(?TABLE_NAME, XiKey) of
    [] ->
      [];
    [{{_,_},{calc,W}}] ->
      %%throw(error_calc);
      {calc, W};
    [{{_,Key},{i,Dims,_}}] ->
      case tset:restrict_domain(K, Dims) of
	[] ->
	  Dims;
	K1 ->
	  lookup({Xi,K1}, tset:subtract_by_domain(K, Dims))
      end;
    [{{_,_},Value}] ->
      Value
  end.

%%------------------------------------------------------------------------------
%% @doc Insert a value at {Xi, Key} into the cache
%%------------------------------------------------------------------------------
insert({Xi,Key,V}) ->
  ets:insert(?TABLE_NAME, {{Xi,Key},V}).

insert_correct_tree(Lbl) ->
  ets:insert(Lbl, {{"A",[]}, {i,[{dim,t}],[]}}),
  ets:insert(Lbl, {{"A",[{{dim,t},0}]}, {i,[{dim,s}],[]}}),
  ets:insert(Lbl, {{"A",[{{dim,t},0},{{dim,s},0}]}, 1}),
  ets:insert(Lbl, {{"A",[{{dim,t},1}]}, {i,[{dim,s}],[]}}),
  ets:insert(Lbl, {{"A",[{{dim,t},1},{{dim,s},0}]}, 2}),
  ets:insert(Lbl, {{"A",[{{dim,t},1},{{dim,s},1}]}, 3}),
  ets:insert(Lbl, {{"A",[{{dim,t},2}]}, {calc,[0]}}),
  ets:insert(Lbl, {{"B",[]}, {i,[{dim,s}],[]}}),
  ets:insert(Lbl, {{"B",[{{dim,s},0}]}, 1}),
  ets:insert(Lbl, {{"B",[{{dim,s},1}]}, 1}).

test() ->
  Lbl = cache_test,
  Tab = new(),
  insert_correct_tree(Tab),
  [{dim,t}] = lookup({"A",[]}, Tab),
  [{dim,s}] = lookup({"A",[{{dim,t},0}]}, Tab),
  1 = lookup({"A",[{{dim,t},0},{{dim,s},0}]}, Tab),
  [] = lookup({"B",[{{dim,s},2}]}, Tab),
  [{dim,s}] = lookup({"B",[{{dim,t},0}]},Tab),
  1 = lookup({"B",[{{dim,t},0},{{dim,s},0}]}, Tab),
  {calc,[0]} = lookup({"A",[{{dim,t},2}]}, Tab),
  2 = lookup({"A",[{{dim,t},1},{{dim,s},0}]}, Tab),
  3 = lookup({"A",[{{dim,t},1},{{dim,s},1}]}, Tab).
