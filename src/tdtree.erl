-module(tdtree).

-export([new/1]).
-export([lookup/2, insert/2]).
-export([test/0]).

new(Lbl) ->
  Tab = ets:new(Lbl, []),
  ets:delete_all_objects(Tab),
  Tab.

lookup({Xi,Key} = XiKey, Tbl) ->
  case ets:lookup(Tbl, XiKey) of
    [] ->
      lookup({Xi,[]}, Key, Tbl);
    [{{_,_},{i,Dims,_}}] ->
      Dims;
    [{{_,_},Value}] ->
      Value
  end.

lookup({Xi,Key} = XiKey, K, Tbl) ->
  case ets:lookup(Tbl, XiKey) of
    [] ->
      [];
    [{{_,_},{calc,_}}] ->
      throw(error_calc);
    [{{_,Key},{i,Dims,_}}] ->
      case tset:restrict_domain(K, Dims) of
	[] ->
	  Dims;
	K1 ->
	  lookup({Xi,K1}, tset:subtract_by_domain(K, Dims), Tbl)
      end;
    [{{_,_},Value}] ->
      Value
  end.

insert({Xi,Key,V}, Tab) ->
  ets:insert(Tab, {{Xi,Key},V}).

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
  Tab = new(Lbl),
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
