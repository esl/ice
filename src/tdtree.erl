-module(tdtree).

-export([new/0, new/3]).
-export([lookup/2, insert/2]).
-export([test/0, test_2/0, test_3/0]).

%%------------------------------------------------------------------------------
%% @doc Create a new dtree node (empty dtree nodes are not allowed).
%%------------------------------------------------------------------------------
new() ->
  [].

new(Xi, K, V) ->
  [{node, {Xi, K, V}, []}].

%%------------------------------------------------------------------------------
%% @doc Lookup an {Xi,K} pair in the DTree
%%------------------------------------------------------------------------------
lookup(_, []) ->
  [];
lookup({Xi,K}, [{node,{Xi,K,{i,V,_}},SNs}|DTree]) ->
  %%----------------------------------------------------------------------------
  %% If during our search we reach a context which is exactly the same as the 
  %% one we are looking for, we know we have found the result.
  %%------------------------------------------------------------------------------
  V;
lookup({Xi,K}, [{node,{Xi,K,V},SNs}|DTree]) ->
  %%----------------------------------------------------------------------------
  %% If during our search we reach a context which is exactly the same as the 
  %% one we are looking for, we know we have found the result.
  %%------------------------------------------------------------------------------
  V;
lookup({Xi,K0}, [{node,{Xi,K1,{i,Dims,Ords}},SNs}|DTree]) ->
  %%----------------------------------------------------------------------------
  %% If the context of the node we are looking at is a subset of the context we
  %% are searching for, then we must be at the right branch, otherwise search
  %% the rest of the branches for this particular depth.
  %% By restricting the context we are searching for to the dimensions in the
  %% internal node, we can tell if there is an entry for this node in the DTree.
  %% In the case where the context K1 is a subset of K0, but there is no entry in
  %% the DTree for this particular dimension, we can be sure that the value has
  %% not been defined. This saves us having to search the rest of the tree.
  %%----------------------------------------------------------------------------
  case tset:intersection(K0, K1) of
    K1 ->
      case lists:any(fun (X) ->
			 tset:restrict_domain(K0, Dims) =:= X
		     end, Ords) of
	true ->
	  lookup({Xi,K0}, SNs);
	false ->
	  lookup({Xi,K0}, DTree)
      end;
    [] ->
      lookup({Xi,K0},DTree)
  end;
lookup({Xi,K0}, [{node, {Xi,K1,V}, []}|DTree]) ->
  %%----------------------------------------------------------------------------
  %% If we reach a value in the DTree which is not an internal node, and not a
  %% {calc, W} value, we have reached a leaf node, so search the rest of the
  %% DTree.
  %%----------------------------------------------------------------------------
  lookup({Xi,K0}, DTree);
lookup({Xi,K}, [{node, {Yi,_,_}, _}|DTree]) ->
  %%----------------------------------------------------------------------------
  %% When the Identifier is distinct, we are at the top level. Search the rest 
  %% of the DTree.
  %%----------------------------------------------------------------------------
  lookup({Xi,K},DTree).

%%------------------------------------------------------------------------------
%% @doc Insert / Update
%%------------------------------------------------------------------------------
insert({Xi,K,V}=XiKV, DTree) ->
  insert(XiKV, DTree, []).

insert({Xi,K,V}, [], NewTree) ->
  [{node,{Xi,K,V},[]}|NewTree];
insert({Xi,K,V}, [{node,{Xi,K,_},SNs}|DTree], NewTree) ->
  %%----------------------------------------------------------------------------
  %% The value exists in the tree, update
  %%----------------------------------------------------------------------------
  NewTree ++ [{node,{Xi,K,V},SNs}|DTree];
insert({Xi,K0,V}, [{node,{Xi,K1,{i,Dims,Ords}},SNs}=Node|DTree], NewTree) ->
  %%----------------------------------------------------------------------------
  %% If the context of the node we want to insert is a subset of the context
  %% we are searching for, and the ordinate for this dimension is not known, 
  %% we need to update the known ordinates of this internal node and continue 
  %% the insertion.
  %% If the ordinate is known, we continue the insertion to update the value.
  %%----------------------------------------------------------------------------
  case tset:intersection(K0, K1) of
    K1 ->
      case lists:any(fun (X) ->
			 tset:restrict_domain(K0, Dims) =:= X
		     end, Ords) of
	true ->
	  NewTree ++ [{node,{Xi,K1,{i,Dims,Ords}},
		       insert({Xi,K0,V},SNs)}|DTree];
	false ->
	  UOrds = [tset:restrict_domain(K0,Dims)|Ords],
	  NewTree ++ [{node,{Xi,K1,{i,Dims,UOrds}},
		       insert({Xi,K0,V},SNs)}|DTree]
      end;
    [] ->
      insert({Xi,K0,V}, DTree, [Node|NewTree])
  end;
insert({Xi,K,V0}, [{node, {Xi,K1,V1}, []}|DTree], NewTree) ->
  insert({Xi,K,V0}, DTree, [{node,{Xi,K1,V1},[]}|NewTree]);
insert({Xi,K,V}, [{node, {Yi,_,_}, _}=Node|DTree], NewTree) ->
  insert({Xi,K,V}, DTree, [Node|NewTree]).

%%------------------------------------------------------------------------------
%% @doc Remove
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Update chain
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Postorder
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
test_data_1() ->
  [{node, {"A", [], {i, [t], [[{t, 0}], [{t, 1}]]}},
    [{node, {"A", [{t, 1}], {i, [s], [[{s, 0}]]}},
      [{node, {"A", [{t, 1}, {s, 0}], {calc, [0]}},
	[]}]},
     {node, {"A", [{t, 0}], {i, [s], [[{s, 0}], [{s, 1}]]}},
      [{node, {"A", [{t, 0}, {s, 0}], 1}, []},
       {node, {"A", [{t, 0}, {s, 1}], {calc, [1]}}, []}]}]}].

test_data_2() ->
  [{node, {"B", [], {i, [s], [[{s, 0}]]}},
    [{node, {"B", [{s, 0}], 1}, []}]}].

test_data_3() ->
  [{node,{"B",[],{i,[{[1],space}],[[{{[1],space},0}]]}},
              [{node,{"B",[{{[1],space},0}],1},[]}]},
        {node,{"A",[],{i,[{[0],time}],[[{{[0],time},0}],[{{[0],time},1}]]}},
              [{node,{"A",[{{[0],time},0}],1},[]},
               {node,{"A",
                      [{{[0],time},1}],
                      {i,[{[1],space}],[[{{[1],space},0}]]}},
                     [{node,{"A",[{{[0],time},1},{{[1],space},0}],{calc,[0]}},
                            []}]}]}].

test_data_4() ->
  [{node,{"B",[],{i,[{[1],space}],[[{{[1],space},1}]]}},
              [{node,{"B",[{{[1],space},1}],1},[]}]},
   {node,{"A",[],
	  {i,[{[0],time}],
	   [[{{[0],time},0}],[{{[0],time},1}],[{{[0],time},2}]]}},
    [{node,{"A",[{{[0],time},0}],1},[]},
     {node,{"A",
	    [{{[0],time},2}],
	    {i,[{[1],space}],[[{{[1],space},0}]]}},
      [{node,{"A",[{{[0],time},2},{{[1],space},0}],{calc,[0]}},
	[]}]},
     {node,{"A",
	    [{{[0],time},1}],
	    {i,[{[1],space}],[[{{[1],space},0}],[{{[1],space},1}]]}},
      [{node,{"A",
	      [{{[0],time},1},{{[1],space},0}],
	      {calc,[1]}},
	[]},
       {node,{"A",
	      [{{[0],time},1},{{[1],space},1}],
	      {calc,[1]}},
	[]}]}]}].

test() ->
  lookup({"A",[]}, test_data_1()),
  [] = lookup({"B",[]}, test_data_1()),
  lookup({"A", [{t, 1}]}, test_data_1()),
  lookup({"A", [{t, 1}, {s, 0}]}, test_data_1()),
  lookup({"A", [{t, 0}, {s, 0}]}, test_data_1()),
  lookup({"A", [{t, 0}, {s, 1}]}, test_data_1()),
  
  lookup({"B", []}, test_data_2()),
  lookup({"B", [{s, 0}]}, test_data_2()),
  lookup({"B", [{t, 0}, {s, 0}]}, test_data_2()),

  DT1 = insert({"A",[],{i,[t],[]}},[]),
  DT2 = insert({"A",[{t,1}],{i,[s],[]}}, DT1),
  DT3 = insert({"A",[{t,1},{s,0}], {calc, [0]}}, DT2),
  DT4 = insert({"A",[{t,1},{s,0}], 2}, DT3),
  DT5 = insert({"A",[{t,0}], {i, [s], []}}, DT4),
  DT6 = insert({"A",[{t,0},{s,0}], 1}, DT5),
  DT7 = insert({"A",[{t,0},{s,1}], 1}, DT6),
  DT8 = insert({"B",[],{i,[s],[]}},DT7),
  DT9 = insert({"B",[{s,0}],0},DT8),
  lookup({"B", [{t, 0}, {s, 0}]},DT9),
  
  DTT1 = insert({"A", [], {calc,[0]}}, []),
  DTT2 = insert({"A", [], [{[0],t}]}, DTT1).

test_2() ->  
  lookup({"A", [{{[0],time},1},{{[1],space},0}]}, test_data_3()).

test_3() ->  
  lookup({"A", [{{[0],time},1},{{[1],space},1}]}, test_data_4()).
  
