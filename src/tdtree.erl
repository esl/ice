-module(tdtree).

-export([new/0]).
-export([lookup/2, insert/2]).
-export([test/0]).

%%------------------------------------------------------------------------------
%% @doc Create a new dtree node (empty dtree nodes are not allowed).
%%------------------------------------------------------------------------------
new() ->
  [].

%%------------------------------------------------------------------------------
%% @doc Lookup an {Xi,K} pair in the decision tree
%%------------------------------------------------------------------------------
lookup(XiK, DTree) ->
  lookup(XiK, [], DTree).

%%------------------------------------------------------------------------------
lookup({_Xi,K0}, Ki, []) ->
  [];

%%------------------------------------------------------------------------------
%% If during our search we reach a context which is exactly the same as the 
%% one we are looking for, we know we have found the result.
%%------------------------------------------------------------------------------
lookup({Xi,[]}, Ki, [{node,{Xi,Ki,{i,V,_}}=N,SNs}|DTree]) ->
%%  io:format("[0] XiK = ~p == N = ~p, Ki = ~p ~nSNs = ~p~n",[{Xi,[]}, N, Ki, SNs]),
  V;
lookup({Xi,[]}, Ki, [{node,{Xi,Ki,V},SNs}|DTree]) ->
  V;

%%------------------------------------------------------------------------------
%% If during our search we reach a leaf node (a node with no children), we give
%% back the last value reached. This ensures that redundant dimensions in K are
%% disregarded.
%%------------------------------------------------------------------------------
%%lookup({Xi,K0}, [], [{node,{Xi,[],{i,Dims,Ords}},[]}|DTree]) ->
%%  case tset:intersection(tset:domain(K0), Dims) of
%%    [] ->
%%      Dims;
%%    Dims ->
%%      []
%%  end;
%%lookup({Xi,K0}, Ki, [{node,{Xi,Ki,{i,Dims,Ords}},[]}|DTree]) ->
%%  case tset:intersection(tset:domain(K0), Dims) of
%%    [] ->
%%      Dims;
%%    Dims ->
%%n      []
%%  end;
lookup({Xi,K}, [], [{node,{Xi,[],V},[]}|DTree]) ->
  [];
lookup({Xi,K}, Ki, [{node,{Xi,Ki,{i,V,_}},[]}|DTree]) ->
  [];
lookup({Xi,K}, Ki, [{node,{Xi,Ki,V},[]}|DTree]) ->
  V;

%%------------------------------------------------------------------------------
%% Xi = Identifier, K0 = Context being looked up, Ki0 = Context of the previous
%% node in the tree.
%% First we take the domain of K0 and compare it to the dims in the internal 
%% node. If there are dimensions in common between the two sets, we continue the
%% search. If no dimensions are common then we cannot continue our traversal, we
%% must return the dimensions that are not known.
%% In the case where there are common dimensions, we then proceed to check 
%% whether the dimensions which are part of K0 are also ordinates in the 
%% internal node. 
%%------------------------------------------------------------------------------
lookup({Xi,K0}, Ki0, [{node,{Xi,K1,{i,Dims,Ords}},SNs}|DTree]) ->
  case tset:intersection(tset:domain(K0), Dims) of
    Dims ->
      case lists:member(tset:restrict_domain(K0, Dims), Ords) of
	true ->
	  Ki1 = tset:restrict_domain(K0, Dims),
	  Diff = tset:subtract(K0,Ki1),
%%	  io:format("New XiK = ~p, New Ki = ~p~n", [{Xi,Diff},Ki1]),
	  lookup({Xi, Diff}, Ki1, SNs);
	false ->
%%	  io:format("False ~n",[]),
	  []
      end;
    [] ->
%%      io:format("K0 = ~p, Dims = ~p~n", [K0, Dims]),
      Dims
  end;
lookup({Xi,K0}, Ki, [{node, {Xi,K1,V}, []}|DTree]) ->
  %%----------------------------------------------------------------------------
  %% If we reach a value in the DTree which is not an internal node, and not a
  %% {calc, W} value, we have reached a leaf node, so search the rest of the
  %% DTree.
  %%----------------------------------------------------------------------------
  lookup({Xi,K0}, Ki, DTree);
lookup({Xi,K0}, Ki, [{node, {Yi,_,_}, _}|DTree]) ->
  %%----------------------------------------------------------------------------
  %% When the Identifier is distinct, we are at the top level. Search the rest 
  %% of the DTree.
  %%----------------------------------------------------------------------------
  lookup({Xi,K0}, Ki, DTree).

%%------------------------------------------------------------------------------
%% @doc Insert / Update
%%------------------------------------------------------------------------------
insert(XiKV, Tree) ->
  insert(XiKV, [], Tree, []).

%% When the context, delta and tree are empty, insert new
insert({Xi, [], V}, [], [], Acc) ->
  Acc ++ [{node, {Xi, [], V}, []}];

%% When the context, delta are empty but the root of Xi exists, update
insert({Xi, [], V}, [], [{node, {Xi, [], _}, SNs}|Tree], Acc) ->
  Acc ++ [{node, {Xi, [], V}, SNs}] ++ Tree;

%% When the context is empty, but the delta isn't, if it matches the delta in 
%% the current node, update
insert({Xi, [], V}, Di, [{node, {Xi, Di, _}, SNs}|Tree], Acc) ->
  Acc ++ [{node, {Xi, Di, V}, SNs}] ++ Tree;

insert({Xi, [], V}, Di, [], Acc) ->
  Acc ++ [{node, {Xi, Di, V}, []}];

%% When the node is an internal node and the delta is the same as the nodes
%% context, we must search for the rest of K0 within the subnodes of this node.
insert({Xi, K0, V}, Di, [{node, {Xi, Di, {i, Dims, Ords}}, SNs}|Tree], Acc) ->
%%  io:format("Xi = ~p, K = ~p, V = ~p, Di = ~p~n", [Xi, K0, V, Di]),
  NewDi = tset:restrict_domain(K0, Dims),
  NewK0 = tset:subtract(K0, NewDi),
  UOrds = tset:union([NewDi], Ords),
  Acc ++ [{node, {Xi, Di, {i, Dims, UOrds}},
	   insert({Xi, NewK0, V}, NewDi, SNs, [])}] ++ Tree;

insert({Xi, K0, V}, Di, [], Acc) ->
%%  Acc ++ [{node, {Xi, K0, V}, []}];
  throw(undefined_branch);

%% When the node is not an internal node and the delta is the same as the nodes
%% context but K0 is nonempty, we cannot insert the node in a subnode (error)
insert({Xi, K, V0}, Di, [{node, {Xi, Di, V1}, SNs}|Tree], Acc) ->
  NewK0 = tset:subtract(K, Di),
%%  io:format("NewK0 = ~p, Di = ~p~n", [NewK0, Di]),
  Acc ++ [{node, {Xi, Di, V1},
	   insert({Xi, NewK0, V0}, Di, SNs, [])}] ++ Tree;

%%  throw(not_an_internal_node);

%% When the delta is empty, and the tree is empty, insert it
insert({Xi, K0, V}, [], [], Acc) ->
  Acc ++ [{node, {Xi, K0, V}, []}];

%% When the nodes delta is distinct from the nodes context, we must search for 
%% the rest of K0 within the siblings of this node.
insert({Xi, K0, V}, Di0, [Node|Tree], Acc) ->
  [Node|Acc] ++ insert({Xi, K0, V}, Di0, Tree, []).


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
correct_tree() ->
  [{node, {"A", [], {i,[{[0],t}],[[{{[0],t},0}],[{{[0],t},1}]]}},
    [{node, {"A", [{{[0],t},0}], {i,[s],[[{s,0}],[{s,1}]]}},
      [{node, {"A", [{s,0}], 1}, []}]},
     {node, {"A", [{{[0],t},1}], {i,[s],[[{s,0}]]}},
      [{node, {"A", [{s,0}], 2}}]}]},
   {node, {"B", [], {i,[s],[[{s,0}],[{s,1}]]}},
    [{node, {"B", [{s,0}], 1}, []},
     {node, {"B", [{s,1}], 1}, []}]}].

test_tree_1()->
  [{node, {"A", [], {i, [t], [[{t,1}]]}},
    [{node, {"A", [{t,1}], {i, [s], []}},
      []}]}].

test() ->
  T1 = correct_tree(),
  [{[0],t}] = lookup({"A",[]}, T1),
  [s] = lookup({"A",[{{[0],t},0}]}, T1),
  1 = lookup({"A",[{{[0],t},0},{s,0}]}, T1),
  [s] = lookup({"B",[{{[0],t},0}]}, T1),
  [] = lookup({"B",[{s,2}]}, T1),
  1 = lookup({"B",[{{[0],t},0},{s,0}]}, T1),

  X = lookup({"A", [{s, 1}, {{[0],t}, 0}]}, T1),
  io:format("A [s <- 0, t <- 0] = ~p~n", [X]),

  T2 = test_tree_1(),
  [] = lookup({"A", [{t,1},{s,0}]}, T2),

  DT1 = insert({"A",[],{calc,[0]}},[]),
  DT1 = [{node,{"A",[],{calc,[0]}},[]}],

  DT2 = insert({"A",[],{i,[{[0],t}],[]}},DT1),
  DT2 = [{node,{"A",[],{i,[{[0],t}],[]}},[]}],

  DT3 = insert({"A",[{{[0],t},1}],{calc,[1]}},DT2),
  DT3 = [{node,{"A",[],{i,[{[0],t}],[[{{[0],t},1}]]}},
	  [{node,{"A",[{{[0],t},1}],{calc,[1]}},[]}]}],

  DT4 = insert({"A",[{{[0],t},1}],{i,[s],[]}},DT3),
  DT4 = [{node,{"A",[],{i,[{[0],t}],[[{{[0],t},1}]]}},
	  [{node,{"A",[{{[0],t},1}],{i,[s],[]}},[]}]}],

  DT5 = insert({"A",[{{[0],t},1},{s,0}],1},DT4),
  DT5 = [{node,{"A",[],{i,[{[0],t}],[[{{[0],t},1}]]}},
 	  [{node,{"A",[{{[0],t},1}],{i,[s],[[{s,0}]]}},
 	    [{node,{"A",[{s,0}],1},[]}]}]}],

  DT6 = insert({"A", [{{[0],t},0}], {i, [s], []}}, DT5),
  
  DT7 = insert({"A", [{{[0],t},0},{s,0}], {calc,[2]}},DT6).
  

