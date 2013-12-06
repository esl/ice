-module(tdtree).

-export([new/0]).
-export([lookup/2, insert/2]).
-export([test/0]).

-type x() :: term().
-type dim() :: term().
-type v() :: term().
-type k() :: [{dim(), v()}].

-type dims() :: [dim()].
-type known_ords() :: [k()].

-type node() :: {node,
                 {x(), dims(),
                  v() | {i, dims(), known_ords()}
                 },
                 dtree()}.
-type dtree() :: [node()].

%%------------------------------------------------------------------------------
%% @doc Create a new dtree node (empty dtree nodes are not allowed).
%%------------------------------------------------------------------------------
-spec new() -> dtree().
new() ->
  [].

%%------------------------------------------------------------------------------
%% @doc Lookup an {Xi,K} in the dtree
%%------------------------------------------------------------------------------
-spec lookup({x(), k()}, dtree()) -> v().
lookup(XiK, DTree) ->
  lookup(XiK, [], [], DTree).

-spec lookup({x(), k()}, XXX1 :: dims(), XXX2 :: dims(), Tree :: dtree()) ->
                dims() | v().

%%------------------------------------------------------------------------------
%% When the context, delta and tree are empty, no result has been found.
%%------------------------------------------------------------------------------
lookup({Xi, K}, [], Dims, []) ->
  Dims;

%%------------------------------------------------------------------------------
%% When the context, delta are empty but the root of Xi exists, we have found
%% our result.
%%------------------------------------------------------------------------------
lookup({Xi, []}, [], _Dims, [{node, {Xi, [], {i, V, _}}, SNs}|Tree]) ->
  V;
lookup({Xi, []}, [], _Dims, [{node, {Xi, [], V}, SNs}|Tree]) ->
  V;

%%------------------------------------------------------------------------------
%% When the context is empty, but the delta isn't, if it matches the delta in
%% the current node then we have found our result.
%%------------------------------------------------------------------------------
lookup({Xi, []}, Di, _Dims, [{node, {Xi, Di, {i, V, _}}, SNs}|Tree]) ->
  V;
lookup({Xi, []}, Di, _Dims, [{node, {Xi, Di, V}, SNs}|Tree]) ->
  V;

%%------------------------------------------------------------------------------
%% When the context is empty, the delta isn't, but we've run out of nodes to
%% search for, it means the branch is empty.
%%------------------------------------------------------------------------------
lookup({Xi, []}, Di, _Dims, []) ->
  [];

%%------------------------------------------------------------------------------
%% When the node is an internal node and the delta is the same as the nodes
%% context, we must search the rest of K0 within the subnodes of this node.
%% If there is no intersection between the context.
%%------------------------------------------------------------------------------
lookup({Xi, K0}, Di, _Dims, [{node, {Xi, Di, {i, Dims, Ords}}, SNs}|Tree]) ->
  NewDi = tset:restrict_domain(K0, Dims),
  NewK0 = tset:subtract(K0, NewDi),
  lookup({Xi, NewK0}, NewDi, Dims, SNs);

%%------------------------------------------------------------------------------
%% When the node is not an internal node and the delta is the same as the
%% nodes context but K0 is nonempty, it means the tag contained too much
%% information (hopefully), return V.
%%------------------------------------------------------------------------------
lookup({Xi, K}, Di, _Dims, [{node, {Xi, Di, V}, SNs}|Tree]) ->
  V;

lookup({Xi, K}, [], _Dims, []) ->
  [];

%%------------------------------------------------------------------------------
%% When the nodes delta is distinct from the nodes context, we must search for 
%% the rest of K0 within the siblings of this node.
%%------------------------------------------------------------------------------
lookup({Xi, K}, Di, Dims, [Node|Tree]) ->
  lookup({Xi, K}, Di, Dims, Tree).


%%------------------------------------------------------------------------------
%% @doc Insert / Update an {Xi, K, V} in the dtree
%%------------------------------------------------------------------------------
-spec insert({x(), k(), v()}, dtree()) -> dtree().
insert(XiKV, Tree) ->
  insert(XiKV, [], Tree, []).

-spec insert({x(), k(), v()}, XXX :: dims(), Tree :: dtree(),
             Acc :: dtree()) -> dtree().

%%------------------------------------------------------------------------------
%% When the context, delta and tree are empty, insert new
%%------------------------------------------------------------------------------
insert({Xi, [], V}, [], [], Acc) ->
  Acc ++ [{node, {Xi, [], V}, []}];

%%------------------------------------------------------------------------------
%% When the context, delta are empty but the root of Xi exists, update
%%------------------------------------------------------------------------------
insert({Xi, [], V}, [], [{node, {Xi, [], _}, SNs}|Tree], Acc) ->
  Acc ++ [{node, {Xi, [], V}, SNs}] ++ Tree;

%%------------------------------------------------------------------------------
%% When the context is empty, but the delta isn't, if it matches the delta in 
%% the current node, update
%%------------------------------------------------------------------------------
insert({Xi, [], V}, Di, [{node, {Xi, Di, _}, SNs}|Tree], Acc) ->
  Acc ++ [{node, {Xi, Di, V}, SNs}] ++ Tree;

insert({Xi, [], V}, Di, [], Acc) ->
  Acc ++ [{node, {Xi, Di, V}, []}];

%%------------------------------------------------------------------------------
%% When the node is an internal node and the delta is the same as the nodes
%% context, we must search for the rest of K0 within the subnodes of this node
%%------------------------------------------------------------------------------
insert({Xi, K0, V}, Di, [{node, {Xi, Di, {i, Dims, Ords}}, SNs}|Tree], Acc) ->
  NewDi = tset:restrict_domain(K0, Dims),
  NewK0 = tset:subtract(K0, NewDi),
  UOrds = tset:union([NewDi], Ords),
  Acc ++ [{node, {Xi, Di, {i, Dims, UOrds}},
	   insert({Xi, NewK0, V}, NewDi, SNs, [])}] ++ Tree;

insert({Xi, K0, V}, Di, [], Acc) ->
  throw(undefined_branch);

%%------------------------------------------------------------------------------
%% When the node is not an internal node and the delta is the same as the nodes
%% context but K0 is nonempty, we cannot insert the node in a subnode (error)
%%------------------------------------------------------------------------------
insert({Xi, K, V0}, Di, [{node, {Xi, Di, V1}, SNs}|Tree], Acc) ->
  throw(not_an_internal_node);

%%------------------------------------------------------------------------------
%% When the delta is empty, and the tree is empty, insert it
%%------------------------------------------------------------------------------
insert({Xi, K0, V}, [], [], Acc) ->
  Acc ++ [{node, {Xi, K0, V}, []}];

%%------------------------------------------------------------------------------
%% When the nodes delta is distinct from the nodes context, we must search for 
%% the rest of K0 within the siblings of this node.
%%------------------------------------------------------------------------------
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
  [{node, {"A", [], {i,[{dim,t}],[[{{dim,t},0}], [{{dim,t},1}], [{{dim,t},2}]]}},
    [{node, {"A", [{{dim,t},0}], {i,[s],[[{s,0}]]}},
      [{node, {"A", [{s,0}], 1}, []}]},
     {node, {"A", [{{dim,t},1}], {i,[s],[[{s,0}],[{s,1}]]}},
      [{node, {"A", [{s,0}], 2}, []},
       {node, {"A", [{s,1}], 3}, []}]},
     {node, {"A", [{{dim,t},2}], {calc, [0]}}, []}]},
   {node, {"B", [], {i,[s],[[{s,0}],[{s,1}]]}},
    [{node, {"B", [{s,0}], 1}, []},
     {node, {"B", [{s,1}], 1}, []}]}].

test_tree_1()->
  [{node, {"A", [], {i, [t], [[{t,1}]]}},
    [{node, {"A", [{t,1}], {i, [s], []}},
      []}]}].

test() ->
  T1 = correct_tree(),
  [{dim,t}] = lookup({"A",[]}, T1),
  [s] = lookup({"A",[{{dim,t},0}]}, T1),
  1 = lookup({"A",[{{dim,t},0},{s,0}]}, T1),
  [s] = lookup({"B",[{{dim,t},0}]}, T1),
  [] = lookup({"B",[{s,2}]}, T1),
  1 = lookup({"B",[{{dim,t},0},{s,0}]}, T1),
  {calc,[0]} = lookup({"A",[{{dim,t},2}]}, T1),
  2 = lookup({"A",[{{dim,t},1},{s,0}]}, T1),
  3 = lookup({"A",[{{dim,t},1},{s,1}]}, T1),

  X = lookup({"A", [{s, 1}, {{dim,t}, 0}]}, T1),
  io:format("A [s <- 0, t <- 0] = ~p~n", [X]),

  T2 = test_tree_1(),
  [] = lookup({"A", [{t,1},{s,0}]}, T2),

  DT1 = insert({"A",[],{calc,[0]}},[]),
  DT1 = [{node,{"A",[],{calc,[0]}},[]}],

  DT2 = insert({"A",[],{i,[{dim,t}],[]}},DT1),
  DT2 = [{node,{"A",[],{i,[{dim,t}],[]}},[]}],

  DT3 = insert({"A",[{{dim,t},1}],{calc,[1]}},DT2),
  DT3 = [{node,{"A",[],{i,[{dim,t}],[[{{dim,t},1}]]}},
	  [{node,{"A",[{{dim,t},1}],{calc,[1]}},[]}]}],

  DT4 = insert({"A",[{{dim,t},1}],{i,[s],[]}},DT3),
  DT4 = [{node,{"A",[],{i,[{dim,t}],[[{{dim,t},1}]]}},
	  [{node,{"A",[{{dim,t},1}],{i,[s],[]}},[]}]}],

  DT5 = insert({"A",[{{dim,t},1},{s,0}],1},DT4),
  DT5 = [{node,{"A",[],{i,[{dim,t}],[[{{dim,t},1}]]}},
 	  [{node,{"A",[{{dim,t},1}],{i,[s],[[{s,0}]]}},
 	    [{node,{"A",[{s,0}],1},[]}]}]}],

  DT6 = insert({"A", [{{dim,t},0}], {i, [s], []}}, DT5),
  
  DT7 = insert({"A", [{{dim,t},0},{s,0}], {calc,[2]}},DT6).
  

