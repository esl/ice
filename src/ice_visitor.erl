%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice_visitor).

%% ice_visitor: visitor pattern applied to ttranform1-generated tree.

-export([bottom_up/2]).
-export([ top_down/2]).
-export([t/0]).

%% API

-spec bottom_up (Ac, Ta) -> Tb
    when Ac::fun ((Ta) -> Tb)
       , Ta::term(), Tb::term().
bottom_up (Acceptor, Tree) ->
    visit(Acceptor, Tree, fun v_bottom_up/2).

-spec top_down (Ac, Ta) -> Tb
    when Ac::fun ((Ta) -> Tb)
       , Ta::term(), Tb::term().
top_down (Acceptor, Tree) ->
    visit(Acceptor, Tree, fun v_top_down/2).

%% Internals

t () ->
    E = {'@', "Y", {t, [{d, tprimop:times({'#', d}, 2)}]}},
    ice_visitor:top_down(
        fun ({'@', Lhs, Rhs}) ->
            Lhs ++" @ "++ Rhs
          ; ({t, Pairs}) ->
            [{L,R}|_] = Pairs,
            "[ "++ L ++" <-> "++ R ++" ]"
          ; ({primop, F, Args}) ->
            F ++"("++ Args ++")"
          ; ({'#', Dim}) ->
            "#."++ Dim
          ; (Rest) when is_atom(Rest); is_function(Rest); is_number(Rest) ->
            [Thing] = io_lib:format("~p",[Rest]),
            Thing
        end, E).

visit (Acceptor, Tree, Way) ->
    V = fun (Node) ->
            case catch (Acceptor(Node)) of
                {'EXIT',{function_clause,_}} -> Node;
                {'EXIT',Error} -> error(Error); %TODO: imitate Erlang's way of throwing std errors.
                U -> U
            end
        end,
    %TODO: use only one fun/var instead of V and W.
    v (V, Way, V(Tree)).

%% Those can be replaced in R17, thanks to EEP 37: “Funs with names”.
v_bottom_up (F, T) -> F(v(F, fun v_bottom_up/2,   T)).
v_top_down  (F, T) ->   v(F, fun  v_top_down/2, F(T)).

%% This can serve as a minimal documentation of the AST's nodes.

v (V,W, {wherevar, E0, XiEis}) -> {wherevar, W(V,E0), W(V,XiEis)};
v (V,W, {wheredim, E0, XiEis}) -> {wheredim, W(V,E0), W(V,XiEis)};

v (V,W, {b_abs,   Frozen, Params, E}) -> {b_abs,   W(V,Frozen), W(V,Params), W(V,E)};
v (V,W, {v_abs,   Frozen, Params, E}) -> {v_abs,   W(V,Frozen), W(V,Params), W(V,E)};
v (V,W, {i_abs,   Frozen,         E}) -> {i_abs,   W(V,Frozen),                W(V,E)};
v (V,W, {b_apply, E0, Eis}) ->           {b_apply, W(V,E0), W(V,Eis)};
v (V,W, {v_apply, E0, Eis}) ->           {v_apply, W(V,E0), W(V,Eis)};
v (V,W, {i_apply, E}) ->                 {i_apply, W(V,E)};

v (V,W, {t, E0E1is}) -> {t, W(V,E0E1is)};

v (V,W, {'if', E0, E1, E2}) -> {'if', W(V,E0), W(V,E1), W(V,E2)};

v (V,W, {primop, Fun, Eis}) -> {primop, W(V,Fun), W(V,Eis)};

v (V,W, {'@', E0, E1}) -> {'@', W(V,E0), W(V,E1)};
v (V,W, {'#', E0}) ->     {'#', W(V,E0)};

%%v (V,W, Const) when is_number(Const); is_boolean(Const) ->             Const;
v (V,W, {string, Str}) ->                                 {string, W(V,Str)};

v (V,W, {dim,         Xi}) when is_list(Xi); is_atom(Xi) -> {dim,                W(V,Xi)};
v (V,W, {dim, Hidden, Xi}) when is_list(Xi); is_atom(Xi) -> {dim, W(V,Hidden), W(V,Xi)};
v (V,W, {phi,         Xi}) when is_list(Xi); is_atom(Xi) -> {dim,                W(V,Xi)};

%%v (V,W,       Xi ) when              is_atom(Xi) ->           Xi;
v (V,W, {'?', Xi}) when is_list(Xi); is_atom(Xi) -> {phi, W(V,Xi)};

v (V,W, [H|T]) -> [W(V,H) | W(V,T)];
v (_,_, []) -> [];
v (_,_, T) -> T.

%% End of Module.
