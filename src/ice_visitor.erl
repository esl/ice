%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice_visitor).

%% ice_visitor: a Tea visitor generator. Give it your accept/1 function.

-export([visit/2, visit/3]).

%% API

-spec visit (Ac, Ta) -> Tb
    when Ta::term(), Tb::term(), Ac::fun((Ta) -> Tb).

visit (Acceptor, Tree) ->
    visit(Acceptor, Tree, top_down).
visit (Acceptor, Tree, Way) ->
    V = fun (Node) ->
            case catch (Acceptor (Node)) of
                {'EXIT',{function_clause,_}} -> Node;
                {'EXIT',Error} -> error(Error); %TODO: imitate Erlang's way of throwing std errors.
                U -> U
            end
        end,
    W = case Way of
        bottom_up -> fun bottom_up/2
        ;top_down -> fun top_down/2
    end,
    %TODO: use only one fun/var instead of V and W.
    v (V,W, V(Tree)).


%% Internals

%% Those can be replaced in R17, thanks to EEP 37: “Funs with names”.
top_down  (F,T) ->   v(F,fun top_down/2, F(T)).
bottom_up (F,T) -> F(v(F,fun bottom_up/2,T)).

%% This can serve as a minimal documentation of the AST's nodes.

v (V,W, {declaration, L, T}) -> {declaration, L, W(V,T)};
v (V,W, {expr,        L, T}) -> {expr,        L, W(V,T)};

v (V,W, {module,L,I})           -> {module,L,W(V,I)};
v (V,W, {module,L,I,Es})        -> {module,L,W(V,I),W(V,Es)};
v (V,W, {export,L,I})           -> {export,L,W(V,I)};
v (V,W, {export_all,L,Is})      -> {export_all,L,W(V,Is)};
v (V,W, {import,L,I})           -> {import,L,W(V,I)};
v (V,W, {import_as,L,I1,I2})    -> {import_as,L,W(V,I1),W(V,I2)};
v (V,W, {import_only,L,I,Is})   -> {import_only,L,W(V,I),W(V,Is)};

v (V,W, {dim_decl, L, I}) ->       {dim_decl, L, W(V,I)};
v (V,W, {dim_decl, L, I, E}) ->    {dim_decl, L, W(V,I), W(V,E)};
v (V,W, {var_decl, L, I, E}) ->    {var_decl, L, W(V,I), W(V,E)};

v (V,W, {fun_decl, L, I, P, E}) ->       {fun_decl, L, W(V,I), W(V,P), W(V,E)};
v (V,W, {fun_decl, L, I, P, E, T}) ->    {fun_decl, L, W(V,I), W(V,P), W(V,E), W(V,T)};
v (V,W, {fun_decl, L, I, P, E, T, G}) -> {fun_decl, L, W(V,I), W(V,P), W(V,E), W(V,T), W(V,G)};
v (V,W, {tguard, L, T, E}) -> {tguard, L, W(V,T), W(V,E)};

v (V,W, {ext_decl,  L, I, A, R, E, G}) -> {ext_decl,  L, W(V,I), W(V,A), W(V,R), W(V,E), W(V,G)};
v (V,W, {ext_decl,  L, I, A, R, E}) ->    {ext_decl,  L, W(V,I), W(V,A), W(V,R), W(V,E)};
v (V,W, {ext_ty,    L, I, T}) ->          {ext_ty,    L, W(V,I), W(V,T)};
v (V,W, {cl_scalar, L, S}) ->             {cl_scalar, L, W(V,S)};

v (V,W, {where, L, E, D, F}) -> {where, L, W(V,E), W(V,D), W(V,F)};
v (V,W, {call,  L, F, P}) ->    {call,  L, W(V,F), W(V,P)};

v (V,W, {base_param,  L, E}) -> {base_param,  L, W(V,E)};
v (V,W, {value_param, L, E}) -> {value_param, L, W(V,E)};
v (V,W, {named_param, L, E}) -> {named_param, L, W(V,E)};

v (V,W, {tuple,         L, T}) ->    {tuple,         L, W(V,T)};
v (V,W, {tuple_element, L, G, D}) -> {tuple_element, L, W(V,G), W(V,D)};

v (V,W, {intension_creation,   L, I, E}) -> {intension_creation,   L, W(V,I), W(V,E)};
v (V,W, {intension_evaluation, L, I}) ->    {intension_evaluation, L, W(V,I)};

v (V,W, {'if',    L, C, E}) -> {'if',    L, W(V,C), W(V,E)};
v (V,W, {if_expr, L, C, E}) -> {if_expr, L, W(V,C), W(V,E)};

v (V,W, {lambda, L, F, P, E}) -> {lambda, L, W(V,F), W(V,P), W(V,E)};

v (V,W, {';',  L, G, D}) ->  {';',   L, W(V,G), W(V,D)};
v (V,W, {'or',  L, G, D}) -> {'or',  L, W(V,G), W(V,D)};
v (V,W, {'and', L, G, D}) -> {'and', L, W(V,G), W(V,D)};
v (V,W, {'@',   L, G, D}) -> {'@',   L, W(V,G), W(V,D)};
v (V,W, {'<',   L, G, D}) -> {'<',   L, W(V,G), W(V,D)};
v (V,W, {'<=',  L, G, D}) -> {'<=',  L, W(V,G), W(V,D)};
v (V,W, {'==',  L, G, D}) -> {'==',  L, W(V,G), W(V,D)};
v (V,W, {'>=',  L, G, D}) -> {'>=',  L, W(V,G), W(V,D)};
v (V,W, {'>',   L, G, D}) -> {'>',   L, W(V,G), W(V,D)};
v (V,W, {'!=',  L, G, D}) -> {'!=',  L, W(V,G), W(V,D)};
v (V,W, {'+',   L, G, D}) -> {'+',   L, W(V,G), W(V,D)};
v (V,W, {'-',   L, G, D}) -> {'-',   L, W(V,G), W(V,D)};
v (V,W, {'*',   L, G, D}) -> {'*',   L, W(V,G), W(V,D)};
v (V,W, {'/',   L, G, D}) -> {'/',   L, W(V,G), W(V,D)};
v (V,W, {'%',   L, G, D}) -> {'%',   L, W(V,G), W(V,D)};
v (V,W, {'..',  L, G, D}) -> {'..',  L, W(V,G), W(V,D)};
v (V,W, {'#.',  L, E}) ->    {'#.',  L, W(V,E)};
v (V,W, {'#!',  L, E}) ->    {'#!',  L, W(V,E)};
v (V,W, {'+',   L, E}) ->    {'+',   L, W(V,E)};
v (V,W, {'-',   L, E}) ->    {'-',   L, W(V,E)};
v (V,W, {'not', L, E}) ->    {'not', L, W(V,E)};

v (V,W, {bool,          L, S}) -> {bool,          L, W(V,S)};
v (V,W, {int,           L, S}) -> {int,           L, W(V,S)};
v (V,W, {float,         L, S}) -> {float,         L, W(V,S)};
v (V,W, {char,          L, S}) -> {char,          L, W(V,S)};
v (V,W, {raw_string,    L, S}) -> {raw_string,    L, W(V,S)};
v (V,W, {cooked_string, L, S}) -> {cooked_string, L, W(V,S)};
v (V,W, {id,            L, S}) -> {id,            L, W(V,S)};

v (V,W, [H|T]) -> [W(V,H) | W(V,T)];
v (_,_, []) -> [];
v (_,_, T) -> T.

%% End of Module.
