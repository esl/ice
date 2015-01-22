%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tclosure_negative_tests).

%% Negative tests for closure of abstractions over environment.

-include_lib("eunit/include/eunit.hrl").

-define(_assertUndefVarId(VarId, SOrT),
        ?_assertError(
           {badmatch, {error, undefined_identifier, {id, VarId}}},
           eval(SOrT))).
-define(_assertUndefVarIdMockingIcePar(VarId, SOrT),
        {setup, fun mock_ice_par/0, fun(_) -> unmock_ice_par() end,
         ?_assertUndefVarId(VarId, SOrT)}).
-define(_assertUndefVarIdMockingIcePrimopEval(VarId, MockArgs, SOrT),
        {setup,
         fun() -> mock_ice_primop_eval(MockArgs) end,
         fun(_) -> unmock_ice_primop_eval() end,
         ?_assertUndefVarId(VarId, SOrT)}).


%% API tests.

abs_cannot_access_variable_ids_in_application_env_test_() ->
  UndefVarId = "A",
  ASTGenFun =
    fun(FAbs) ->
        {where, s("(F.1) where var A = 1 end"),
         [{var, s("F"), FAbs},
          {dim, s("t"), s("0")}]}
        %% "((F.1) where var A = 1 end) where var F = ...;; dim t <- 0 end"
    end,
  {foreach, fun setup/0, fun cleanup/1,
   lists:append(
     [
      ?_assertUndefVarId(
         UndefVarId, "((F.1) where var A = 1 end) where fun F.x =   A end"),
      ?_assertUndefVarId(
         UndefVarId, "((F!1) where var A = 1 end) where fun F!x =   A end"),
      ?_assertUndefVarId(
         UndefVarId, "((F 1) where var A = 1 end) where fun F x =   A end"),
      ?_assertUndefVarId(
         UndefVarId, "((↓ F) where var A = 1 end) where var F = ↑{} A end")
     ],
     tests_w_b_abs_from_various_expressions(UndefVarId, ASTGenFun))}.

abs_cannot_access_variable_ids_in_creation_env_test_() ->
  UndefVarId = "A",
  ASTGenFun =
    fun(FAbs) ->
        {where, s("(F where var A = 1 end).1"),
         [{var, s("F"), FAbs},
          {dim, s("t"), s("0")}]}
        %% "((F where var A = 1 end).1) where var F = ...;; dim t <- 0 end"
    end,
  {foreach, fun setup/0, fun cleanup/1,
   lists:append(
     [
      ?_assertUndefVarId(
         UndefVarId, "( (F where var A = 1 end).1) where fun F.x =   A end"),
      ?_assertUndefVarId(
         UndefVarId, "( (F where var A = 1 end)!1) where fun F!x =   A end"),
      ?_assertUndefVarId(
         UndefVarId, "( (F where var A = 1 end) 1) where fun F x =   A end"),
      ?_assertUndefVarId(
         UndefVarId, "(↓(F where var A = 1 end)  ) where var F = ↑{} A end")
      ],
     tests_w_b_abs_from_various_expressions(UndefVarId, ASTGenFun))}.


tests_w_b_abs_from_various_expressions(A=_UndefVarIdA, T=_ASTGenF) ->
  BAbs = {b_abs, [], [s("x")], s(A)}, %% \_ x -> A
  [
   ?_assertUndefVarIdMockingIcePrimopEval(
      A,
      {'primop_identity', fun(X) -> X end},
      T({primop, 'primop_identity', [BAbs]})),
   %%
   ?_assertUndefVarIdMockingIcePar(A, T(s("#.t @ [t <- \\.x -> A]"))),
   %%
   ?_assertUndefVarIdMockingIcePar(
      A,
      T(s("(\\.x -> 1) // A harmless b_abs, just for returning one
            @ [
               // Define a tuple with a dummy dimension
               // identifier-ordinate couple. The dim id is an 'if'
               // expression, the ordinate is a dumb unused '0'. The
               // dim id is an 'if' expression in order to evaluate
               // BAbs while still returning a valid dim id.
               (if
                 (   (\\.x -> A).0 // Force evaluation of BAbs
                  == (\\.x -> A).0 )
               then // Condition should be true
                 t
               else // Dead code
                 false
               fi)
                 <- 0 // A dumb ordinate
              ]"))),
   %%
   ?_assertUndefVarId(A, T(s("(\\.x -> A) @ [t <- 0]"))),
   %%
   ?_assertUndefVarId(A, T(s("if true then (\\.x -> A) else false fi"))),
   ?_assertUndefVarId(A, T(s("if false then true else (\\.x -> A) fi"))),
   %%
   ?_assertUndefVarId(A, T(s("(\\.a -> \\.x -> A).0"))),
   ?_assertUndefVarId(A, T(s("(\\!a -> \\.x -> A)!0"))),
   ?_assertUndefVarId(A, T(s("(\\ a -> \\.x -> A) 0"))),
   ?_assertUndefVarId(A, T(s("↓(↑{} \\.x -> A)"))),
   %%
   ?_assertUndefVarId(A, T(s("(\\.a -> a).(\\.x -> A)"))),
   ?_assertUndefVarId(A, T(s("(\\!a -> a)!(\\.x -> A)"))),
   ?_assertUndefVarId(A, T(s("(\\ a -> a) (\\.x -> A)"))),
   %%
   ?_assertUndefVarId(A, T(s("G where var G = \\.x -> A end"))),
   ?_assertUndefVarId(A, T(s("\\.x -> A where var KeepMe = 0 end"))),
   %%
   ?_assertUndefVarId(A, T(s("#.g where dim g <- \\.x -> A end"))),
   ?_assertUndefVarId(A, T(s("\\.x -> A where dim keep_me <- 0 end")))
  ].


%% Internals - Mocking

mock_ice_par() ->
  ok = meck:new(ice_par, [passthrough]),
  ok = meck:expect(ice_par, eval,
                   fun(Xs, I, E, K, D, W, T) ->
                       ice_par:eval_seq(Xs, I, E, K, D, W, T)
                   end).

unmock_ice_par() ->
  ok = meck:unload(ice_par).

mock_ice_primop_eval({MockedOp, F}) ->
  ok = meck:new(ice_primop_eval, [passthrough, non_strict]),
  ok = meck:expect(ice_primop_eval, MockedOp, F).

unmock_ice_primop_eval() ->
  ok = meck:unload(ice_primop_eval).

%% Internals

setup() ->
  ice:start().

cleanup(Pid) ->
  ice:stop().

s(S) ->
  ice_string:parse(S).

eval(S) when is_list(S) ->
  T = ice_string:parse(S),
  ice:eval(T);
eval(T) ->
  ice:eval(T).

%% End of Module.
