%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tclosure_negative_tests).

%% Negative tests for closure of abstractions over environment.

-include_lib("eunit/include/eunit.hrl").

-define(_assertUndefVarId(VarId, SOrT),
        ?_assertError(
           {badmatch, {error, undefined_identifier, VarId}},
           eval(SOrT))).
-define(_assertUndefVarIdMockingTpar(VarId, SOrT),
        {setup, fun mock_tpar/0, fun(_) -> unmock_tpar() end,
         ?_assertUndefVarId(VarId, SOrT)}).
-define(_assertUndefVarIdMockingTprimop(VarId, TprimopMockArgs, SOrT),
        {setup,
         fun() -> mock_tprimop(TprimopMockArgs) end,
         fun(_) -> unmock_tprimop() end,
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
   ?_assertUndefVarIdMockingTprimop(A, {'primop_identity', fun(X) -> X end},
                                    T({primop, 'primop_identity', [BAbs]})),
   %%
   ?_assertUndefVarIdMockingTpar(A, T(s("#.t @ [t <- \\.x -> A]"))),
   %%
   ?_assertUndefVarIdMockingTpar(
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

mock_tpar() ->
  ok = meck:new(tpar, [passthrough]),
  ok = meck:expect(tpar, eval,
                   fun(Xs, I, E, K, D, W, T) ->
                       tpar:eval_seq(Xs, I, E, K, D, W, T)
                   end).

unmock_tpar() ->
  ok = meck:unload(tpar).

mock_tprimop({MockedOp, F}) ->
  ok = meck:new(tprimop, [passthrough]),
  ExpectationFun = fun
                     (Op) when Op =:= MockedOp ->
                       F;
                     (Op) ->
                       meck:passthrough([Op])
                   end,
  ok = meck:expect(tprimop, f, ExpectationFun).

unmock_tprimop() ->
  ok = meck:unload(tprimop).

%% Internals

setup() ->
  {ok, Pid} = tcache:start_link(100),
  Pid.

cleanup(Pid) ->
  tcache_stop(Pid).

tcache_stop(Pid) ->
  catch tcache:stop(),
  case is_process_alive(Pid) of
    false ->
      ok;
    true ->
      tcache_stop(Pid)
  end.

s(S) ->
  {ok, T} = tea:string(S),
  T.

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T);
eval(T) ->
  tea:eval(T).

%% End of Module.
