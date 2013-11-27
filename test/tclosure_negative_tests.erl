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
   ?_assertUndefVarIdMockingTpar(A, T({'@', s("#.t"), {t, [{"t", BAbs}]}})),
   %% "#.t @ [t <- \_x -> A]"
   %%
   ?_assertUndefVarIdMockingTpar(
      A,
      T({'@', {b_abs, [], [s("x")], 1}, %% A harmless b_abs, just for returning one
         {t, [
              %% Define a tuple with a dummy dimension
              %% identifier-ordinate couple. The dim id is an 'if'
              %% expression, the ordinate is a dumb unused '0'. The
              %% dim id is an 'if' expression in order to evaluate
              %% BAbs while still returning a valid dim id.
              { {'if', tprimop:eq(
                         {b_apply, BAbs, [0]}, %% Force evaluation of BAbs
                         {b_apply, BAbs, [0]}), %% Result should be true...
                 "t", %% 'then' part of if-then-else
                 s("false")}, %% 'else' part of if-then-else - dead code
                0 %% A dumb ordinate
              }
             ]}})),
   %% "(\_ x -> 1) @ [(if ((\_x -> A).0 == (\_x -> A).0) then t else 0 fi) <- 0]"
   %%
   ?_assertUndefVarId(A, T({'@', BAbs, s("[t <- 0]")})),
   %% "(\_ x -> A) @ [t <- 0]"
   %%
   ?_assertUndefVarId(A, T({'if', s("true"), BAbs, s("false")})),
   ?_assertUndefVarId(A, T({'if', s("false"), s("true"), BAbs})),
   %% "if true then (\_ x -> A) else false fi"
   %% "if false then true else (\_ x -> A) fi"
   %%
   ?_assertUndefVarId(A, T({b_apply, {b_abs, [], [s("a")], BAbs}, [s("0")]})),
   ?_assertUndefVarId(A, T({v_apply, {v_abs, [], [s("a")], BAbs}, [s("0")]})),
   %% TODO: Improve transformation 0 %% ?_assertUndefVarId(A, T({n_apply, {n_abs, [], [s("a")], BAbs}, [s("0")]})),
   ?_assertUndefVarId(A, T({i_apply, {i_abs, [],           BAbs}          })),
   %% "(\_ a -> \_ x -> A).0"
   %% "(\  a -> \_ x -> A)!0"
   %% "(\\ a -> \_ x -> A) 0"
   %% "↓(↑{} \_ x -> A)"
   %%
   ?_assertUndefVarId(A, T({b_apply, {b_abs, [], [s("a")], s("a")}, [BAbs]})),
   ?_assertUndefVarId(A, T({v_apply, {v_abs, [], [s("a")], s("a")}, [BAbs]})),
   %% TODO: Improve transformation 0 %% {n_apply, {n_abs, [], [s("a")], s("a")}, [BAbs]}
   %%
   ?_assertUndefVarId(A, T({where, s("G"), [{var,"G",BAbs}]})),
   ?_assertUndefVarId(A, T({where, BAbs, [{var,"KeepMe",s("0")}]})),
   %% G where var G = \_ x -> A end
   %% \_ x -> A where var KeepMe = 0 end
   %%
   ?_assertUndefVarId(A, T({where, s("#.g"), [{dim,"g",BAbs}]})),
   ?_assertUndefVarId(A, T({where, BAbs, [{dim,"keep_me",s("0")}]}))
   %% #.g where dim g <- \_ x -> A end
   %% \_ x -> A where dim keep_me <- 0 end
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
