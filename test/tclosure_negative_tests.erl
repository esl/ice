%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tclosure_negative_tests).

%% Negative tests for closure of abstractions over environment.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

abs_environment_closure_negative_test_() ->
  {foreachx,
   fun(Options) ->
       case proplists:get_bool(mock_tpar, Options) of
         true  -> mock_tpar();
         false -> nothing
       end,
       setup()
   end,
   fun(Options, Pid) ->
       cleanup(Pid),
       case proplists:get_bool(mock_tpar, Options) of
         true  -> unmock_tpar();
         false -> nothing
       end
   end,
   lists:append(
     [
      abs_cannot_access_variable_ids_in_application_env(),
      abs_cannot_access_variable_ids_in_creation_env()
     ])}.


abs_cannot_access_variable_ids_in_application_env() ->
  UndefVarId = "A",
  lists:map(
    fun(Test) ->
        F = fun(SOrT) ->
                ?_assertError(
                   {badmatch, {error, undefined_identifier, UndefVarId}},
                   eval(SOrT))
            end,
        ensure_option_in_test(F, Test)
    end,
    lists:append(
      [
       "((F.1) where var A = 1 end) where fun F.x =   A end",
       "((F!1) where var A = 1 end) where fun F!x =   A end",
       %% "((F 1) where var A = 1 end) where fun F x =   A end", TODO: Improve transformation 0
       "((↓ F) where var A = 1 end) where var F = ↑{} A end"
      ],
      %%
      lists:map(
        fun(X) ->
            F = fun(FAbs) ->
                    {where, s("(F.1) where var A = 1 end"),
                     [{var, s("F"), FAbs},
                      {dim, s("t"), s("0")}]}
                    %% "((F.1) where var A = 1 end) where var F = ...;; dim t <- 0 end"
                end,
            compose_tree_and_b_abs(F, X)
        end,
        b_abs_returned_by_various_expressions(UndefVarId)))).

abs_cannot_access_variable_ids_in_creation_env() ->
  UndefVarId = "A",
  lists:map(
    fun(Test) ->
        F = fun(SOrT) ->
                ?_assertError(
                   {badmatch, {error, undefined_identifier, UndefVarId}},
                   eval(SOrT))
            end,
        ensure_option_in_test(F, Test)
    end,
    lists:append(
      [
       "( (F where var A = 1 end).1) where fun F.x =   A end",
       "( (F where var A = 1 end)!1) where fun F!x =   A end",
       %% "( (F where var A = 1 end) 1) where fun F x =   A end", TODO: Improve transformation 0
       "(↓(F where var A = 1 end)  ) where var F = ↑{} A end"
      ],
      %%
      lists:map(
        fun(X) ->
            F = fun(FAbs) ->
                    {where, s("(F where var A = 1 end).1"),
                     [{var, s("F"), FAbs},
                      {dim, s("t"), s("0")}]}
                    %% "((F where var A = 1 end).1) where var F = ...;; dim t <- 0 end"
                end,
            compose_tree_and_b_abs(F, X)
        end,
        b_abs_returned_by_various_expressions(UndefVarId)))).

b_abs_returned_by_various_expressions(UndefVarIdA) ->
  BAbs = {b_abs, [], [s("x")], s(UndefVarIdA)}, %% \_ x -> A
  [
   {primop, fun(X) -> X end, [BAbs]},
   %%
   {'@', s("#.t"), {t, [{{dim,"t"}, BAbs}]}},
   %% "#.t @ [t <- \_x -> A]"
   %%
   {[mock_tpar],
    {'@', {b_abs, [], [s("x")], 1}, %% A harmless b_abs, just for returning one
     {t, [
          %% Define a tuple with a dummy dimension idetifier-ordinate
          %% couple. The dim id is an 'if' expression, the ordinate is
          %% a dumb unused '0'. The dim id is an 'if' expression in
          %% order to evaluate BAbs while still returning a valid dim
          %% id.
          { {'if', tprimop:eq(
                     {b_apply, BAbs, [0]}, %% Force evaluation of BAbs
                     {b_apply, BAbs, [0]}), %% Result should be true...
             %% XXX A dimension identifier cannot be written as a
             %% program with the current integration with the parser,
             %% as dimension identifiers and variable identifiers do
             %% not share the same domain ATM
             {dim,"t"}, %% 'then' part of if-then-else
             s("false")}, %% 'else' part of if-then-else - dead code
            0 %% A dumb ordinate
          }
         ]}}},
   %% "(\_ x -> 1) @ [(if ((\_x -> A).0 == (\_x -> A).0) then t else 0 fi) <- 0]"
   %%
   {'@', BAbs, s("[t <- 0]")},
   %% "(\_ x -> A) @ [t <- 0]"
   %%
   {'if', s("true"), BAbs, s("false")},
   {'if', s("false"), s("true"), BAbs},
   %% "if true then (\_ x -> A) else false fi"
   %% "if false then true else (\_ x -> A) fi"
   %%
   {b_apply, {b_abs, [], [s("a")], BAbs}, [s("0")]},
   {v_apply, {v_abs, [], [s("a")], BAbs}, [s("0")]},
   %% TODO: Improve transformation 0 %% {n_apply, {n_abs, [], [s("a")], BAbs}, [s("0")]}
   {i_apply, {i_abs, [],           BAbs}          },
   %% "(\_ a -> \_ x -> A).0"
   %% "(\  a -> \_ x -> A)!0"
   %% "(\\ a -> \_ x -> A) 0"
   %% "↓(↑{} \_ x -> A)"
   %%
   {b_apply, {b_abs, [], [s("a")], s("a")}, [BAbs]},
   {v_apply, {v_abs, [], [s("a")], s("a")}, [BAbs]},
   %% TODO: Improve transformation 0 %% {n_apply, {n_abs, [], [s("a")], s("a")}, [BAbs]}
   %%
   {where, s("G"), [{var,"G",BAbs}]},
   {where, BAbs, [{var,"KeepMe",s("0")}]},
   %% G where var G = \_ x -> A end
   %% \_ x -> A where var KeepMe = 0 end
   %%
   {where, s("#.g"), [{dim,"g",BAbs}]},
   {where, BAbs, [{dim,"keep_me",s("0")}]}
   %% #.g where dim g <- \_ x -> A end
   %% \_ x -> A where dim keep_me <- 0 end
  ].


%% Internals

compose_tree_and_b_abs(ASTGenFun, {Options, FAbs}) when is_list(Options) ->
  {Options, ASTGenFun(FAbs)};
compose_tree_and_b_abs(ASTGenFun, FAbs) ->
  compose_tree_and_b_abs(ASTGenFun, {[], FAbs}).

ensure_option_in_test(TestGenFun, {Options, SOrT}) when is_list(Options) ->
  F = fun(_,_) -> %% Fun needed for usage of foreachx
          TestGenFun(SOrT)
      end,
  {Options, F};
ensure_option_in_test(TestGenFun, SOrT) ->
  ensure_option_in_test(TestGenFun, {[], SOrT}).


mock_tpar() ->
  ok = meck:new(tpar, [passthrough]),
  ok = meck:expect(tpar, eval,
                   fun(Xs, I, E, K, D, W, T) ->
                       tpar:eval_seq(Xs, I, E, K, D, W, T)
                   end).

unmock_tpar() ->
  ok = meck:unload(tpar).


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
