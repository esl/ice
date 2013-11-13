%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(where_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_testMockingTpar(Expr),
        {setup, fun mock_tpar/0, fun(_) -> unmock_tpar() end,
         ?_test(Expr)}).


%% API tests.

where_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(where_with_only_dims_is_represented_as_wheredim_only()),
    ?_test(where_with_only_vars_is_represented_as_wherevar_only()),
    %%
    ?_test(var_can_use_dim_in_same_where()),
    ?_test(var_can_use_dims_in_same_where()),
    ?_testMockingTpar(dim_cannot_use_var_in_same_where())
   ]}.

wheredim_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(dim_cannot_be_redefined_in_same_wheredim())
   ]}.

wherevar_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(var_can_refer_to_var_defined_before_in_same_wherevar()),
    ?_test(var_can_refer_to_var_defined_after_in_same_wherevar()),
    %%
    ?_test(var_cannot_be_redefined_in_same_wherevar()),
    %%
    ?_test(var_redefined_in_nested_wherevar_hangs_cache()),
    ?_test(var_redefined_in_nested_wherevar_hangs_cache2()),
    ?_test(var_redefined_in_nested_wherevar_hangs_cache3()),
    ?_test(var_redefined_in_nested_wherevar_is_shadowed_by_outer_if_outer_already_queried())
   ]}.


where_with_only_dims_is_represented_as_wheredim_only() ->
  S = "#.t where dim t <- 46 end",
  ?assertMatch({wheredim, _, [{_, 46}]}, t0(s(S))),
  ?assertMatch({46,_}, eval(S)).

where_with_only_vars_is_represented_as_wherevar_only() ->
  S = "A where var A = 46 end",
  ?assertMatch({wherevar, _, [{_, 46}]}, t0(s(S))),
  ?assertMatch({46,_}, eval(S)).

var_can_use_dim_in_same_where() ->
  S = "X where dim t <- 0;; var X = #.t end",
  ?assertMatch({0,_}, eval(S)).

var_can_use_dims_in_same_where() ->
  S = "X where var X = #.t + #.s;; dim t <- 2;; dim s <- 3 end",
  ?assertMatch({5,_}, eval(S)).

dim_cannot_use_var_in_same_where() ->
  S = "#.t where var X = 46;; dim t <- X end",
  ?assertError({badmatch, {error, undefined_identifier, "X"}}, eval(S)).

dim_cannot_be_redefined_in_same_wheredim() ->
  S = "#.t where dim t <- 46;; dim t <- 58 end",
  ?assertMatch({58,_}, eval(S)). %% FIXME This should give a compile error "Error, dim t already defined."

var_can_refer_to_var_defined_before_in_same_wherevar() ->
  S = "A where var B = 46;; var A = B end",
  ?assertMatch({46,_}, eval(S)).

var_can_refer_to_var_defined_after_in_same_wherevar() ->
  S = "A where var A = B;; var B = 46 end",
  ?assertMatch({46,_}, eval(S)).

var_cannot_be_redefined_in_same_wherevar() ->
  S = "A where var A = 46;; var A = 58 end",
  ?assertMatch({58,_}, eval(S)). %% FIXME This should give a compile error "Error, var A already defined."

var_redefined_in_nested_wherevar_hangs_cache() ->
  S = "A
      where
        var B = C
        where
          var A = 46 // Inner A
          var C = A
        end
        var A = B // Outer A
      end",
  ?assertError({badmatch, hang}, eval(S)). %% XXX Probably same var name in different wherevar clauses shall belong to distinct namespaces, therefore being different variables

var_redefined_in_nested_wherevar_hangs_cache2() ->
  S = "A
      where
        var B = C
        where
          var A = 46 // Inner A
          var C = D
          where
            var D = A
          end
        end
        var A = B // Outer A
      end",
  ?assertError({badmatch, hang}, eval(S)). %% XXX Probably same var name in different wherevar clauses shall belong to distinct namespaces, therefore being different variables

var_redefined_in_nested_wherevar_hangs_cache3() ->
  S = "X
      where
        var X = A
        where
          var B = C
          where
            var A = 46 // Inner A
            var C = D
            where
              var D = A
            end
          end
          var A = B // Outer A
        end
      end",
  ?assertError({badmatch, hang}, eval(S)). %% XXX Probably same var name in different wherevar clauses shall belong to distinct namespaces, therefore being different variables

var_redefined_in_nested_wherevar_is_shadowed_by_outer_if_outer_already_queried() ->
  S = "A
      where
        var B = 46
        var C =
          if B == 46 then // Force evaluation of B
            D
            where
              var B = 58 // Programmer intends to shadow outer B
              var D = B
            end
          else
            1
          fi
        var A = C
      end",
  ?assertMatch({46,_}, eval(S)). %% This is really confusing for the developer


%% Internals - Mocking

mock_tpar() ->
  ok = meck:new(tpar, [passthrough]),
  ok = meck:expect(tpar, eval,
                   fun(Xs, I, E, K, D, W, T) ->
                       tpar:eval_seq(Xs, I, E, K, D, W, T)
                   end).

unmock_tpar() ->
  ok = meck:unload(tpar).

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

t0(T) ->
  ttransform0:transform0(T).

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T).

%% End of Module.
