%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(contextual_semantics_tests).

%% Tests specific to the contextual semantics.

-include_lib("eunit/include/eunit.hrl").

-define(_testMockingTpar(Expr),
        {setup, fun mock_tpar/0, fun(_) -> unmock_tpar() end,
         ?_test(Expr)}).

%% API tests.

wheredim_shall_not_return_abs_varying_in_local_dim_test_() ->
  %% From Feb 2013 cache semantics paper, section 14.1 "Assumptions",
  %% point 2 - focus on sub-point 2:
  %%
  %% "2. [...] The denotational semantics generates different
  %% dimensions for a given local dimension identifier each time that
  %% the local wheredim clause in which it is declared is entered. In
  %% the cache-based implementation, each local dimension identifier
  %% is mapped to the same dimension upon each entry, i.e., the
  %% wheredim clause must be defined in such a way that the different
  %% entries into the clause must be such that a single dimension is
  %% suitable. For this choice to work, a static semantics must ensure
  %% that the corresponding wheredim clause satisfies the following
  %% constraints:
  %% * [...]
  %% * No wheredim clause can return an abstraction that varies in a
  %% local dimension identifier defined in that wheredim clause. To
  %% ensure that this is the case, if a local dimension identifier
  %% appears in the rank of the body of an abstraction, then that
  %% local dimension identifier must appear in the list of frozen
  %% dimensions for that abstraction."
  %%
  %% The tests below codify wheredim closures exposing each one of the
  %% two behaviours described above.
  %%
  %% TODO Understand:
  %% * Why wheredim closures must not "return an abstraction that
  %% varies in a local dimension identifier defined in that wheredim
  %% clause" without freezing it; and
  %% * How generating "different dimensions for a given local
  %% dimension identifier each time that the local wheredim clause in
  %% which it is declared is entered" would help.
  %% Giving answers to these questions would clarify whether this
  %% whole assumption (Feb 2013 cache semantics paper, section 14.1
  %% "Assumptions", point 2, sub-point 2) is relevant, and if ignoring
  %% it could mess up the cache.
  %%
  %% TODO Consider if masking is relevant.
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(wheredim_returning_abs_varying_in_dim_defined_in_same_wheredim()),
    ?_test(wheredim_returning_abs_varying_in_dim_defined_in_same_wheredim_freezing_dim())
   ]}.

wheredim_shall_not_depend_on_same_wheredim_in_different_context_test_() ->
  %% From Feb 2013 cache semantics paper, section 14.1 "Assumptions",
  %% point 2 - focus on sub-point 1:
  %%
  %% "2. [...] The denotational semantics generates different
  %% dimensions for a given local dimension identifier each time that
  %% the local wheredim clause in which it is declared is entered. In
  %% the cache-based implementation, each local dimension identifier
  %% is mapped to the same dimension upon each entry, i.e., the
  %% wheredim clause must be defined in such a way that the different
  %% entries into the clause must be such that a single dimension is
  %% suitable. For this choice to work, a static semantics must ensure
  %% that the corresponding wheredim clause satisfies the following
  %% constraints:
  %% * The evaluation of a wheredim clause in one context cannot
  %% depend on the evaluation of the same wheredim clause in another
  %% context. To ensure that this is the case, the transitive closure
  %% of the dependency graph for the nodes in the abstract syntax tree
  %% must not contain any loops for wheredim clauses.
  %% * [...]
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(wheredim_recursing_on_input_outer_dim()),
    ?_testMockingTpar(wheredim_recursing_on_local_dim()),
    ?_testMockingTpar(wheredim_recursing_on_local_wheredim_and_wrapped_in_fun()),
    ?_test(var_recursing_on_input_outer_dim())
   ]}.


wheredim_returning_abs_varying_in_dim_defined_in_same_wheredim() ->
  S = "BadF!1
      where
        var BadF = // This is a tricky declaration
          (\\ !x -> #.d)
          where
            dim d <- 46
          end
      end",
  ?assertMatch(
     {[{dim,_,"d"}],_},
     %% Upstream TL returns spdim "Invalid dimension" (as the
     %% evaluation of "BadF" returns an abstraction that depends on
     %% dimension "d" without freezing it, and when the body of the
     %% abstraction is evaluated -in the application context-
     %% dimension "d" is not known).
     eval(S)).

wheredim_returning_abs_varying_in_dim_defined_in_same_wheredim_freezing_dim() ->
  S = "BadF!1
      where
        var BadF = // This is a tricky declaration
          (\\ {d} !x -> #.d)
          where
            dim d <- 46
          end
      end",
  ?assertMatch({46,_}, eval(S)). %% Upstream TL returns 46

wheredim_recursing_on_input_outer_dim() ->
  S = "F @ [n <- 3]
      where
        dim n <- 0 // Dimension 'n' is defined outside of variable 'F'.
        var F =
          // 'F' recurses on dimension 'n', therefore recursive 'find'
          // calls to the cache for variable 'F' will be for different
          // contexts (i.e. no hang).
          if #.d == 0 then 1 else #.d * (F @ [n <- #.d - 1]) fi
          where
            dim d <- #.n
          end
      end",
  ?assertMatch({6,_}, eval(S)). %% Upstream TL returns 6

wheredim_recursing_on_local_dim() ->
  S = "F @ [n <- 3]
      where
        dim n <- 0
        var F =
          // 'F' recurses on dimension 'd', therefore recursive 'find'
          // calls to the cache for variable 'F' will be for the same
          // context (dimension 'n' is the same), i.e. hang.
          //
          // XXX It is unclear how generating a different dim each time
          // the wheredim is entered would help.
          if #.d == 0 then 1 else #.d * (F @ [d <- #.d - 1]) fi
          where
            dim d <- #.n
          end
      end",
  ?assertError(
     {badmatch, hang},
     %% Upstream TL cached returns spundef; not cached segfaults
     %% (apparent infinite loop).
     eval(S)).

wheredim_recursing_on_local_wheredim_and_wrapped_in_fun() ->
  %% Ref: Nov 2013 semantics paper, section 2 page 5
  S = "fact.3
      where
        fun fact.n = F
        where
          var F =
            // 'F' recurses on dimension 'd', therefore recursive 'find'
            // calls to the cache for variable 'F' will be for the same
            // context (dimension 'n' is the same), i.e. hang.
            //
            // XXX It is unclear how generating a different dim each time
            // the wheredim is entered would help.
            if #.d == 0 then 1 else #.d * (F @ [d <- #.d - 1]) fi
            where
              dim d <- n
            end
        end
      end",
  {wherevar, _,
   [{"fact", {b_abs, [], [{phi,"n"}],
              {wherevar, "F",
               [{"F", {wheredim, _,
                       %% Dim d is defined inside recursive wheredim F
                       [{{dim,_,"d"}, {'?', {phi,"n"}}}]}}]}}}]} =
    t1(t0(s(S))),
  ?assertError(
     {badmatch, hang},
     %% Upstream TL cached returns spundef; not cached segfaults
     %% (apparent infinite loop).
     eval(S)).

var_recursing_on_input_outer_dim() ->
  %% Ref: Nov 2013 semantics paper, section 2 page 6 (wherevar and
  %% wheredim clauses combined into one)
  S = "fact.3
      where
        fun fact.n = F
        where
          dim d <- n // Dimension 'd' is defined outside of variable 'F'.
          // 'F' recurses on dimension 'd', therefore recursive 'find'
          // calls to the cache for variable 'F' will be for different
          // contexts (i.e. no hang).
          var F =
            if #.d == 0 then 1 else #.d * (F @ [d <- #.d - 1]) fi
        end
      end",
  {wherevar, _,
   [{"fact", {b_abs, [], [{phi,"n"}],
              {wheredim, {wherevar, "F",
                          [{"F", _}]},
               %% Dim d is defined outside recursive wheredim F
               [{{dim,_,"d"}, {'?',{phi,"n"}}}]}}}]} =
    t1(t0(s(S))),
  ?assertMatch({6,_}, eval(S)). %% Upstream TL returns 6


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

t1(T) ->
  ttransform1:transform1(T).

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T).

%% End of Module.
