%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(contextual_semantics_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

local_dim_in_wheredim_clause_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    %% Ref: Feb 2013 cache semantics paper, section 14.1
    %% "Assumptions", point 2, sub-point 2.
    ?_test(wheredim_returning_abs_varying_in_dim_defined_in_same_wheredim()),
    %%
    %% Ref: Feb 2013 cache semantics paper, section 14.1
    %% "Assumptions", point 2, sub-point 1.
    ?_test(recursive_wheredim())
   ]}.


wheredim_returning_abs_varying_in_dim_defined_in_same_wheredim() ->
  S = "BadF!1
      where
        var BadF = // This is a tricky declaration
          (F where fun F!x = #.d end) // This is equivalent to an anonymous function
          where
            dim d <- 46
          end
      end",
  ?assertMatch(
     {46,_},
     %% Upstream TL returns spdim "Invalid dimension" (as the
     %% evaluation of "BadF" returns an abstraction that depends on
     %% dimension "d" without freezing it, and when the body of the
     %% abstraction is evaluated -in the application context-
     %% dimension "d" is not known).  See also:
     %% * Feb 2013 cache semantics paper (section 14.1 "Assumptions"
     %%   re static semantics, point 2, sub-point 2); and
     %% * Nov 2013 semantics paper (section 6 "Conclusions", page 18,
     %%   paragraph with text "... the cache supposes that a dimension
     %%   identifier in a wheredim clause can always be mapped to a
     %%   single dimension ... A proper static semantics is currently
     %%   under development...").
     %%
     %% Semantics alternative to the one currently used by upstream TL
     %% and described above are:
     %% * Ensure that a different dimension be allocated for each
     %%   instantiation of the same wheredim clause, i.e. in the
     %%   currently-being-evaluated expression tree (see Feb 2013
     %%   cache semantics paper, section 14.1 "Assumptions", point 2,
     %%   and Nov 2013 semantics paper, section 2, page 5); or
     %% * Distinguish (/ rename / enumerate) statically all local
     %%   dimensions in wheredim clauses based on their position in
     %%   the static AST, and freeze referenced dimensions in
     %%   abstractions as per transformation described in the Aug 2012
     %%   semantics paper, section 6.4 "Transformation" (notice the
     %%   usage of the "set of hidden dimensions allocated" in
     %%   wheredim clauses and in the list of frozen dimensions in
     %%   abstractions).
     eval(S)).

recursive_wheredim() ->
  S = "FactOf3
      where
        var FactOf3 = BadFact @ [d <- 3]
        dim d <- 0
        var BadFact =
          (if #.n == 0 then 1 else #.n * (BadFact @ [d <- #.n - 1]) fi)
          where
            dim n <- #.d
          end
      end",
  ?assertMatch(
     {6,_},
     %% Upstream TL returns 6 too, even if, based on the Nov 2013
     %% semantics paper, the semantics implemented in upstream TL maps
     %% the local dimension of a wheredim clause always to the same
     %% dimension. This suggests that the first sub-point in
     %% assumption 2 in the Feb 2013 cache semantics paper (section
     %% 14.1 "Assumptions" re static semantics, point 2, sub-point 1)
     %% is not relevant.
     eval(S)).


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

eval(S) when is_list(S) ->
  {ok, T} = tea:string(S),
  tea:eval(T).

%% End of Module.
