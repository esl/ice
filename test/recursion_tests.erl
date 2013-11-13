%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(recursion_tests).

%% Positive tests for recursive declarations.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

recursion_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(var_recursing_on_dim()),
    ?_test(base_fun_recursing_on_param()),
    ?_test(fun_recursing_on_dim1()),
    ?_test(fun_recursing_on_dim2()),
    ?_test(fun_recursing_on_dim3()),
    ?_test(fun_recursing_on_dim4()),
    ?_test(base_funs_mutually_recursing_on_params())
   ]}.


var_recursing_on_dim() ->
  S = "X
      where
        dim t <- 0
        var X = fact @ [t <- 3]
        var fact = if #.t == 0 then 1 else #.t * fact @ [t <- #.t - 1] fi
      end",
  ?assertMatch({6,_}, eval(S)).

base_fun_recursing_on_param() ->
  S = "fact.3
      where
        fun fact.n = if n == 0 then 1 else n * fact.(n-1) fi
      end",
  ?assertMatch({6,_}, eval(S)).

fun_recursing_on_dim1() ->
  S = "fact.3
      where
        fun fact.n = F
        where
          dim d <- n
          var F = if #.d == 0 then
                    1
                  else
                    #.d * (F @ [d <- #.d - 1])
                  fi
        end
      end",
  ?assertMatch({6,_}, eval(S)).

fun_recursing_on_dim2() ->
  S = "fact.3
      where
        fun fact.n = F
        where
          dim d <- n
          var F = if #.d == 0 then
                    1
                  else
                    ((#.d + 1) * F) @ [d <- #.d - 1]
                  fi
        end
      end",
  ?assertMatch({6,_}, eval(S)).

fun_recursing_on_dim3() ->
  T = {where, s("fact.3"),
       [{fn, "fact", [{b_param,"n"}],
         {where, "F",
          [{dim, "d", "n"},
           {var, "F",
            {'if', s("#.d == 0"),
             s("1"),
             {'@', tprimop:times(
                     {fn_call, "index", [{v_param,{dim,"d"}}]},
                     s("F")),
              s("[d <- #.d - 1]")}}}]}},
        {fn, "index", [{v_param,"d"}],
         tprimop:plus({'#',s("d")}, s("1"))}]},
  %% "fact.3
  %% where
  %%   fun fact.n = F
  %%   where
  %%     dim d <- n
  %%     var F = if #.d == 0 then
  %%               1
  %%             else
  %%               (index!d * F) @ [d <- #.d - 1]
  %%             fi
  %%   end
  %%   fun index!d = #.d + 1
  %% end"
  ?assertMatch({6,_}, eval(T)).

fun_recursing_on_dim4() ->
  T = {where, s("fact.0"),
       [{fn, "fact", [{b_param,"n"}],
         {where, "F",
          [{dim, "d", "n"},
           {var, "F",
            {'if', s("#.d == 0"),
             s("1"),
             {'@', tprimop:times(
                     {fn_call, "index", [{v_param,{dim,"d"}}]},
                     s("F")),
              s("[d <- #.d - 1]")}}},
           {fn, "index", [{v_param,"d"}],
            tprimop:plus({'#',s("d")}, s("1"))}]}}]},
  %% "fact.3
  %% where
  %%   fun fact.n = F
  %%   where
  %%     dim d <- n
  %%     var F = if #.d == 0 then
  %%               1
  %%             else
  %%               (index!d * F) @ [d <- #.d - 1]
  %%             fi
  %%     fun index!d = #.d + 1
  %%   end
  %% end"
  ?assertMatch({1,_}, eval(T)). %% XXX Computation should be fact.3, not fact.0. TODO Fix semantics

base_funs_mutually_recursing_on_params() ->
  S = "is_even.46
      where
        fun is_even.n = if n == 0 then true  else is_odd .(n-1) fi
        fun is_odd .n = if n == 0 then false else is_even.(n-1) fi
      end",
  ?assertMatch({true,_}, eval(S)).


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
