%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(recursion_tests).

-include_lib("eunit/include/eunit.hrl").


%% API tests.

recursion_test_() ->
  {foreach,
   _Setup = fun() -> {ok, Pid} = tcache:start_link(100), Pid end,
   _Cleanup = fun(Pid) -> tcache_stop(Pid) end,
   [
    ?_assertMatch({6,_}, eval(var_recursing_on_dim())),
    ?_assertMatch(
       {[{phi,"n"}],_}, %% XXX Result is wrong, it should be 6. TODO Fix semantics
       eval(base_fun_recursing_on_param())),
    ?_assertMatch(
       {1,_}, %% XXX Computation should be fact.3, not fact.0. TODO Fix semantics
       eval(fun_recursing_on_dim())),
    ?_assertMatch(
       {[{phi,"n"}],_}, %% XXX Result is wrong, it should be true. TODO Fix semantics
       eval(base_funs_mutually_recursing_on_params()))
   ]}.


var_recursing_on_dim() ->
  "X
  where
    dim t <- 0
    var X = fact @ [t <- 3]
    var fact = if #.t == 0 then 1 else #.t * fact @ [t <- #.t - 1] fi
  end".

base_fun_recursing_on_param() ->
  {where, s("fact.3"),
   [{fn, "fact", [{b_param,"n"}],
     {'if', s("n == 0"),
      s("1"),
      tprimop:times(s("n"),
                    s("fact.(n-1)"))}}]}.
  %% "fact.3
  %% where
  %%   fun fact.n = if n == 0 then 1 else n * fact.(n-1) fi
  %% end".

fun_recursing_on_dim() ->
  {where, s("fact.0"),
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
        tprimop:plus({'#',s("d")}, s("1"))}]}}]}.
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
  %% end".

base_funs_mutually_recursing_on_params() ->
  {where, s("is_even.46"),
   [{fn, "is_even", [{b_param,"n"}],
     {'if', s("n == 0"), s("true" ), s("is_odd .(n-1)")}},
    {fn, "is_odd" , [{b_param,"n"}],
     {'if', s("n == 0"), s("false"), s("is_even.(n-1)")}}]}.
  %% "is_even.46
  %% where
  %%   fun is_even.n = if n == 0 then true  else is_odd .(n-1) fi
  %%   fun is_odd .n = if n == 0 then false else is_even.(n-1) fi
  %% end".


%% Internals

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
