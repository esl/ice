%%------------------------------------------------------------------------------
%% Function closure
%%------------------------------------------------------------------------------
-module(tclosure).

-export([close_shallowest_abs_in_wherevar_expressions/3,
         close_abs/3]).

%%------------------------------------------------------------------------------
%% This module closes expressions over the environment, and also over
%% the interpretation function.
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Close the shallowest abstractions in a wherevar.
%%
%% Close the shallowest abstractions in the specified new expressions
%% defined in a wherevar clause.
%%
%% Only the shallowest abstractions are closed, and not all the
%% expressions, in order to keep the abstract syntax tree as clean as
%% possible.  Closing all expressions (i.e. not only the abstractions)
%% would imply generating unnecessary/untidy abstract syntax tree
%% nodes, e.g. a closure node with only a ground value inside.
%%------------------------------------------------------------------------------
close_shallowest_abs_in_wherevar_expressions(XiEis, I, E) ->
  EPert = tset:perturb(E, XiEis), %% XXX This perturbation could be a union...
  %% ... if variables in different wherevar clauses were distinct even
  %% if with same name.
  [{Xi, close_shallowest_abs(Ei, I, EPert)} || {Xi, Ei} <- XiEis].

%%------------------------------------------------------------------------------
%% @doc Close the specified abstraction.
%%------------------------------------------------------------------------------
close_abs({b_abs, _Is, _Params, _E0}=Abs, I, E) ->
  {closure, I, E, Abs};
close_abs({v_abs, _Is, _Params, _E0}=Abs, I, E) ->
  {closure, I, E, Abs};
close_abs({i_abs, _Is,          _E0}=Abs, I, E) ->
  {closure, I, E, Abs}.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-define(IS_VAR_ID(Xi), is_list(Xi) orelse is_atom(Xi)).

%%------------------------------------------------------------------------------
%% @doc Close all AST nodes until the shallowest abstraction.
%%
%% This function considers nested wherevars.
%% @private
%%------------------------------------------------------------------------------
close_shallowest_abs(Const, _I, _E) when is_number(Const) orelse
                                         is_boolean(Const) ->
  Const;
close_shallowest_abs({string, _Str}=Const, _I, _E) ->
  Const;
close_shallowest_abs({char, _Char}=Const, _I, _E) ->
  Const;
close_shallowest_abs({primop, F, Eis}, I, E) ->
  %% If primop were not able to return one of its arguments when an
  %% abstraction, there would be no need to close arguments.
  {primop, F, lists:map(fun(Ei) -> close_shallowest_abs(Ei, I, E) end, Eis)};
close_shallowest_abs({t, Es}, I, E) ->
  {t, lists:map(
        fun({Xi,Ei}) ->
            {close_shallowest_abs(Xi, I, E),
             close_shallowest_abs(Ei, I, E)}
        end,
        Es)};
close_shallowest_abs({'@', E0, E1}, I, E) ->
  {'@', close_shallowest_abs(E0, I, E), close_shallowest_abs(E1, I, E)};
close_shallowest_abs({'if', E0, E1, E2}, I, E) ->
  {'if', close_shallowest_abs(E0, I, E),
   close_shallowest_abs(E1, I, E),
   close_shallowest_abs(E2, I, E)};
close_shallowest_abs({Q, _E0}=Expr, _I, _E) when Q == '#' orelse Q == '?' ->
  %% Context query can return abstraction if in context. There is
  %% nothing to be closed over the environment.
  Expr;
close_shallowest_abs({b_abs, _Is, _Params, _E0}=Abs, I, E) ->
  close_abs(Abs, I, E);
close_shallowest_abs({b_apply, E0, Eis}, I, E) ->
  {b_apply, close_shallowest_abs(E0, I, E),
   lists:map(fun(Ei) -> close_shallowest_abs(Ei, I, E) end, Eis)};
close_shallowest_abs({v_abs, _Is, _Params, _E0}=Abs, I, E) ->
  close_abs(Abs, I, E);
close_shallowest_abs({v_apply, E0, Eis}, I, E) ->
  {v_apply, close_shallowest_abs(E0, I, E),
   lists:map(fun(Ei) -> close_shallowest_abs(Ei, I, E) end, Eis)};
close_shallowest_abs({i_abs, _Is,          _E0}=Abs, I, E) ->
  close_abs(Abs, I, E);
close_shallowest_abs({i_apply, E0     }, I, E) ->
  {i_apply, close_shallowest_abs(E0, I, E)};
close_shallowest_abs({closure, _ClI, _ClE, _Abs}=Expr, _I, _E) ->
  %% Abstraction already closed when closing outer wherevar.
  Expr;
close_shallowest_abs({wherevar, E0, XiEis}, I, E) ->
  XiClEis = close_shallowest_abs_in_wherevar_expressions(XiEis, I, E),
  {wherevar, close_shallowest_abs(E0, I, tset:perturb(E, XiClEis)), XiClEis};
close_shallowest_abs({wheredim, E0, XiEis}, I, E) ->
  {wheredim, close_shallowest_abs(E0, I, E),
   lists:keymap(fun(Ei) -> close_shallowest_abs(Ei, I, E) end, 2, XiEis)};
close_shallowest_abs({dim,{_Pos,_Idx},Xi}=Di, _I, _E) when ?IS_VAR_ID(Xi) ->
  Di;
close_shallowest_abs({phi,Xi}=Di, _I, _E) when ?IS_VAR_ID(Xi) ->
  Di;
close_shallowest_abs(Xi, _I, _E) when ?IS_VAR_ID(Xi) ->
  Xi.
