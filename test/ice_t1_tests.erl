-module(ice_t1_tests).

-include_lib("eunit/include/eunit.hrl").

%% (\\b { ia in ... } a b c -> E0)
%% (\\v { ia in ... } a b c -> E0)
%% Tests which verify that base abstractions contain dimensions 
%% generated for intensions which are in the right position 
%% according to the hidden dimension generation rule.

b_abs_intensions_test() ->
  BAbs1 = "\\ {x,y}.a.b -> a",

  TAbs1 = ice_transform:t1s(BAbs1),

  %%io:format(user, "TAbs1 = ~p~n", [TAbs1]),

  {foreach, fun setup/0, fun cleanup/1,
   [ ?assertMatch({b_abs, _, _, _, _, _}, ice_transform:t1s(BAbs1)) ]}.

b_abs_hidden_dims() ->
  ok.

b_abs_transformed_args() ->
  ok.

b_abs_position() ->
  ok.

v_abs_intensions() ->
  ok.

v_abs_hidden_dims() ->
  ok.

v_abs_transformed_args() ->
  ok.

v_abs_position() ->
  ok.

setup() ->
  ice:start().

cleanup(_) ->
  ice:stop().
  
  
