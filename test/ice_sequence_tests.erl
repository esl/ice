%% See the LICENSE file for licensing information
%% -*- coding: utf-8 -*-
-module(ice_sequence_tests).

-include_lib("eunit/include/eunit.hrl").

ice_sequence_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    ?_test(sequence_parses()),
    ?_test(sequence_evaluates())
   ]}.

%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------
sequence_parses() ->
  Case1 = "1 + 2; 2 - 1",
  ?assertMatch({seq, 
		{primop, '+', [{int, 1}, {int, 2}]},
		{primop, '-', [{int, 2}, {int, 1}]}},
	       ice_string:parse(Case1)).

sequence_evaluates() ->
  Case1 = "1 + 2; 2 - 1",
  ?assertMatch({1, _}, ice:i(Case1)).

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------
setup() -> 
  not_implemented.

cleanup(_) -> 
  not_implemented.
