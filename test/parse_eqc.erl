%%% @author Torben Hoffmann <>
%%% @copyright (C) 2013, Torben Hoffmann
%%% @doc
%%%
%%% @end
%%% Created : 28 Nov 2013 by Torben Hoffmann <>

-module(parse_eqc).


-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_parse_relax() ->
  ?FORALL({T,X,Y}, {nat(), my_x(), my_y()},
          parse(T,X,Y)).



parse(T,X,Y) ->
  S = relax_string(T,X,Y),
  case tea:string(S) of
    {ok,_T} ->
      true;
    _ ->
      false
  end.

relax_string(T,X,Y) ->
  SFormat =
    "S @ [t <- ~B, x <- ~B, y <- ~B]
    where
      // Electrode has always been at (3,4) with potential 5 - and forever will
      var POTENTIAL = if #.x == 3 && #.y == 4 then 5 else 0 fi
      var S = if POTENTIAL != 0 then POTENTIAL else fby.t 0 (avg S) fi
      fun avg A = ((prev.x A + next.x A) + (prev.y A + next.y A)) / 4
    end
    where
      dim t <- 0 // time
      dim x <- 0
      dim y <- 0
    end
    where
      fun fby.d X Y = if #.d == 0 then X else Y @ [d <- #.d - 1] fi
      fun prev.d M = M @ [d <- #.d - 1]
      fun next.d M = M @ [d <- #.d + 1]
    end",
    lists:flatten(io_lib:format(SFormat, [T,X,Y])).

my_x() ->
  nat().
%%  ?SUCHTHAT(N, int(), N>-10 andalso N<10).

my_y() ->
  my_x().

%% nat() ->
%%   ?SUCHTHAT(N, int(), N>=0).

   
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
