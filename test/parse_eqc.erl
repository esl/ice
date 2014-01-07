%%% @doc
%%%
%%% @end


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

eval(T, X, Y) ->
  {ok, Ast} = tea:string(relax_string(T, X, Y)),
  tea:eval(Ast).


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


%% risk
risk_value(X,Y,Z) ->
  SFormat =
    "i! (S @ [x <- ~B, y <- ~B, z <- ~B]) @ [t <- 0]
     where
     var S = if #.x >= #.y 
              then if #.y >= #.z then P.t (#.x) (#.y)
                                 else if #.x >= #.z then P.t (#.x) (#.y)
                                                    else P.t (#.z) (#.y) fi
                   fi
              else if #.x >= #.z then P.t (#.y) (#.x)
                                 else if #.y >= #.z then P.t (#.y) (#.z)
                                                    else P.t (#.z) (#.y) fi
                   fi
             fi
     dim t <- 0
     dim x <- 0
     dim y <- 0
     dim z <- 0
     fun P.d X Y = i^ {d} if #.d==0 then X elsif #.d == 1 then Y else 0 fi  
     end",
  S= lists:flatten(io_lib:format(SFormat, [X,Y,Z])),
  {ok, Ast} = tea:string(S),
  tea:eval(Ast).

freq2of3(X,Y) ->
  SFormat =
    "Max2 @ [x <- ~B, y <- ~B]
     where
       var Max2 = if #.x > #.y then #.x else #.y fi
       dim x <- 0
       dim y <- 0 
     end",
  S= lists:flatten(io_lib:format(SFormat, [X,Y])),
  {ok, Ast} = tea:string(S),
  tea:eval(Ast).

rv(X,Y) ->
  SFormat =
    "S @ [x <- ~B, y <- ~B]
     where
     var S = if #.x >= #.y 
              then P.t (#.x) (#.y)
              else P.t (#.y) (#.x) fi
     dim t <- 0
     dim x <- 0
     dim y <- 0
     fun P.d X Y = if #.d == 0 then X elsif #.d == 1 then Y else 0 fi  
     end",
  S= lists:flatten(io_lib:format(SFormat, [X,Y])),
  {ok, Ast} = tea:string(S),
  tea:eval(Ast).

sieve(N) ->
  SFormat =
    "sieve @ [d <- ~B]
     where  
       fun sieve.d = S where
       dim da <- 0
       var S = fby.d (#.da + 2)
                    􏰅(wvr.da S S mod (first.da S) ̸= 0))􏰆􏰆
       fun first.d X = X @ [d <- 0]
       fun wvr.d X Y = if first.d Y
                        then fby.d X 􏰅wvr.d (next.d X) (next.d Y )􏰆
                        else wvr.d (next.d X) (next.d Y ) fi
       fun fby.d X Y = if #.d == 0 then X else Y @[d<- #.d−1] fi
     end
     end",
  my_eval(SFormat, [N]).

my_eval(SFormat, Args) ->
    S = ice_format(SFormat, Args),
    {ok, Ast} = ice:string(S),
    ice:eval(Ast).

ice_format(SFormat, Args) ->
    lists:flatten(io_lib:format(SFormat, Args)).

risk(X, Y) ->
  SFormat = 
    "W @ [a <- ~B, d <- ~B]
     where
     dim a <- 0
     dim d <- 0
     var W = if #.d <= 0
              then 1
              else if #.a <=1 
                    then 0
                    else P10 @ [a <- #.a, d <- #.d] * W @ [ a <- #.a-1, d <- #.d ] +
                         P01 @ [a <- #.a, d <- #.d] * W @ [ a <- #.a,   d <- #.d -1 ] +
                         P11 @ [a <- #.a, d <- #.d] * W @ [ a <- #.a - 1,   d <- #.d -1 ] +
                         P20 @ [a <- #.a, d <- #.d] * W @ [ a <- #.a -2 ,   d <- #.d] +
                         P02 @ [a <- #.a, d <- #.d] * W @ [ a <- #.a,   d <- #.d - 2 ]
                    fi
             fi
     var P10 = if #.d == 1 
                then if #.a == 2 
                      then 0.5834f0
                      else if #.a == 3
                            then 0.4231f0
                            else 0.3403f0
                           fi
                      fi
                else if #.a == 2 
                      then 0.7454f0
                      else 0.0f0 // attacker will always roll as many as possible
                     fi
               fi
     var P01 = if #.d == 1 
                then if #.a == 2 
                      then 0.4166f0
                      else if #.a == 3
                            then 0.5787f0
                            else 0.6597f0
                           fi
                      fi
                else if #.a == 2 
                      then 0.2546f0
                      else 0.0f0 // attacker will always roll as many as possible
                     fi
               fi
     var P11 = if #.d >= 2 
                then if #.a == 3 
                      then 0.3241f0
                      else if #.a > 3 
                            then 0.3357f0
                            else 0.0f0
                           fi
                      fi
                else 0.0f0
               fi
     var P20 = if #.d >= 2 
                then if #.a == 3 
                      then 0.4483f0
                      else if #.a > 3 
                            then 0.2926f0
                            else 0.0f0
                           fi
                      fi
                else 0.0f0
               fi
     var P02 = if #.d >= 2 
                then if #.a == 3 
                      then 0.2276f0
                      else if #.a > 3 
                            then 0.3717f0
                            else 0.0f0
                           fi
                      fi
                else 0.0f0
               fi 
     end
   ",
  ice:i(ice_format(SFormat, [X, Y])).


a_win_2_eq_eq() ->
  SFormat = 
    "Vy @ [ y <- 6, z <- 6]
     where
       dim y <- 0
       dim z <- 0
       var Vy = if #.y == 1 
                then 0
                else Vz @ [ y <- #.y, z <- #.z ] +
                     Vy @ [ y <- #.y -1, z <- #.z]
               fi     
       var Vz = if #.z == 0 
                then 0
                else if #.y > #.z
                      then #.z
                      else Vz @ [ y <- #.y, z <- #.z -1 ]
                     fi
               fi  
     end",
  ice:i(SFormat).

a_win_1_2v1() ->
  SFormat = 
    "Vy1 @ [ y1 <- 6, y2 <- 6,  z <- 6 ]
     where
       dim y1 <- 0
       dim y2 <- 0
       dim z <- 0
       var Vy1 = if #.y1 == 1 
                  then a_win_1_2v1.(#.y1).(#.y2).(#.z) + 
                       Vy2 @ [ y2 <- #.y2-1] +
                       Vz  @ [ z <- #.z-1]
                  else a_win_1_2v1.(#.y1).(#.y2).(#.z) + 
                       Vy1 @ [ y1 <- #.y1 - 1] +
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vz @  [ z <- #.z - 1 ] 
                 fi     
       var Vy2 = if #.y2 == 1 
                  then a_win_1_2v1.(#.y1).(#.y2).(#.z) + Vz @ [ z <- #.z - 1]
                  else a_win_1_2v1.(#.y1).(#.y2).(#.z) + 
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vz @  [ z <- #.z - 1 ]
               fi  
       var Vz = if #.z == 1 
                then a_win_1_2v1.(#.y1).(#.y2).(#.z) 
                else a_win_1_2v1.(#.y1).(#.y2).(#.z) + 
                     Vz @ [ z <- #.z - 1 ]
               fi  


       fun a_win_1_2v1.a1.a2.d = 
            if a1 > d or a2 > d 
             then 1
             else 0
            fi
     end",
  ice:i(SFormat).

a_win_2_2v2() ->
  SFormat = 
    "Vy1 @ [ y1 <- 6, y2 <- 6,  z1 <- 6, z2 <- 6 ]
     where
       dim y1 <- 0
       dim y2 <- 0
       dim z1 <- 0
       dim z2 <- 0
       var Vy1 = if #.y1 == 1 
                  then a_win_2_2v2.(#.y1).(#.y2).(#.z1).(#.z2) + 
                       Vy2 @ [ y2 <- #.y2-1] +
                       Vz1  @ [ z1 <- #.z1-1] +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_2v2.(#.y1).(#.y2).(#.z1).(#.z2) + 
                       Vy1 @ [ y1 <- #.y1 - 1] +
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vz1 @  [ z1 <- #.z1 - 1 ] +
                       Vz2  @ [ z2 <- #.z2-1]
                 fi     
       var Vy2 = if #.y2 == 1 
                  then a_win_2_2v2.(#.y1).(#.y2).(#.z1).(#.z2) + 
                       Vz1 @ [ z1 <- #.z1 - 1]  +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_2v2.(#.y1).(#.y2).(#.z1).(#.z2) + 
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vz1 @ [ z1 <- #.z1 - 1 ]  +
                       Vz2 @ [ z2 <- #.z2-1]
               fi  
       var Vz1 = if #.z1 == 1 
                then a_win_2_2v2.(#.y1).(#.y2).(#.z1).(#.z2) +
                     Vz2 @ [ z2 <- #.z2-1]
                else a_win_2_2v2.(#.y1).(#.y2).(#.z1).(#.z2) + 
                     Vz1 @ [ z1 <- #.z1 - 1 ]  +
                     Vz2 @ [ z2 <- #.z2-1]
               fi  
       var Vz2 = if #.z2 == 1 
                then a_win_2_2v2.(#.y1).(#.y2).(#.z1).(#.z2) 
                else a_win_2_2v2.(#.y1).(#.y2).(#.z1).(#.z2) + 
                     Vz2 @ [ z2 <- #.z2-1]
               fi  


       fun a_win_2_2v2.a1.a2.d1.d2 = 
            if (a1 > d1 and a2 > d2) or (a1 > d2 and a2 > d1)
             then 1
             else 0
            fi
     end",
  ice:i(SFormat).


a_win_2_3v2() ->
  SFormat = 
    "Vy1 @ [ y1 <- 6, y2 <- 6, y3<-6, z1 <- 6, z2 <- 6 ]
     where
       dim y1 <- 0
       dim y2 <- 0
       dim y3 <- 0
       dim z1 <- 0
       dim z2 <- 0
       var Vy1 = if #.y1 == 1 
                  then a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) + 
                       Vy2 @ [ y2 <- #.y2-1] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1  @ [ z1 <- #.z1-1] +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) + 
                       Vy1 @ [ y1 <- #.y1 - 1] +
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @  [ z1 <- #.z1 - 1 ] +
                       Vz2  @ [ z2 <- #.z2-1]
                 fi     
       var Vy2 = if #.y2 == 1 
                  then a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) + 
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @ [ z1 <- #.z1 - 1]  +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) + 
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @ [ z1 <- #.z1 - 1 ]  +
                       Vz2 @ [ z2 <- #.z2-1]
               fi  
       var Vy3 = if #.y3 == 1 
                  then a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) + 
                       Vz1 @ [ z1 <- #.z1 - 1]  +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) + 
                       Vy3 @ [ y3 <- #.y3 - 1 ] +
                       Vz1 @ [ z1 <- #.z1 - 1 ]  +
                       Vz2 @ [ z2 <- #.z2-1]
               fi  

       var Vz1 = if #.z1 == 1 
                then a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) +
                     Vz2 @ [ z2 <- #.z2-1]
                else a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) + 
                     Vz1 @ [ z1 <- #.z1 - 1 ]  +
                     Vz2 @ [ z2 <- #.z2-1]
               fi  
       var Vz2 = if #.z2 == 1 
                then a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) 
                else a_win_2_3v2.(#.y1).(#.y2).(#.y3).(#.z1).(#.z2) + 
                     Vz2 @ [ z2 <- #.z2-1]
               fi  

       fun a_win_2_3v2.a1.a2.a3.d1.d2 = 
            if a1 >= a2 
            then if a2 >= a3 
                   then if (a1>d1 and a2>d2) or (a1>d2 and a2>d2) then 1 else 0 fi
                   else if (a1>d1 and a3>d2) or (a1>d2 and a3>d2) then 1 else 0 fi
                  fi
             else if a1 >= a3 
                   then if (a1>d1 and a2>d2) or (a1>d2 and a2>d2) then 1 else 0 fi
                   else if (a2>d1 and a3>d2) or (a2>d2 and a3>d2) then 1 else 0 fi
                  fi
            fi

//       fun a_win_2_3v2.a1.a2.a3.d1.d2 = 
//            if a1 >= a2 
//            then if a2 >= a3 
//                   then a_win_2_2v2.a1.a2.d1.d2
//                   else a_win_2_2v2.a1.a3.d1.d2
//                  fi
//             else if a1 >= a3 
//                   then a_win_2_2v2.a1.a2.d1.d2
//                   else a_win_2_2v2.a2.a3.d1.d2
//                  fi
//            fi

       fun a_win_2_2v2.a1.a2.d1.d2 = 
            if (a1 > d1 and a2 > d2) or (a1 > d2 and a2 > d1)
             then 1
             else 0
            fi
     end",
  ice:i(SFormat).

hack() ->
  SFormat = 
    "Vy1 @ [ y1 <- 6, y2 <- 6, y3<-6, z1 <- 6, z2 <- 6 ]
     where
       dim y1 <- 0
       dim y2 <- 0
       dim y3 <- 0
       dim z1 <- 0
       dim z2 <- 0
       var Vy1 = if #.y1 == 1 
                  then a_win_2_3v2 @ [ y1 <- #.y1 ] + 
                       Vy2 @ [ y2 <- #.y2-1] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1  @ [ z1 <- #.z1-1] +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2 @ [ y1 <- #.y1 ] + 
                       Vy1 @ [ y1 <- #.y1 - 1] +
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @  [ z1 <- #.z1 - 1 ] +
                       Vz2  @ [ z2 <- #.z2-1]
                 fi     
       var Vy2 = if #.y2 == 1 
                  then a_win_2_3v2 @ [ y1 <- #.y1 ] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @ [ z1 <- #.z1 - 1]  +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2 @ [ y1 <- #.y1 ] + 
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @ [ z1 <- #.z1 - 1 ]  +
                       Vz2 @ [ z2 <- #.z2-1]
               fi  
       var Vy3 = if #.y3 == 1 
                  then a_win_2_3v2 @ [ y1 <- #.y1 ] +
                       Vz1 @ [ z1 <- #.z1 - 1]  +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2 @ [ y1 <- #.y1 ] +
                       Vy3 @ [ y3 <- #.y3 - 1 ] +
                       Vz1 @ [ z1 <- #.z1 - 1 ]  +
                       Vz2 @ [ z2 <- #.z2-1]
               fi  

       var Vz1 = if #.z1 == 1 
                then a_win_2_3v2 @ [ y1 <- #.y1 ] +
                     Vz2 @ [ z2 <- #.z2-1]
                else a_win_2_3v2 @ [ y1 <- #.y1 ] +
                     Vz1 @ [ z1 <- #.z1 - 1 ]  +
                     Vz2 @ [ z2 <- #.z2-1]
               fi  
       var Vz2 = if #.z2 == 1 
                then a_win_2_3v2 @ [ y1 <- #.y1 ] 
                else a_win_2_3v2 @ [ y1 <- #.y1 ] +
                     Vz2 @ [ z2 <- #.z2-1 ]
               fi  

       var a_win_2_3v2 = 
            if #.y1 >= #.y2 
            then if #.y2 >= #.y3 
                   then if (#.y1>#.z1 and #.y2>#.z2) or (#.y1>#.z2 and #.y2>#.z2) then 1 else 0 fi
                   else if (#.y1>#.z1 and #.y3>#.z2) or (#.y1>#.z2 and #.y3>#.z2) then 1 else 0 fi
                  fi
             else if #.y1 >= #.y3 
                   then if (#.y1>#.z1 and #.y2>#.z2) or (#.y1>#.z2 and #.y2>#.z2) then 1 else 0 fi
                   else if (#.y2>#.z1 and #.y3>#.z2) or (#.y2>#.z2 and #.y3>#.z2) then 1 else 0 fi
                  fi
            fi

     end",
  ice:i(SFormat).


hack2() ->
  SFormat = 
    "Vy1 @ [ y1 <- 6, y2 <- 6, y3<-6, z1 <- 6, z2 <- 6 ]
     where
       dim y1 <- 0
       dim y2 <- 0
       dim y3 <- 0
       dim z1 <- 0
       dim z2 <- 0
       dim a1 <- 0
       dim a2 <- 0
       dim a3 <- 0
       dim d1 <- 0
       dim d2 <- 0
       var Vy1 = if #.y1 == 1 
                  then a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] + 
                       Vy2 @ [ y2 <- #.y2-1] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1  @ [ z1 <- #.z1-1] +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] + 
                       Vy1 @ [ y1 <- #.y1 - 1] +
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @  [ z1 <- #.z1 - 1 ] +
                       Vz2  @ [ z2 <- #.z2-1]
                 fi     
       var Vy2 = if #.y2 == 1 
                  then a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @ [ z1 <- #.z1 - 1]  +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] + 
                       Vy2 @ [ y2 <- #.y2 - 1 ] +
                       Vy3 @ [ y3 <- #.y3-1] +
                       Vz1 @ [ z1 <- #.z1 - 1 ]  +
                       Vz2 @ [ z2 <- #.z2-1]
               fi  
       var Vy3 = if #.y3 == 1 
                  then a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] +
                       Vz1 @ [ z1 <- #.z1 - 1]  +
                       Vz2  @ [ z2 <- #.z2-1]
                  else a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] +
                       Vy3 @ [ y3 <- #.y3 - 1 ] +
                       Vz1 @ [ z1 <- #.z1 - 1 ]  +
                       Vz2 @ [ z2 <- #.z2-1]
               fi    

       var Vz1 = if #.z1 == 1 
                then a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] +
                     Vz2 @ [ z2 <- #.z2-1]
                else a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] +
                     Vz1 @ [ z1 <- #.z1 - 1 ]  +
                     Vz2 @ [ z2 <- #.z2-1]
               fi  
       var Vz2 = if #.z2 == 1 
                then a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] 
                else a_win_2_3v2 @ [ a1 <- #.y1, a2<-#.y2, a3<-#.y3, d1<-#.z1, d2<-#.z2 ] +
                     Vz2 @ [ z2 <- #.z2-1 ]
               fi  

       var a_win_2_3v2 = 
            if #.a1 >= #.a2 
            then if #.a2 >= #.a3 
                   then if (#.a1>#.d1 and #.a2>#.d2) or (#.a1>#.d2 and #.a2>#.d2) then 1 else 0 fi
                   else if (#.a1>#.d1 and #.a3>#.d2) or (#.a1>#.d2 and #.a3>#.d2) then 1 else 0 fi
                  fi
             else if #.a1 >= #.a3 
                   then if (#.a1>#.d1 and #.a2>#.d2) or (#.a1>#.d2 and #.a2>#.d2) then 1 else 0 fi
                   else if (#.a2>#.d1 and #.a3>#.d2) or (#.a2>#.d2 and #.a3>#.d2) then 1 else 0 fi
                  fi
            fi

     end",
  ice:i(SFormat).


%% nat() ->
%%   ?SUCHTHAT(N, int(), N>=0).




   
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
