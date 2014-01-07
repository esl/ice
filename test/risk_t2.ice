Vy1 @ [ y1 <- 6, y2 <- 6, y3<-6, z1 <- 6, z2 <- 6 ]
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
           else 
             if (a1>d1 and a3>d2) or (a1>d2 and a3>d2) then 1 else 0 fi
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
end
