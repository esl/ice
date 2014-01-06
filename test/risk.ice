Pij @ [s <- 2, n <- 1]
where
  dim t <- 0

  // Utility Functions
  fun LofPair.d X = X @ [d <- #.d * 2]
  fun RofPair.d X = X @ [d <- #.d * 2 + 1]

  fun fby.d X Y = if #.d <= 0 then X else Y @ [d <- #.d - 1] fi
  fun next.d X = X @ [d <- #.d + 1]

  fun wvr_n.d.n X B = X @ [d <- T]
  where
    var T = fby.d U (U @ [d <- T + 2])
    var U = if B then #.d else next.d U fi
  end

  // State Space
  var a = fby.t 1 (a + 1)
  var d = fby.t 0 (d + 1)

  fun states.d.i1.lim1.i2.lim2 X Y =
    if i1 > lim1 then
      false
    elsif i2 > lim2 then
      states.d.(i1 + 1).lim1.0.lim2 X Y
    else
      fby.d (X @ [t <- i1])
        (fby.d (Y @ [t <- i2]) (states.d.i1.lim1.(i2 + 1).lim2 X Y))
    fi

  dim s <- 0

  var A = 3
  var D = 2

  var S = (states.s.0.A.0.D a d) @ [s <- #.s + 2]

  // Absorbing & Transient States
  var nAbsorbingStates = A + D - 1
  var nTransientStates = A * D - D

  fun isAbsorbingState.s S =
    if (LofPair.s S) >= 2 and (RofPair.s S) == 0 then
      true
    elsif (LofPair.s S) == 1 and (RofPair.s S) > 0 then
      true
    else
      false
    fi

  fun isTransientState.s S = not (isAbsorbingState.s S)

  // P Indices
  dim n <- 0

  var Qij = wvr_n.s.2 (fby.n (LofPair.s S) (RofPair.s S)) (isAbsorbingState.s S)
  var Rij = wvr_n.s.2 (fby.n (LofPair.s S) (RofPair.s S)) (isTransientState.s S)

  var Pij =
    if #.s < (nAbsorbingStates * 2) then
      Qij
    elsif #.s < (nTransientStates * 2) then
      Rij
    else
      false
    fi
end

