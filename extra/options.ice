// http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter45.html
// An ICE Options Pricer

fun black_scholes_call.S.X.T.r.v = return where
  var d1 = (log.(S/X) + (r + 0.5f0 * pow.v.2) * T) / (v * sqrt.T)
  var d2 = d1 - v * sqrt.T
  var return = S * cnd.d1 - X * exp.(~r * T) * cnd.d2
  fun cnd.X = cnd_ret where
    var L = abs.X
    var K_x = 1 / (1 + 0.2316419f0 * L)
    var K_y = pow.K_x.2
    var K_z = pow.K_x.3
    var K_w = pow.K_x.4
    var dot =  0.31938153f0  * K_x
            + ~0.356563782f0 * K_y
            +  1.781477937f0 * K_z
            + ~1.821255978f0 * K_w
    var W = (dot + 1.330274429f0 * K_w * K_x)
          / sqrt.(2 * Pi) * exp.(~0.5f0 * pow.L.2)
    var Pi = 3.1415926535f0
    var cnd_ret = if X > 0
                  then 1 - W
                  else     W fi
  end
end
