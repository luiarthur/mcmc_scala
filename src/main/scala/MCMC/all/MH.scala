package MCMC.all

object MH {
  def metropolis(curr: Double, loglike_plus_prior:Double=>Double, candSig:Double, 
                 inbounds: Double=>Boolean = (x: Double) => {-1E10<x && x<1E10}) = {
    val cand = Rand.nextGaussian(curr,candSig)
    if (inbounds(cand)) {
      val u = math.log(Rand.nextUniform(0,1))
      val p = loglike_plus_prior(cand) - loglike_plus_prior(curr)
      if (p > u) cand else curr
    } else curr
  }
}
