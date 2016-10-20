package MCMC.all

object MH {
  def metropolis(curr: Double, loglike_plus_prior:Double=>Double, acc:Int, candSig:Double, inbounds: Double=>Boolean = (x: Double) => {-1E10 < x && x < 1E10}) = {
    val cand = scala.util.Random.nextGaussian * candSig + curr
    if (inbounds(cand)) {
      val u = math.log(scala.util.Random.nextDouble)
      val p = loglike_plus_prior(cand) - loglike_plus_prior(curr)
      if (p > u) (cand, acc+1) else (curr, acc)
    } else (curr, acc)
  }
}
