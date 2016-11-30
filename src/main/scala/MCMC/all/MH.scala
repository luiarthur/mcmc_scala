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

  // Check this: FIXME
  def met(curr:Double, ll_plus_lp:Double=>Double, candSig:Double) = {
    val cand = Rand.nextGaussian(curr,candSig)
    val u = math.log(Rand.nextUniform(0,1))
    val p = loglike_plus_prior(cand) - loglike_plus_prior(curr)
    if (p > u) cand else curr
  }

  // Check this: FIXME
  def metLogit(curr:Double,ll:Double=>Double,lp:Double=>Double,candSig:Double) = {
    // curr should be between 0 and 1
    
    def logit(p:Double) = math.log(p / (1-p))
    def invLogit(x:Double) = 1.0 / (1.0 + math.exp(-x))

    def logLikePlusLogPrior(logitP: Double) = {
      val p = invLogit(logitP)
      val logJ = -logitP + 2*math.log(p)
      val logPriorLogitP = lp(p) + logJ 

      ll(curr) + logPriorLogitP
    }

    invLogit(met(logit(curr), logLikePlusLogPrior, candSig))
  }
}
