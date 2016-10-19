package MCMC.all

object AFT {
  class Prior(
    // priors for beta ~ Normal(meanVector=m,covMatrix=s2I)
    val m: List[Double],
    val s2: List[Double],
    val csb: List[Double], // candidate sigma for metropolis
    // priors for sigma ~ Inverse Gamma(shape=a, rate=b)
    val a: Double = 2,
    val b: Double = 1,
    val css: Double = 1 // candidate sigma for metropolis
  )

  class Model[T <: Gibbs.State](t: List[Double], 
    X: List[List[Double]], v: List[Int], init: T, 
    prior: Prior, B: Int, burn: Int, printEvery: Int=10) {

      val N = t.size 
      assert(X.size == N && v.size == N, "t,X,v not same size.")

      // log-priors:
      def logPriorBeta(bj: Double, j: Int) = 
        -(bj-prior.m(j)) * (bj-prior.m(j)) / (2*prior.s2(j))

      def logPriorSig(sig: Double) =
        (-prior.a - 1) * math.log(sig) - prior.b/sig

      def loglike(sig: Double, beta: List[Double], 
        model: String="weibull") = {
          val xb = X.map(xi => 
              xi.zip(beta).map(xb => xb._1*xb._2).sum)
          model match {
            case "weibull" => ???
            case "lognormal" => ???
            case "loglogistic" => ???
          }
      }
  }
}
