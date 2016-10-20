package MCMC.all

object AFT {
  import Distr.{LogNormal,LogLogistic,Weibull}

  private def mean(x: List[Double]) = x.sum / x.size
  private def variance(x: List[Double]) = {
    val xbar = mean(x)
    val n = x.size
    x.map{xi => (xi-xbar)*(xi-xbar) / (n-1)}.sum
  }

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

  def sample[T <: Gibbs.State](t: List[Double], 
    X: List[List[Double]], v: List[Int], 
    prior: Prior, B: Int, burn: Int, model: String="weibull",
    printEvery: Int=10) = {

    val N = t.size 
    assert(X.size == N && v.size == N, "t,X,v not same size.")

    // log-priors:
    def logPriorBeta(bj: Double, j: Int) = 
      -(bj-prior.m(j)) * (bj-prior.m(j)) / (2*prior.s2(j))

    def logPriorSig(sig: Double) =
      (-prior.a - 1) * math.log(sig) - prior.b/sig

    def loglike(sig: Double, beta: List[Double]) = {
      val mu = X.map(xi => xi.zip(beta).map(xb => xb._1*xb._2).sum)
      val (observed,censored) = {t.zip(mu)}.zip(v).partition(_._2 == 1)
      val o = observed.map{_._1}
      val c = censored.map{_._1}
      val dist = model match {
        case "loglogistic" => LogLogistic
        case "lognormal" => LogNormal
        case _ => Weibull
      }
      val ologlike = o.map{ tmu =>  dist.logpdf(tmu._1, tmu._2, sig) }.sum
      val cloglike = c.map{ tmu => dist.logSurv(tmu._1, tmu._2, sig) }.sum
      ologlike + cloglike
    }

    case class State(beta: List[Double], sig: Double, 
                     betaAcc: List[Int], sigAcc: Int) extends Gibbs.State {
      def update = {
        // update sig
        val (newSig,newSigAcc) = MH.metropolis(sig,
                                    (s:Double)=>loglike(s,beta)+logPriorSig(s),
                                    sigAcc,prior.css,inbounds=(x:Double)=>x>0)
        // update beta
        def updateBeta(b: List[Double], j: Int, acc: List[Int]): (List[Double],List[Int]) = {
          if (j == b.size) (b,acc) else {
            val (bj,accj) = MH.metropolis(b(j),
              (b:Double)=>loglike(newSig,beta.updated(j,b))+logPriorBeta(b,j),
              acc(j),prior.csb(j))
            updateBeta(b.updated(j,bj),j+1,acc.updated(j,accj))
          }
        }
        val (newBeta,newBetaAcc) = updateBeta(beta,0,betaAcc)
        State(newBeta,newSig,newBetaAcc,newSigAcc)
      }
    }

    val P = prior.m.size
    val init = State(List.fill(P)(0.0), 1.0, List.fill(P)(0), 0)

    val samps = Gibbs.sample(init=init,B,burn)
    val d = samps.map(s => -2.0 * loglike(s.sig,s.beta))
    val dic = mean(d) + variance(d) / 2.0
    (samps, dic)
  }
}
