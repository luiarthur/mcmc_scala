package MCMC.all

object Distr {
  import org.apache.commons.math3.distribution.{NormalDistribution}
  import math.{log,exp,Pi}

  private val N01 = new NormalDistribution(0,1)

  trait ErrorDistribution {
    def logpdf(x: Double): Double
    def cdf(x: Double): Double
  }

  object Normal extends ErrorDistribution {
    def logpdf(x: Double) = -(log(2.0*Pi) - x*x)/2.0
    def cdf(x: Double) = N01.cumulativeProbability(x)
  }

  object Logistic extends ErrorDistribution {
    def logpdf(x: Double) = -x -2.0*log(1.0+exp(-x))
    def cdf(x: Double) = 1/(exp(-x) + 1)
  }

  object ExtremeValue extends ErrorDistribution {
    def logpdf(x: Double) = -x -exp(-x)
    def cdf(x: Double) = exp(-exp(-x))
  }

  trait TimeDistribution {
    val errDistr: ErrorDistribution
    def logpdf(t: Double, mu: Double, s: Double) = {
      assert(t > 0 && s > 0, "t,s should be > 0")
      errDistr.logpdf((log(t)-mu)/s) - log(t*s)
    }
    def logSurv(t: Double, mu: Double, s: Double) = {
      assert(t > 0 && s > 0, "t,s should be > 0")
      log(1-errDistr.cdf((log(t)-mu)/s))
    }
  }

  object LogNormal extends TimeDistribution { val errDistr = Normal }
  object LogLogistic extends TimeDistribution { val errDistr = Logistic }
  object Weibull extends TimeDistribution { val errDistr = ExtremeValue }
}
