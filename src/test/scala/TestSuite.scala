import org.scalatest.FunSuite

class TestSuite extends FunSuite {
  import MCMC.all.timer
  import math.sqrt

  def round(x: Double, d: Int = 2) = (scala.math.pow(10,d) * x).toInt / scala.math.pow(10,d)
  // Post-processing
  def mean(x: List[Double]) = x.sum / x.size
  def sd(x: List[Double]) = {
    val xbar = mean(x)
    val n = x.size
    val sqDiff = x.map{xi => (xi-xbar)*(xi-xbar)}
    sqrt( sqDiff.sum / (n-1) )
  }


  test("Gibbs") { // - julia is 2-3 times faster than scala
    import MCMC.all._
    Rand.reSeed(1L)

    // Generate Data
    val (mu,sig2,n) = (5.0,2.0,1000)
    val y = List.fill(n)(Rand.nextGaussian(mu,sqrt(sig2)))
    val ybar = y.sum / n

    // Extend State class & define samplers for full conditionals
    case class State(mu: Double, sig2: Double) extends Gibbs.State {
      def rig(shp: Double, rate: Double) = 1 / Rand.nextGamma(shp,1/rate)
      val (sig2a, sig2b) = (2.0, 1.0)
      def update = {
        // update mu
        val newMu = Rand.nextGaussian(ybar,sqrt(sig2/n))

        // update sig2
        val ss = y.map{ yi => (yi-newMu)*(yi-newMu) }.sum
        val newSig2 = rig(sig2b+n/2, sig2a+ss/2)
        State(newMu, newSig2)
      }
    }

    // Time & Run Gibbs Sampler
    val out = timer {Gibbs.sample(init=State(mu=2.0,sig2=10.0),B=10000,burn=1000)}
    out.take(3).foreach{println}

    println("post mean mu: " + mean(out.map{_.mu}) )
    println("post mean sig2: " + mean(out.map{_.sig2}) )
    println("post sd mu: " + sd(out.map{_.mu}) )
    println("post sd sig2: " + sd(out.map{_.sig2}) )
  }
}
