package MCMC

package object all {

  def timer[R](block: => R): R = {  
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1E9 + "s")
    result
  }

  def mcmcHelp = {
    val fullExample = 
    """
    import breeze.stats.distributions.{Gaussian,Gamma}
    import math.sqrt
    import MCMC.all._

    // Generate Data
    val (mu,sig2,n) = (5.0,2.0,1000)
    val y = Gaussian(mu,sqrt(sig2)).sample(n).toVector
    val ybar = y.sum / n

    // Extend State class & define samplers for full conditionals
    case class State(mu: Double, sig2: Double) extends Gibbs.State {
      def rig(shp: Double, rate: Double) = 1 / Gamma(shp, 1/rate).sample
      val (sig2a, sig2b) = (2,1)
      def update = {
        // update mu
        val newMu = Gaussian(ybar,sqrt(sig2/n)).sample

        // update sig2
        val ss = y.map{ yi => (yi-newMu)*(yi-newMu) }.sum
        val newSig2 = rig(sig2b+n/2.0, sig2a+ss/2.0)
        State(newMu, newSig2)
      }
    }

    // Time & Run Gibbs Sampler
    val out = timer {Gibbs.sample(init=State(mu=2.0,sig2=10.0),B=10000,burn=1000)}

    // Post-processing
    def mean(x: List[Double]) = x.sum / x.size
    def sd(x: List[Double]) = {
      val xbar = mean(x)
      val n = x.size
      val sqDiff = x.map{xi => (xi-xbar)*(xi-xbar)}
      sqrt(mean(sqDiff)*n/(n-1))
    }

    println("post mean mu: " + mean(out.map{_.mu}) )
    println("post mean sig2: " + mean(out.map{_.sig2}) )
    println("post sd mu: " + sd(out.map{_.mu}) )
    println("post sd sig2: " + sd(out.map{_.sig2}) )

    """

    println(fullExample)
  }

}
