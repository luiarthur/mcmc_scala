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


  test("Gibbs") { // julia is 2-3 times faster than scala
    import breeze.stats.distributions.{Gaussian,Gamma}
    import MCMC.all._

    // Generate Data
    val (mu,sig2,n) = (5.0,2.0,1000)
    val y = Gaussian(mu,sqrt(sig2)).sample(n)
    val ybar = y.sum / n

    // Extend State class & define samplers for full conditionals
    case class State(mu: Double, sig2: Double) extends Gibbs.State {
      def rig(shp: Double, rate: Double) = 1 / Gamma(shp, 1/rate).sample
      val (sig2a, sig2b) = (2.0, 1.0)
      def update = {
        // update mu
        val newMu = Gaussian(ybar,sqrt(sig2/n)).sample

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

  test("aft") {
    val XTV = scala.io.Source.fromFile("src/test/resources/tongue.dat").getLines.map(x=>x.split(",").toList.map(_.toDouble)).toList
    val X = XTV.map(xtv => List(1.0,xtv(0)))
    val t = XTV.map(xtv => xtv(1))
    val v = XTV.map(xtv => xtv(2).toInt)

    import MCMC.all._
    val B = 10000
    val burn = 5000

    val prior = new AFT.Prior(m = List(0,0), s2 = List(10,10), csb = List(1,1),
                              a = 2.0, b = 1.0, css = 1)
    val aft = new AFT(t,X,v,prior)

    val (weib,wdic) = timer{aft.sample(B,burn)}
    weib.take(2).foreach{println}
    println(Console.GREEN+"weib: "+weib.map(_.sig).sum/B+Console.RESET)
    println(Console.GREEN+"weib: "+sd(weib.map(_.sig))+Console.RESET)
    println(Console.GREEN+"weib: "+weib.map(_.beta(0)).sum/B+Console.RESET)
    println(Console.GREEN+"weib: "+weib.map(_.beta(1)).sum/B+Console.RESET)
    println(Console.GREEN+"weib: "+sd(weib.map(_.beta(0)))+Console.RESET)
    println(Console.GREEN+"weib: "+sd(weib.map(_.beta(1)))+Console.RESET)
    println(Console.GREEN+"weib DIC: "+wdic+Console.RESET)

    //val (llog,lldic) = timer{aft.sample(B,burn,Distr.LogLogistic)}
    //println(Console.GREEN+"llog: "+llog.map(_.sig).sum/B+Console.RESET)
    //println(Console.GREEN+"llog: "+sd(llog.map(_.sig))+Console.RESET)
    //println(Console.GREEN+"llog: "+llog.map(_.beta(0)).sum/B+Console.RESET)
    //println(Console.GREEN+"llog: "+llog.map(_.beta(1)).sum/B+Console.RESET)
    //println(Console.GREEN+"llog DIC: "+lldic+Console.RESET)

    //val (lnorm,lndic) = timer{aft.sample(B,burn,Distr.LogNormal)}
    //println(Console.GREEN+"lnorm: "+lnorm.map(_.sig).sum/B+Console.RESET)
    //println(Console.GREEN+"lnorm: "+sd(lnorm.map(_.sig))+Console.RESET)
    //println(Console.GREEN+"lnorm: "+lnorm.map(_.beta(0)).sum/B+Console.RESET)
    //println(Console.GREEN+"lnorm: "+lnorm.map(_.beta(1)).sum/B+Console.RESET)
    //println(Console.GREEN+"lnorm DIC: "+lndic+Console.RESET)
  }
}
