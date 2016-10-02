import org.scalatest.FunSuite

class TestSuite extends FunSuite {
  import MCMC.all.timer

  test("Implicit") {
    import MCMC.all._
    println(Vector(10.0,10.1,10.2).but(1))
  }

  def round(x: Double, d: Int = 2) = (scala.math.pow(10,d) * x).toInt / scala.math.pow(10,d)


  test("Gibbs") {
    import MCMC.all._
    import breeze.stats.distributions.Gamma
    def rnorm(mu: Double, sd: Double) = scala.util.Random.nextGaussian * sd + mu
    def rig(shp: Double, rate: Double) = 1 / Gamma(shp, 1/rate).sample

    // TURTH: y ~ Normal(mu=5, var=2)
    val mu = 5.0
    val sig2 = 2.0
    val n = 1000
    val y = Vector.fill(n)( rnorm(mu,math.sqrt(sig2)) )

    val a = 2.0
    val b = 1.0

    val ybar = y.sum / n

    def samplerMu(s: State) = {
      val s2 = s.s("sig2").head
      Vector(rnorm(ybar, math.sqrt(s2/n.toDouble)))
    }

    def samplerSig2(s: State) = {
      val m = s.s("mu").head
      val ss = y.map{ yi => (yi-m)*(yi-m) }.sum
      Vector(rig(a+ n*.5, b + ss*.5))
    }

    val specs = new Specifications(
      init = State(Map(
               "mu"   -> Vector(0.0),
               "sig2" -> Vector(1.0)
             )),
      fcs = FullConditionals(Map(
              "mu"   -> samplerMu,
              "sig2" -> samplerSig2
            ))
    )

    val gibbs = Gibbs(specs, 100000, 1000)
    val samps = timer { gibbs.sample }
    val postMean = samps.meansDouble
    val postSD = samps.sdsDouble

    samps.take(3).foreach{println}
    println(Console.GREEN)
    println("        " + "mu" + "\t" + "sig2")
    println("Truth : " + mu + "\t" + sig2)
    println(" Mean : " + postMean.map{round(_)}.mkString("\t"))
    println("   SD : " + postSD.map{round(_)}.mkString("\t"))
    println(Console.RESET)
  }

  test("Spec2-XML") {
    import MCMC.all._
    println(Spec2.specs)
  }

  test("Spec3") {
    // 3 times faster than julia
    import breeze.stats.distributions.{Gaussian,Gamma}
    import math.sqrt
    import MCMC.all.Spec3._

    val mu = 5.0
    val sig2 = 2.0
    val n = 100
    def rnorm(mu: Double, sd: Double) = scala.util.Random.nextGaussian * sd + mu
    def rig(shp: Double, rate: Double) = 1 / Gamma(shp, 1/rate).sample

    val y = Gaussian(mu,sqrt(sig2)).sample(n).toVector

    val a = 2.0
    val b = 1.0

    val ybar = y.sum / n

    case class State(mu: Double, sig2: Double) extends GibbsState {
      def update = {
        // update mu
        val newMu = Gaussian(ybar,sqrt(sig2/n)).sample
        // update sig2
        val ss = y.map{ yi => (yi-newMu)*(yi-newMu) }.sum
        val newSig2 = rig(a+n/2.0, b+ss/2.0)
        State(newMu, newSig2)
      }
    }

    val init = State(2.0,1.0)
    val out = timer { gibbs(init,100000,1000) }
    println(out.head)
    println(out.map{_.mu}.sum / out.size)
    println(out.map{_.sig2}.sum / out.size)


    //println(out.head.mu)
  }
}
