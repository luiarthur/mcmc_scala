import org.scalatest.FunSuite

class TestSuite extends FunSuite {
  import MCMC.all._
  import breeze.stats.distributions.Gamma

  test("Implicit") {
    println(Vector(10.0,10.1,10.2).but(1))
  }

  def round(x: Double, d: Int = 2) = (scala.math.pow(10,d) * x).toInt / scala.math.pow(10,d)


  test("Gibbs") {
    def rnorm(mu: Double, sd: Double) = scala.util.Random.nextGaussian * sd + mu
    def rig(shp: Double, rate: Double) = 1 / Gamma(shp, 1/rate).sample

    // TURTH: y ~ Normal(mu=5, var=2)
    val mu = 5.0
    val sd = math.sqrt(2.0)
    val n = 1000
    val y = Vector.fill(n)( rnorm(mu,sd) )

    val a = 2.0
    val b = 1.0

    val ybar = y.sum / n

    def samplerMu(v: Vector[Double]) = rnorm(ybar, math.sqrt(v(1)/n.toDouble) )
    def samplerSig2(v: Vector[Double]) = {
      val ss = y.map{ yi => (yi-v(0))*(yi-v(0)) }.sum
      rig(a+ n*.5, b + ss*.5)
    }

    val fullConds = Vector(FullCond(samplerMu), FullCond(samplerSig2))
    val inits = State( Vector(0.0, 1.0) )
    val gibbs = Gibbs(fullConds, inits, 10000, 1000)
    val samps = gibbs.sample

    samps.take(3).foreach{println}
    println(Console.GREEN)
    println("            " + "mu" + "\t\t" + "sig2")
    println("     Mean : " + samps.means.map{round(_)}.mkString("\t"))
    println("       SD : " + samps.sds.map{round(_)}.mkString("\t"))
    println(Console.RESET)
  }
}
