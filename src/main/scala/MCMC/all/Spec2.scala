package MCMC.all

object Spec2 {
  import breeze.stats.distributions._

  val n = 100
  val y = Vector.fill(n)(scala.util.Random.nextGaussian)

  val specs =
<specs>
    <precomputes>
        ybar = {y.sum / y.size}
    </precomputes>
    
    <init>
        mu: Double = 10.0
        sig2: Double = 1.0
    </init>

    <fullcond>
        mu | sig2, y ~ Normal(ybar, sig2/n)
        sig2 | mu, y ~ IG(a,b)
    </fullcond>
</specs>

  def parse(spec: xml.Elem) = ???

}
