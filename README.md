# mcmc_scala [![Build Status](https://travis-ci.org/luiarthur/mcmc_scala.svg?branch=master)](https://travis-ci.org/luiarthur/mcmc_scala)
MCMC Implementation in Scala

# To do:
- better interface
- make parameters `Either[Double,Vector[Double]]`
- better design?

# Usage Example:
```scala
import MCMC.all._
import breeze.stats.distributions.Gamma

def round(x: Double, d: Int = 2) = (scala.math.pow(10,d) * x).toInt / scala.math.pow(10,d)
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

val gibbs = Gibbs(specs, 10000, 1000)
val samps = gibbs.sample
val postMean = samps.meansDouble
val postSD = samps.sdsDouble

samps.take(3).foreach{println}
println(Console.GREEN)
println("        " + "mu" + "\t" + "sig2")
println("Truth : " + mu + "\t" + sig2)
println(" Mean : " + postMean.map{round(_)}.mkString("\t"))
println("   SD : " + postSD.map{round(_)}.mkString("\t"))
println(Console.RESET)

```
