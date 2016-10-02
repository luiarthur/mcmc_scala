# mcmc_scala [![Build Status](https://travis-ci.org/luiarthur/mcmc_scala.svg?branch=master)](https://travis-ci.org/luiarthur/mcmc_scala)
MCMC Implementation in Scala

# To do:
- better interface
- make parameters `Either[Double,Vector[Double]]`
- better design?

# Usage Example:
```scala
import breeze.stats.distributions.{Gaussian,Gamma}
import math.sqrt

import MCMC.all._
val (mu,sig2,n) = (5.0,2.0,1000)
val y = Gaussian(mu,sqrt(sig2)).sample(n).toVector

val ybar = y.sum / n

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

val init = State(2.0,10.0)
val out = timer { Gibbs.sample(init,10000,1000) }

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
```
