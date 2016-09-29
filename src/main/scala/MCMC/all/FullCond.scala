package MCMC.all

case class FullCond (val sampler: Vector[Double] => Double) {
  def rand(otherParams: Vector[Double]) = sampler(otherParams)
}
