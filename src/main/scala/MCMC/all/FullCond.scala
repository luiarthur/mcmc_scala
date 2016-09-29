package MCMC.all

case class FullCond (val sampler: Vector[Double] => Double) {
  def rand(params: Vector[Double]) = sampler(params)
}
