package MCMC.all

// Under development...

abstract class Strange[T]
object Strange {
  implicit object IntOk extends Strange[Int]
  implicit object DoubleOk extends Strange[Double]
}
/*
  def f[T: Param](t: T) = t + 1 // doesn't work...
 */
