package MCMC

package object all {
  implicit class SmartVector[T](vec: Vector[T]) {
    def but(i: Int) = vec patch (from = i, patch = Nil, replaced = 1)
  }

  implicit class ListState(ls: List[State]) {
    private def _mean(x: List[Double]) = x.sum / x.size.toDouble
    private def _sd(x: List[Double]) = {
      val xbar = _mean(x)
      val ss = x.map{xi => (xi-xbar)*(xi-xbar)}.sum
      math.sqrt( ss / (x.size-1.0) )
    }

    def d = ls(0).params.size
    def sep(j: Int) = ls map {s => s.params(j)}
    def mean(j: Int) = _mean( sep(j) )
    def sd(j: Int) = _sd( sep(j) )
    def means = for (i <- 0 until d) yield _mean( sep(i) )
    def sds = for (i <- 0 until d) yield _sd( sep(i) )
  }
}
