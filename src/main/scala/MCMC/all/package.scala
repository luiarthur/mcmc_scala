package MCMC

package object all {

  type Param = Vector[Double]

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

    def params = ls(0).s.keys
    def sepDouble(s: String) = ls map {st => st.s(s).head}
    def meanDouble(s: String) = _mean( sepDouble(s) )
    def sdDouble(s: String) = _sd( sepDouble(s) )
    def meansDouble = params map { p => _mean(sepDouble(p)) }
    def sdsDouble = params map { p => _sd(sepDouble(p)) }
  }

}
