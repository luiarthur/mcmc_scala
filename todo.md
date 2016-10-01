# To Try:

```scala
import breeze.linalg.{DenseVector,DenseMatrix}



trait Param
case class Scalar(x: Double) extends Param
case class Vec(x: DenseVector[Double]) extends Param
case class Mat(x: DenseMatrix[Double]) extends Param
case class ScalarInt(x: Int) extends Param
case class VecInt(x: DenseVector[Int]) extends Param
case class MatInt(x: DenseMatrix[Int]) extends Param

case class Scalar[T](x: Numeric[T]) extends Param
case class Vec[+T](x: DenseVector[T]) extends Param
case class Mat[+T](x: DenseMatrix[T]) extends Param


def f(p: Param) = p match {
  case Scalar(x) => x
  case Vec(x) => x(0)
  case Mat(x) => x(0,0)
}

f(Scalar(10.0))
f(Vec(DenseVector(1,2,3)))


:paste
trait  Param[T]
object Param {
  implicit object IntOk extends Param[Int]
  implicit object DoubleOk extends Param[Long]
}

def f[T](t: Param[T]) = t + 1

```
