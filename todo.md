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

def f(p: Param) = p match {
  case Scalar(x) => x
  case Vec(x) => x(0)
  case Mat(x) => x(0,0)
}

f(Scalar(10.0))
f(Vec(DenseVector(1,2,3)))

```

