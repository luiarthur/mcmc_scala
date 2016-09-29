# mcmc_scala
MCMC Implementation in Scala

# To do:
- better interface
- make parameters `Either[Double,Vector[Double]]`
- better design? Perhaps something like this:

```scala
// Can be placed in package object
type Param = Either[Double,Vector[Double]] // or just Double for only univariates

// MCMC State
case class State(s: Map[String, Param])

// Full Conditionals
case class FullConditionals(fcs: Map[String, State=>Param])

// Model Specification
class Specifications(init: State, fcs: FullConditionals)
/*
val specs = new Specification(
  init = State(Map(
           "mu"   -> Left(0.0),
           "sig2" -> Left(1.0)
         )),
  fcs = FullConditionals(Map(
          "mu"   -> muSampler: State=>Param,
          "sig2" -> sig2Sampler: State=>Param
        ))
)
*/

// Gibbs
class Gibbs(specs: Specifications, B: Int, burn: Int) {

  private val _fcs = specs.fcs
  val d = specs.init.size

  private def update(currState: State) = {
    def loop(state: State, j: Int): State = 
      if (j==d) state else {
        val samp = _fcs(p(j))(state)
        val newState = State( state.updated(p(j),samp) )
        loop(newState, j+1)
      }

    loop(currState,0)
  }
}
```
