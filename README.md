# mcmc_scala
MCMC Implementation in Scala

# To do:
- better interface
- better design? Perhaps something like this:

```scala
// MCMC State
class State(s: Map[String, Double])
/*
val init = State(Map(
             "mu"   -> 0,
             "sig2" -> 1
           ))
*/

// Full Conditionals
class FullConditionals(fc: Map[String, Vector[Double]=>Double])
/*
val fcs = FullConditionals(Map(
            "mu"   -> muSampler,
            "sig2" -> sig2Sampler
          ))
*/

// Gibbs
class Gibbs(fcs: FullConditionals, init: State, B: Int, burn: Int) 
```
