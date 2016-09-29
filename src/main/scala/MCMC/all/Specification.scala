package MCMC.all

// Model Specification
class Specifications(val init: State, val fcs: FullConditionals) {
  def size = init.s.size
}

// Example:
// val specs = new Specification(
//   init = State(Map(
//            "mu"   -> Left(0.0),
//            "sig2" -> Left(1.0)
//          )),
//   fcs = FullConditionals(Map(
//           "mu"   -> muSampler: State=>Param,
//           "sig2" -> sig2Sampler: State=>Param
//         ))
// )
