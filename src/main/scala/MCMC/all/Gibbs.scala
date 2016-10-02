package MCMC.all

object Gibbs {
  abstract class State{ def update: State }

  def sample[T <: State](init: T, B: Int, burn: Int) = {
    def loop(S: List[State], i: Int): List[State] = {
      if (i < B + burn) {
        if (i % ((B+burn)/10) == 0) print("\rProgress: " + i +"/"+ (B+burn))
        val newState = S.head.update :: S
        loop(newState, i+1)
      } else {
        print("\rProgress: " + i + "/" + (B + burn) + "\n")
        S
      }
    }
    loop(List(init),0).take(B).asInstanceOf[List[T]]
    /* Non-recursive (preallocate memory)
     * while-loops avoid creating objects (whereas for loops do not)
    val out = Array.fill[State](B+burn)(init)
    var i = 1
    while (i < B+burn) {
      if (i % ((B+burn)/10) == 0) print("\rProgress: " + i +"/"+ (B+burn))
      out(i) = out(i-1).update
      i += 1
    }
    print("\rProgress: " + i + "/" + (B + burn) + "\n")
    out.drop(burn).asInstanceOf[Array[T]].toList
    */
  }

}
