package MCMC.all

object Gibbs {
  abstract class State{ def update: State }

  def sample[T <: State](init: T, B: Int, burn: Int, printEvery: Int = 10) = {
    def loop(S: List[T], i: Int): List[T] = {
      if (i < B + burn) {
        if (i % (B/printEvery) == 0) print("\rProgress: " + i +"/"+ (B+burn))
        val newState = S.head.update.asInstanceOf[T] :: S
        loop(newState, i+1)
      } else {
        print("\rProgress: " + i + "/" + (B + burn) + "\n")
        S
      }
    }
    loop(List(init),0).take(B)
    //
    //Non-recursive (preallocates memory)
    //while-loops avoid creating objects (whereas for loops do not)
    //val out = Array.fill[State](B+burn)(init)
    //var i = 1
    //while (i < B+burn) {
    //  if (i % ((B+burn)/10) == 0) print("\rProgress: " + i +"/"+ (B+burn))
    //  out(i) = out(i-1).update
    //  i += 1
    //}
    //print("\rProgress: " + i + "/" + (B + burn) + "\n")
    //out.drop(burn).asInstanceOf[Array[T]].toList
  }

}
