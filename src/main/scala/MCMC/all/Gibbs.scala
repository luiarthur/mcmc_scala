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
    loop(List(init),0).asInstanceOf[List[T]]
  }

}
