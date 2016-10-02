package MCMC.all

object Spec3 {
  abstract class GibbsState { def update: GibbsState }

  def gibbs[T <: GibbsState](init: T, B: Int, burn: Int) = {
    def loop(S: List[T], i: Int): List[T] = {
      if (i < B + burn) {
        if (i % (B/10) == 0) print("\rProgress: " + i +"/"+B)
        val newState = S.head.update.asInstanceOf[T] :: S
        loop(newState, i+1)
      } else {println; S}
    }
    loop(List(init),0)
  }

}
