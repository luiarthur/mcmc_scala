package MCMC.all

object Gibbs {
  def apply = ???
}

case class Gibbs(specs: Specifications, B: Int, burn: Int) {

  private val _fcs = specs.fcs
  private val _params = specs.init.s.keys.toList
  val d = specs.size

  private def _update(currState: State) = {
    def loop(state: State, p: List[String]): State = 
      if (p.size==0) state else {
        val samp = _fcs.fcs(p.head)(state)
        val newState = State( state.s.updated(p.head,samp) )
        loop(newState, p.tail)
      }

    loop(currState,_params)
  }

  def sample = {
    def loop(S: List[State], i: Int): List[State] = {
      if (i < B + burn) {
        if (i % (B/10) == 0) print("\rProgress: " + i +"/"+B)
        val newState = _update(S.head) :: S
        loop(newState, i+1)
      } else { println; S }
    }
    loop(S=List(specs.init),i=0).dropRight(burn)
  }

}
