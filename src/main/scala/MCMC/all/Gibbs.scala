package MCMC.all

object Gibbs {
  def apply = ???
}

case class Gibbs[T](fullConds: Vector[FullCond],
                    inits: State,
                    B: Int, burn: Int) {

  //private implicit class SmartVector[T](vec: Vector[T]) {
  //  // removes nth element in vec: Vector[T]
  //  def but(n: Int) = vec patch (from = n, patch = Nil, replaced = 1)
  //}

  val d = inits.params.size

  private def update(currState: State) = {
    def loop(state: State, j: Int): State = 
      if (j==d) state else {
        val params = state.params
        val samp = fullConds(j).rand( params )
        val newState = State(params.updated(j,samp))
        loop(newState, j+1)
      }

    loop(currState,0)
  }

  def sample = {
    def loop(S: List[State], i: Int): List[State] = {
      if (i < B + burn) {
        if (i % (B/10) == 0) print("\rProgress: " + i +"/"+B)
        val newState = update(S.head) :: S
        loop(newState, i+1)
      } else { println; S }
    }
    loop(S=List(inits),i=0).dropRight(burn)
  }

}
