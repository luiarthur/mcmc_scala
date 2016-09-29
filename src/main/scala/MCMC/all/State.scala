package MCMC.all

case class State(s: Map[String, Param]) {

  override def toString = {
    def str(v: Param) = v.size match {
      case 1 => v.head
      case _ => v
    }

    val l = s.toList
    val out = l map { pv => pv._1 + ": " + str(pv._2) }
    out.mkString(";\t")
  }

}
