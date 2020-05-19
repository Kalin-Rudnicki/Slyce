package slyce

object Test {

  class State(val id: Int) {

    var transitions: Map[Char, State] = _

    override def toString: String =
      s"State(${transitions.map(pair => s"${pair._1} -> State #${pair._2.id}").mkString(", ")})"

  }

  def main(args: Array[String]): Unit = {

    val s1: State = new State(0)
    val s2: State = new State(1)
    val s3: State = new State(2)

    s1.transitions = Map('a' -> s1, 'b' -> s2)
    s2.transitions = Map('b' -> s2, 'c' -> s3)
    s3.transitions = Map('a' -> s1, 'c' -> s3)

    println(s1)
    println(s2)
    println(s3)

  }

}
