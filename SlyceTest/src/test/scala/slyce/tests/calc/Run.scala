package slyce.tests.calc

import slyce.tests.Runner

object Run extends App {
  import Data._

  Runner.run(Parser, "res-test/calc/samples/ex1.txt") { rawTree =>
    println("Success:")
    println()
    println(rawTree)
    println()

    def loop(
        unseen: NonTerminal.Lines_2,
        seen: List[NonTerminal.Line],
    ): List[NonTerminal.Line] =
      unseen match {
        case NonTerminal.Lines_2._1(_, line, _, tail) =>
          loop(
            tail,
            line :: seen,
          )
        case NonTerminal.Lines_2._2 =>
          seen.reverse
      }

    val lines: List[NonTerminal.Line] =
      rawTree match {
        case NonTerminal.Lines._1(_, line, tail) =>
          loop(
            tail,
            line :: Nil,
          )
        case NonTerminal.Lines._2 =>
          Nil
      }

    lines.foreach {
      case NonTerminal.Line._2(assign) =>
        ???
      case NonTerminal.Line._1(expr) =>
        ???
    }
  }

}
