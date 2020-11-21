package slyce.tests.calc

import slyce.tests.Runner

object Run extends App {
  import Data._

  Runner.run(Parser, "res-test/calc/samples/ex1.txt") { rawTree =>
    println("Success:")
    println()
    println(rawTree)
    println()

    rawTree.toList.foreach {
      case NonTerminal.Line._2(assign) =>
        // TODO (KR) :
        println(1)
        println(assign)
      case NonTerminal.Line._1(expr) =>
        // TODO (KR) :
        println(2)
        println(expr)
    }
  }

}
