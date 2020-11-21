package slyce.tests.argList

import slyce.tests.Runner

object Run extends App {
  import Data._

  Runner.run(Parser, "res-test/argList/samples/ex1.txt") { rawTree =>
    println("Success:")
    println()
    println(rawTree)
    println()

  // TODO (KR) :

  }

}
