package slyce.metaSelf

import slyce.Runner
import slyce.metaSelf.Data.NonTerminal.Grammar

object Run extends App {

  Runner.run(GrammarParser, "SlyceGenerate/src/main/slyce/grammar.sgf") { rawTree =>
    import klib.ColorString.syntax._
    import auto._
    import klib.Idt._
    import klib.Logger.GlobalLogger

    implicit val flags: Set[String] = Set("Runner")

    rawTree match {
      case Grammar._1(_, _, startNt, _, nts, _) =>
        GlobalLogger.print(s"@start ${startNt.text}")
    }

  }

}
