package slyce.metaSelf

import slyce.Runner

object Run extends App {

  Runner.run(GrammarParser, "SlyceGenerate/src/main/slyce/grammar.sgf") { rawTree =>
    import slyce.metaSelf.Data._

    import klib.ColorString.syntax._
    import auto._
    import klib.Idt._
    import klib.Logger.GlobalLogger

    implicit val flags: Set[String] = Set("Runner")

    rawTree match {
      case NonTerminal.Grammar._1(_, _, startNt, _, nts, _) =>
        GlobalLogger.print(s"@start ${startNt.text}")

        nts.toList.foreach {
          case NonTerminal.Nt._1(_1, _2) =>
            GlobalLogger.break
            GlobalLogger.info(_1.text)
            _2 match {
              case NonTerminal.NtBase._1(_1) =>
                _1 match {
                  case NonTerminal.StdNtBase._1(_1, _2) =>
                    GlobalLogger.detailed(_1)
                    _2.toList.foreach { rl =>
                      GlobalLogger.detailed(
                        Indented(
                          ">",
                          Indented(
                            rl.toList.map(_.toString),
                          ),
                        ),
                      )
                    }
                  // TODO (KR) :
                }
              case NonTerminal.NtBase._2(_1) =>
                _1 match {
                  case NonTerminal.ListNtBase._1(_1, _2) =>
                    GlobalLogger.detailed(_1)
                  // TODO (KR) :
                }
              case NonTerminal.NtBase._3(_1) =>
                _1 match {
                  case NonTerminal.AssocNtBase._1(_1, _2, _3) =>
                    GlobalLogger.detailed(_1)
                  // TODO (KR) :
                }
            }
        }
    }

  }

}
