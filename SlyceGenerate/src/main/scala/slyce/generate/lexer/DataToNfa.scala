package slyce.generate.lexer

import slyce.common.helpers._
import scalaz.\/

import slyce.generate.architecture.{lexer => arch}

object DataToNfa extends arch.DataToNfa[Data, Err, Nfa] {

  override def apply(input: Data): Err \/ Nfa = {
    def makeMode(mode: Data.Mode): Err \/ Nfa.State =
      mode.lines
        .map(Nfa.State.fromLine)
        .traverseErrs
        .map(Nfa.State.join)

    input.modes
      .map(m => makeMode(m).map((m, _)))
      .traverseErrs
      .map { m1 =>
        {
          // DEBUG : (Start) ==================================================
          import klib.ColorString.syntax._
          import auto._
          import klib.Idt._
          import klib.Logger.GlobalLogger

          implicit val flags: Set[String] = Set("DataToNfa")

          GlobalLogger.break
          GlobalLogger.debug("=====| DataToNfa |=====")

          // DEBUG : (End) ==================================================
        }

        Nfa(
          input.startMode,
          m1.map {
            case (m2, s) =>
              m2.name -> s
          }.toMap,
        )
      }
  }

}
