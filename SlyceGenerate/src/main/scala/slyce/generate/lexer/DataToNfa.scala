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
        .map(Nfa.State.collect)

    input.modes
      .map(m => makeMode(m).map((m, _)))
      .traverseErrs
      .map { m1 =>
        val nfa = Nfa(
          input.startMode,
          m1.map {
            case (m2, s) =>
              m2.name -> s
          }.toMap,
        )

        {
          // DEBUG : (Start) ==================================================
          import klib.ColorString.syntax._
          import auto._
          import klib.Idt._
          import klib.Logger.GlobalLogger

          implicit val flags: Set[String] = Set("DataToNfa")

          GlobalLogger.break
          GlobalLogger.debug("=====| DataToNfa |=====")
          val allStates = nfa.modes.toSet
            .flatMap { (t: (String, Nfa.State)) =>
              t._2.findAll
            }
            .toList
            .zipWithIndex
            .toMap
          GlobalLogger.debug(
            Group(
              "modes:",
              Indented(
                nfa.modes.toList.map {
                  case m -> s =>
                    s"$m => #${allStates(s)}"
                },
              ),
              "states:",
              Indented(
                allStates.toList.sortBy(_._2).map {
                  case s -> i =>
                    Group(
                      s"#$i =>",
                      Indented(
                        s"isTrivial => ${s.isTrivial}",
                        "transitions =>",
                        Indented(
                          s.transitions.map {
                            case c -> t =>
                              s"#${allStates(t)}: $c"
                          },
                        ),
                        "epsilonTransitions =>",
                        Indented(
                          s.epsilonTransitions.map { e =>
                            s"#${allStates(e)}"
                          },
                        ),
                        "end =>",
                        Indented(
                          s.`end`.map(_.toString),
                        ),
                      ),
                    )
                },
              ),
            ),
          )

          // DEBUG : (End) ==================================================
        }

        nfa
      }
  }

}
