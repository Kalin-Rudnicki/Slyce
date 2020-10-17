package slyce.tests

import scala.annotation.tailrec

import scalaz.-\/
import scalaz.Scalaz.ToOptionIdOps
import scalaz.\/
import scalaz.\/-

import slyce.implementations.generation.lexer._
import Regex.CharClass._

object Test extends App {

  val data: Data = Data(
    startMode = "General",
    modes = List(
      // =====| General |=====
      Data.Mode(
        name = "General",
        lines = List(
          //: //[^\n]*\n
          Data.Mode.Line(
            lineNo = 5,
            regex = Regex.Sequence(
              Regex.Repeat(
                Inclusive('\\'),
                2,
                2.some
              ),
              Regex.Repeat * Exclusive('\n'),
              Inclusive('\n')
            ),
            yields = Yields(
              yields = None,
              toMode = None
            )
          ),
          //: /\*
          Data.Mode.Line(
            lineNo = 6,
            regex = Regex.Sequence(
              Inclusive('/'),
              Inclusive('*')
            ),
            yields = Yields(
              yields = None,
              toMode = "MultiLineComment".some
            )
          ),
          //: [ \t]
          Data.Mode.Line(
            lineNo = 7,
            regex = Inclusive(' ', '\t'),
            yields = Yields(
              yields = None,
              toMode = None
            )
          ),
          //: [=()]
          Data.Mode.Line(
            lineNo = 8,
            regex = Inclusive('=', '(', ')'),
            yields = Yields(
              yields = Yields.Yield.Text.std.some,
              toMode = None
            )
          ),
          //: [+\-]
          Data.Mode.Line(
            lineNo = 9,
            regex = Inclusive('+', '-'),
            yields = Yields(
              yields = Yields.Yield.Terminal.std("addOp").some,
              toMode = None
            )
          ),
          //: [*/]
          Data.Mode.Line(
            lineNo = 10,
            regex = Inclusive('*', '/'),
            yields = Yields(
              yields = Yields.Yield.Terminal.std("multOp").some,
              toMode = None
            )
          ),
          //: ^
          Data.Mode.Line(
            lineNo = 11,
            regex = Inclusive('^'),
            yields = Yields(
              yields = Yields.Yield.Terminal.std("powOp").some,
              toMode = None
            )
          ),
          //: -?\d+
          Data.Mode.Line(
            lineNo = 12,
            regex = Regex.Sequence(
              Regex.Repeat ? Inclusive('-'),
              Regex.Repeat + Inclusive.d
            ),
            yields = Yields(
              yields = Yields.Yield.Terminal.std("int").some,
              toMode = None
            )
          ),
          //: -?\d+\.\d+
          Data.Mode.Line(
            lineNo = 13,
            regex = Regex.Sequence(
              Regex.Repeat ? Inclusive('-'),
              Regex.Repeat + Inclusive.d,
              Inclusive('.'),
              Regex.Repeat + Inclusive.d
            ),
            yields = Yields(
              yields = Yields.Yield.Terminal.std("float").some,
              toMode = None
            )
          ),
          //: [a-z][_a-zA-Z\d]*
          Data.Mode.Line(
            lineNo = 14,
            regex = Regex.Sequence(
              Inclusive('_') | Inclusive.az,
              Regex.Repeat * (
                Inclusive('_') |
                  Inclusive.az |
                  Inclusive.AZ |
                  Inclusive.d
              )
            ),
            yields = Yields(
              yields = Yields.Yield.Terminal.std("var").some,
              toMode = None
            )
          )
        )
      ),
      // =====| MultiLineComment |=====
      Data.Mode(
        name = "MultiLineComment",
        lines = List(
          //: \*/
          Data.Mode.Line(
            lineNo = 17,
            regex = Regex.Sequence(
              Inclusive('*'),
              Inclusive('/')
            ),
            yields = Yields(
              yields = None,
              toMode = "General".some
            )
          ),
          //: .
          Data.Mode.Line(
            lineNo = 18,
            regex = Exclusive(),
            yields = Yields(
              yields = None,
              toMode = None
            )
          )
        )
      )
    )
  )

  val dfa: Err \/ Dfa = Lexer(data)

  dfa match {
    case -\/(errs) =>
      println("Errors:")
      errs.foreach(println)
      System.exit(1)
    case \/-(dfa) =>
      @tailrec
      def findAllStates(unseen: Set[Dfa.State], seen: Set[Dfa.State] = Set()): Set[Dfa.State] =
        if (unseen.isEmpty)
          seen
        else {
          val nowSeen = seen | unseen
          def references(state: Dfa.State): Set[Dfa.State] =
            Set(
              state.transitions.toList.flatMap(_._2).toSet,
              state.elseTransition.toSet,
              state.yields.map(_.toMode).toSet
            ).flatten

          findAllStates(unseen.flatMap(references) &~ nowSeen, nowSeen)
        }

      val stateMap =
        findAllStates(Set(dfa.initialState)).toList.zipWithIndex.toMap

      println("Success:")
      println
      println(s"initial-state: ${stateMap(dfa.initialState)}, ${stateMap.size}")
      println
      stateMap.toList.sortBy(_._2).foreach {
        case (state, idx) =>
          println(s"$idx:")
          println("\ttransitions:")
          state.transitions.foreach {
            case (chars, to) =>
              println(s"\t\t$chars => ${to.fold("")(stateMap(_).toString)}")
          }
          println("\telse-transition:")
          state.elseTransition.foreach { to =>
            println(s"\t\t$to")
          }
          println("\tyields:")
          state.yields.foreach { yields =>
            println(s"\t\tyields: ${yields.yields}")
            println(s"\t\tto: ${stateMap(yields.toMode)}")
          }
      }
  }

}
