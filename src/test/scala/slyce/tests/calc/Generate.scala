package slyce.tests.calc

import scalaz.-\/
import scalaz.Scalaz.ToOptionIdOps
import scalaz.\/
import scalaz.\/-

import slyce.implementations.generation.{lexer => lex}

object Generate extends App {

  val data: lex.Data = {
    import lex._
    import Regex.CharClass._

    Data(
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
                  Inclusive('/'),
                  2,
                  2.some
                ),
                Regex.Repeat * Exclusive('\n'),
                Inclusive('\n')
              ),
              yields = Yields(
                yields = Nil,
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
                yields = Nil,
                toMode = "MultiLineComment".some
              )
            ),
            //: [ \t]
            Data.Mode.Line(
              lineNo = 7,
              regex = Inclusive(' ', '\t'),
              yields = Yields(
                yields = Nil,
                toMode = None
              )
            ),
            //: [=()]
            Data.Mode.Line(
              lineNo = 8,
              regex = Inclusive('=', '(', ')', '\n'),
              yields = Yields(
                yields = List(Yields.Yield.Text.std),
                toMode = None
              )
            ),
            //: [+\-]
            Data.Mode.Line(
              lineNo = 9,
              regex = Inclusive('+', '-'),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("addOp")),
                toMode = None
              )
            ),
            //: [*/]
            Data.Mode.Line(
              lineNo = 10,
              regex = Inclusive('*', '/'),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("multOp")),
                toMode = None
              )
            ),
            //: ^
            Data.Mode.Line(
              lineNo = 11,
              regex = Inclusive('^'),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("powOp")),
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
                yields = List(Yields.Yield.Terminal.std("int")),
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
                yields = List(Yields.Yield.Terminal.std("float")),
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
                yields = List(Yields.Yield.Terminal("_var", (0, -1))),
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
                yields = Nil,
                toMode = "General".some
              )
            ),
            //: .
            Data.Mode.Line(
              lineNo = 18,
              regex = Exclusive(),
              yields = Yields(
                yields = Nil,
                toMode = None
              )
            )
          )
        )
      )
    )
  }

  val lexer: lex.Err \/ lex.Dfa = lex.Lexer(data)

  lexer match {
    case -\/(errs) =>
      println("Errors:")
      errs.foreach(println)
      System.exit(1)
    case \/-(dfa) =>
      implicit val idt: String = "  "

      println("Success:")
      println()
      println(dfa.toksStr.mkString("\n"))
      println()
      println(dfa.dfaStr.mkString("\n"))
  }

}
