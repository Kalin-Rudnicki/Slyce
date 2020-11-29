package slyce.tests.calc

import scalaz.Scalaz.ToOptionIdOps
import scalaz.NonEmptyList

import slyce.generate.{lexer => lex}
import slyce.generate.{grammar => gram}
import slyce.Generator

object Generate extends App {

  val lexerData: lex.Data = {
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
                  2.some,
                ),
                Regex.Repeat * Exclusive('\n'),
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("comment")),
                toMode = None,
              ),
            ),
            //: /\*
            Data.Mode.Line(
              lineNo = 6,
              regex = Regex.Sequence(
                Inclusive('/'),
                Inclusive('*'),
                Regex.Repeat * Regex.Group(
                  Regex.Sequence(
                    Exclusive('*'),
                  ),
                  Regex.Sequence(
                    Inclusive('*'),
                    Exclusive('/'),
                  ),
                ),
                Inclusive('*'),
                Inclusive('/'),
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("comment")),
                toMode = None,
              ),
            ),
            //: [ \t]
            Data.Mode.Line(
              lineNo = 7,
              regex = Inclusive(' ', '\t'),
              yields = Yields(
                yields = Nil,
                toMode = None,
              ),
            ),
            //: [=()]
            Data.Mode.Line(
              lineNo = 8,
              regex = Inclusive('=', '(', ')', '\n'),
              yields = Yields(
                yields = List(Yields.Yield.Text.std),
                toMode = None,
              ),
            ),
            //: [+\-]
            Data.Mode.Line(
              lineNo = 9,
              regex = Inclusive('+', '-'),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("addOp")),
                toMode = None,
              ),
            ),
            //: [*/]
            Data.Mode.Line(
              lineNo = 10,
              regex = Inclusive('*', '/'),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("multOp")),
                toMode = None,
              ),
            ),
            //: ^
            Data.Mode.Line(
              lineNo = 11,
              regex = Inclusive('^'),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("powOp")),
                toMode = None,
              ),
            ),
            //: -?\d+
            Data.Mode.Line(
              lineNo = 12,
              regex = Regex.Sequence(
                Regex.Repeat ? Inclusive('-'),
                Regex.Repeat + Inclusive.d,
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("int")),
                toMode = None,
              ),
            ),
            //: -?\d+\.\d+
            Data.Mode.Line(
              lineNo = 13,
              regex = Regex.Sequence(
                Regex.Repeat ? Inclusive('-'),
                Regex.Repeat + Inclusive.d,
                Inclusive('.'),
                Regex.Repeat + Inclusive.d,
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("float")),
                toMode = None,
              ),
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
                ),
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal("_var", (0, -1))),
                toMode = None,
              ),
            ),
          ),
        ),
      ),
    )
  }

  val grammarData: gram.Data = {
    import gram._
    import Data.{Identifier => Id}
    import Data.{NonTerminal => NT}
    import Data.NT._

    val anyNl: Data.Element =
      ListNT.*(
        before = IgnoredList()(
          Id.raw("\n"),
        )(),
        after = None,
      )
    val someNl: Data.Element =
      ListNT.+(
        before = IgnoredList()(
          Id.raw("\n"),
        )(),
        after = None,
      )

    Data(
      startNT = "Lines",
      nts = List(
        // Lines
        NT(
          name = "Lines",
          nt = StandardNT.^(
            IgnoredList(
              anyNl,
            )(
              ListNT.*(
                before = IgnoredList()(
                  Id("Line"),
                )(),
                after = IgnoredList(
                  someNl,
                )(
                  Id("Line"),
                )().some,
              ),
            )(
              anyNl,
            ),
          ),
        ),
        // Line
        NT(
          name = "Line",
          nt = StandardNT.^(
            // 1
            IgnoredList()(
              Id("Expr"),
            )(),
            // 2
            IgnoredList()(
              Id("Assign"),
            )(),
            // 3
            IgnoredList()(
              Id("comment"),
            )(),
          ),
        ),
        // Assign
        NT(
          name = "Assign",
          nt = StandardNT.`:`(
            // 1
            List(
              true -> Id("_var"),
              true -> Id.raw("="),
              true -> Id("Expr"),
            ),
          ),
        ),
        // Expr
        NT(
          name = "Expr",
          nt = AssocNT(
            assocElements = NonEmptyList(
              AssocNT.AssocElement.>(Id("powOp")),
              AssocNT.AssocElement.<(Id("multOp")),
              AssocNT.AssocElement.<(Id("addOp")),
            ),
            base = StandardNT.^(
              // 1
              IgnoredList(
                Id.raw("("),
              )(
                Id("Expr"),
              )(
                Id.raw(")"),
              ),
              // 2
              IgnoredList()(
                Id("int"),
              )(),
              // 3
              IgnoredList()(
                Id("float"),
              )(),
              // 4
              IgnoredList()(
                Id("_var"),
              )(),
            ),
          ),
        ),
      ),
    )
  }

  Generator.generate(
    lexerData,
    grammarData,
    "calc",
  )

}
