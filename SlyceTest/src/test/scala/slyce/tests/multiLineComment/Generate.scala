package slyce.tests.multiLineComment

import scalaz.Scalaz.ToOptionIdOps

import slyce.generate.{lexer => lex}
import slyce.generate.{grammar => gram}
import slyce.Generator

object Generate extends App {

  val lexerData: slyce.generate.lexer.Data = {
    import lex._
    import Regex.CharClass._

    Data(
      startMode = "General",
      modes = List(
        // =====| General |=====
        Data.Mode(
          name = "General",
          lines = List(
            Data.Mode.Line(
              lineNo = 1,
              regex = Inclusive(' ', '\t', '\n'),
              yields = Yields(
                yields = Nil,
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 2,
              regex = Regex.Sequence(
                Regex.Repeat(
                  Inclusive('/'),
                  2,
                  2.some,
                ),
                Exclusive('\n').anyAmount,
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("comment")),
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 3,
              regex = Regex.Sequence(
                Inclusive('/'),
                Inclusive('*'),
                Regex
                  .Group(
                    Regex.Sequence(
                      Exclusive('*'),
                    ),
                    Regex.Sequence(
                      Inclusive('*'),
                      Exclusive('/'),
                    ),
                  )
                  .anyAmount,
                Inclusive('*'),
                Inclusive('/'),
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("multiLineComment")),
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
    import Data.{Optional => Opt}
    import Data.NT._

    Data(
      startNT = "Comments",
      nts = List(
        NT(
          name = "Comments",
          nt = ListNT.*(
            before = IgnoredList()(
              Id("Comment"),
            )(),
            after = None,
          ),
        ),
        NT(
          name = "Comment",
          nt = StandardNT.^(
            IgnoredList()(
              Id("comment"),
            )(),
            IgnoredList()(
              Id("multiLineComment"),
            )(),
          ),
        ),
      ),
    )
  }

  Generator.generate(
    lexerData,
    grammarData,
    Generator.testSettings("multiLineComment"),
  )

}
