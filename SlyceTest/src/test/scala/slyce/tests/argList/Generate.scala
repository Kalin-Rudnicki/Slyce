package slyce.tests.argList

import scalaz.Scalaz.ToOptionIdOps

import slyce.generate.{lexer => lex}
import slyce.generate.{grammar => gram}
import slyce.tests.Generator

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
              regex = Inclusive('(', ')', ',', '!'),
              yields = Yields(
                yields = List(Yields.Yield.Text.std),
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 3,
              regex = Regex.Sequence(
                Inclusive('_') | Inclusive.az,
                Regex.Repeat * (
                  Inclusive('_') |
                    Inclusive.AZ |
                    Inclusive.az |
                    Inclusive.d
                ),
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("_var")),
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
      startNT = "List",
      nts = List(
        // List
        NT(
          name = "List",
          nt = StandardNT.^(
            IgnoredList(
              Id.Raw("("),
            )(
              ListNT.*(
                before = IgnoredList()(
                  Id.Terminal("_var"),
                )(),
                after = IgnoredList(
                  Id.Raw(","),
                )(
                  Id.Terminal("_var"),
                )().some,
              ),
            )(
              Id.Raw(")"),
              Opt(Id.Raw("!")),
            ),
          ),
        ),
      ),
    )
  }

  Generator.generate(
    lexerData,
    grammarData,
    "argList",
  )

}
