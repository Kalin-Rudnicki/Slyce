package slyce.tests.argList

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter

import scalaz.-\/
import scalaz.Scalaz.ToOptionIdOps
import scalaz.NonEmptyList
import scalaz.\/
import scalaz.\/-

import slyce.generate.{formatting => fmt}
import slyce.generate.{lexer => lex}
import slyce.generate.{grammar => gram}

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
              regex = Inclusive('(', ')', ','),
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
            ),
          ),
        ),
      ),
    )
  }

  val lines = for {
    dfa <- lex.Lexer(lexerData)
    simpleData <- gram.DataToSimpleData(grammarData)
    stateMachine <- gram.SimpleDataToStateMachine(simpleData)
    // Format
    tokLines <- fmt.TokenLines((dfa, simpleData))
    stateLines <- fmt.DfaLines(dfa)
    ntLines <- fmt.NtLines(simpleData)
    parseStateLines <- fmt.ParseStateLines(stateMachine)
  } yield (
    tokLines,
    stateLines,
    ntLines,
    parseStateLines,
  )

  lines match {
    case -\/(errs) =>
      println("Errors:")
      errs.foreach(println)
      System.exit(1)
    case \/-((tokLines, stateLines, ntLines, parseStateLines)) =>
      import slyce.common.helpers.Idt._

      val pkg = List("slyce", "tests", "argList")
      val dir = "SlyceTest/src/test/scala"
      val name = "ExOutput"
      val file = new File(s"$dir/${pkg.mkString("/")}/$name.scala")

      println("Success:")
      val writer = new BufferedWriter(new FileWriter(file))
      val src =
        Group(
          Group(
            s"package ${pkg.mkString(".")}",
            Break,
            "import scala.io.Source",
            Break,
            "import scalaz.-\\/",
            "import scalaz.\\/-",
            "import scalaz.Scalaz.ToEitherOps",
            Break,
            "import slyce.parse._",
            "import slyce.common.helpers._",
          ),
          Break,
          Group(
            s"object $name extends App {",
            Indented(
              Break,
              tokLines,
              Break,
              ntLines,
              Break,
              stateLines,
              Break,
              parseStateLines,
              Break,
              Group(
                "{",
                Indented(
                  "val source = Source.fromFile(\"res-test/argList/samples/ex1.txt\")",
                  "val str = source.mkString",
                  "source.close",
                  Break,
                  Group(
                    "val res = for {",
                    Indented(
                      "toks <- dfa(str)",
                      "rawTree <- stateMachine(toks)",
                    ),
                    "} yield rawTree",
                  ),
                  Break,
                  "res match {",
                  Indented(
                    "case -\\/(err) =>",
                    Indented(
                      "println(\"Error:\")",
                      "println()",
                      "err.foreach(println)",
                    ),
                    "case \\/-(rawTree) =>",
                    Indented(
                      "println(\"Success:\")",
                      "println()",
                      "println(rawTree)",
                    ),
                  ),
                  "}",
                ),
                "}",
              ),
            ),
            Break,
            "}",
          ),
        ).build("  ")
      println(src)
      writer.write(src)
      writer.close
  }

}
