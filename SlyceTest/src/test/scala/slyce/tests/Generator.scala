package slyce.tests

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter

import scalaz.-\/
import scalaz.\/-

import slyce.generate.Formatter
import slyce.generate.{lexer => lex}
import slyce.generate.{grammar => gram}

object Generator {

  private val srcRoot = "SlyceTest/src/test/scala"

  def generate(
      lexerData: lex.Data,
      grammarData: gram.Data,
      subPkg: String,
  ): Unit = {
    val formatter = for {
      dfa <- lex.Lexer(lexerData)
      simpleData <- gram.DataToSimpleData(grammarData)
      stateMachine <- gram.SimpleDataToStateMachine(simpleData)
      // Format
      fmtr <- Formatter(dfa, simpleData, stateMachine)
    } yield fmtr

    formatter match {
      case -\/(errs) =>
        println("Errors:")
        errs.foreach(println)
        System.exit(1)
      case \/-(formatter) =>
        import slyce.generate.architecture.Formatter.Settings

        val settings = Settings(
          packageName = List("slyce", "tests", subPkg),
          className = "Parser",
          indent = "  ",
        )

        val file = new File(s"$srcRoot/${settings.packageName.mkString("/")}/${settings.className}.scala")

        println("Success:")
        val writer = new BufferedWriter(new FileWriter(file))
        val src = formatter(settings)
        println(src)
        writer.write(src)
        writer.close
    }

  }

}
