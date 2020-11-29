package slyce

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
    import klib.ColorString.syntax._
    import auto._
    import klib.Idt._
    import klib.Logger.GlobalLogger
    import klib.Logger.{LogLevel => LL}

    GlobalLogger.sources.stdOut(LL.Debug)
    GlobalLogger.flags.add("Generator")
    GlobalLogger.flags.add("DataToNfa")
    GlobalLogger.flags.add("NfaToDfa")

    implicit val flags: Set[String] = Set("Generator")

    val formatter = for {
      dfa <- lex.Lexer(lexerData)
      simpleData <- gram.DataToSimpleData(grammarData)
      stateMachine <- gram.SimpleDataToStateMachine(simpleData)
      // Format
      fmtr <- Formatter(dfa, simpleData, stateMachine)
    } yield fmtr

    GlobalLogger.break
    GlobalLogger.debug("=====| Generator |=====")
    formatter match {
      case -\/(errs) =>
        GlobalLogger.fatal("Failure")
        errs.foreach(GlobalLogger.error(_))
        System.exit(1)
      case \/-(formatter) =>
        import slyce.generate.architecture.Formatter.Settings

        val settings = Settings(
          packageName = List("slyce", "tests", subPkg),
          className = "Parser",
          indent = "  ",
        )

        val file = new File(s"$srcRoot/${settings.packageName.mkString("/")}/${settings.className}.scala")

        GlobalLogger.important("Success!")
        val writer = new BufferedWriter(new FileWriter(file))
        val src = formatter(settings)
        GlobalLogger.debug(src, Set("Generator:src"))
        writer.write(src)
        writer.close
    }

  }

}
